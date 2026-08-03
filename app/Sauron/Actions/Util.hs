{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Sauron.Actions.Util (
  withGithubApiSemaphore
  , withGithubApiSemaphore'

  , githubWithLogging
  , githubWithLoggingResponse
  , githubWithLogging'
  , githubWithLogging''
  , githubWithLoggingUnit

  , openBrowserToUrl
  , copyToClipboard

  , findRepoParent
  , findJobParent
  , findNotificationsParent
  , findIssuesParent
  , findPullsParent
  , findWorkflowsParent
  , findWorkflowParent

  , withRepoDefaultBranch
  , withRepoDefaultBranch'
) where

import Brick.BChan
import Control.Concurrent.QSem
import Control.Exception.Safe (bracket_, handleAny, try)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson (FromJSON)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import Data.ByteString.Builder (intDec, toLazyByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as L
import Data.String.Interpolate
import qualified Data.Text as T
import Data.Time
import GitHub
import Network.HTTP.Client (Response, responseBody, responseHeaders)
import Network.HTTP.Types (EscapeItem(..))
import Network.HTTP.Types.Header (hContentLength)
import Relude
import System.Exit (ExitCode(..))
import qualified System.IO as SIO
import Sauron.Logging
import Sauron.Types
import UnliftIO.Process

#ifdef mingw32_HOST_OS
import UnliftIO.Directory


openBrowserToUrl :: MonadIO m => String -> m ()
openBrowserToUrl url = do
  findExecutable "explorer.exe" >>= \case
    Just p -> void $ readCreateProcessWithExitCode (proc p [url]) ""
    Nothing -> return ()

-- | Copy text to the clipboard, falling back to an OSC 52 escape sequence (option 2) if the
-- native clipboard tool is missing or errors. Returns True if some method reported success.
copyToClipboard :: MonadIO m => String -> m Bool
copyToClipboard text = liftIO $ do
  r <- try (readCreateProcessWithExitCode (proc "clip" []) text)
  case r of
    Right (ExitSuccess, _, _) -> return True
    (_ :: Either SomeException (ExitCode, String, String)) -> copyViaOsc52 SIO.stdout text
#elif darwin_HOST_OS
openBrowserToUrl :: MonadIO m => String -> m ()
openBrowserToUrl url =
  void $ readCreateProcessWithExitCode (proc "open" [url]) ""

-- | See the Linux definition below; on macOS the native tool is pbcopy, with the same OSC 52
-- fallback.
copyToClipboard :: MonadIO m => String -> m Bool
copyToClipboard text = liftIO $ do
  r <- try (readCreateProcessWithExitCode (proc "pbcopy" []) text)
  case r of
    Right (ExitSuccess, _, _) -> return True
    (_ :: Either SomeException (ExitCode, String, String)) -> copyViaTtyOsc52 text
#else
openBrowserToUrl :: MonadIO m => String -> m ()
openBrowserToUrl url =
  void $ readCreateProcessWithExitCode (proc "xdg-open" [url]) ""

-- | Copy text to the clipboard. First tries the native clipboard tools (option 1): Wayland's
-- wl-copy, then X11's xclip and xsel. If none exist or they all error, falls back to an OSC 52
-- escape sequence (option 2), which asks the terminal itself to copy and so works over SSH with
-- no external tool. Returns True once some method reports success.
--
-- The native tools fork a background process to hold the selection, which inherits the child's
-- stdout/stderr; reading those to EOF (as readCreateProcessWithExitCode does) would hang forever.
-- So we point the tool's output at /dev/null and wait only on the parent, which exits as soon as
-- it has forked.
copyToClipboard :: MonadIO m => String -> m Bool
copyToClipboard text = liftIO $ go [("wl-copy", []), ("xclip", ["-selection", "clipboard"]), ("xsel", ["--clipboard", "--input"])]
  where
    go :: [(String, [String])] -> IO Bool
    go [] = copyViaTtyOsc52 text
    go ((cmd, cmdArgs):rest) =
      try (runClipboardTool cmd cmdArgs) >>= \case
        Right ExitSuccess -> return True
        (_ :: Either SomeException ExitCode) -> go rest

    runClipboardTool :: String -> [String] -> IO ExitCode
    runClipboardTool cmd cmdArgs =
      SIO.withFile "/dev/null" SIO.WriteMode $ \devNull ->
        withCreateProcess (proc cmd cmdArgs) { std_in = CreatePipe, std_out = UseHandle devNull, std_err = UseHandle devNull } $
          \mstdin _ _ ph -> do
            whenJust mstdin $ \h -> SIO.hPutStr h text >> SIO.hClose h
            waitForProcess ph
#endif

-- | Copy via an OSC 52 escape sequence written to the controlling terminal (/dev/tty). Opening
-- /dev/tty rather than stdout means it works even if stdout is redirected, and keeps the sequence
-- out of vty's own output buffer. We can't tell whether the terminal honored it (OSC 52 is
-- fire-and-forget, and terminals may disable it), so this reports success as long as the sequence
-- was written.
copyViaTtyOsc52 :: String -> IO Bool
copyViaTtyOsc52 text =
  handleAny (\_ -> return False) $
    SIO.withFile "/dev/tty" SIO.WriteMode $ \h -> copyViaOsc52 h text

-- | Write an OSC 52 "set clipboard" escape sequence (ESC ] 52 ; c ; <base64> BEL) to the given
-- handle. Returns True if the write succeeded.
copyViaOsc52 :: SIO.Handle -> String -> IO Bool
copyViaOsc52 h text =
  handleAny (\_ -> return False) $ do
    BS.hPutStr h (BC.pack "\ESC]52;c;" <> B64.encode (encodeUtf8 text) <> BC.pack "\a")
    SIO.hFlush h
    return True

withGithubApiSemaphore :: (HasCallStack, MonadReader BaseContext m, MonadIO m, MonadMask m) => (HasCallStack => m a) -> m a
withGithubApiSemaphore action = do
  sem <- asks requestSemaphore
  withGithubApiSemaphore' sem action

withGithubApiSemaphore' :: (HasCallStack, MonadIO m, MonadMask m) => QSem -> (HasCallStack => m a) -> m a
withGithubApiSemaphore' sem = bracket_ (liftIO $ waitQSem sem) (liftIO $ signalQSem sem)

findRepoParent :: NonEmpty (SomeNode Variable) -> Maybe (Node Variable RepoT)
findRepoParent elems = viaNonEmpty head [x | SomeNode x@(RepoNode _) <- toList elems]

findJobParent :: [SomeNode Variable] -> Maybe (Node Variable SingleJobT)
findJobParent elems = viaNonEmpty head [x | SomeNode x@(SingleJobNode _) <- toList elems]

findNotificationsParent :: NonEmpty (SomeNode Variable) -> Maybe (Node Variable PaginatedNotificationsT)
findNotificationsParent elems = viaNonEmpty head [x | SomeNode x@(PaginatedNotificationsNode _) <- toList elems]

findIssuesParent :: NonEmpty (SomeNode Variable) -> Maybe (Node Variable PaginatedIssuesT)
findIssuesParent elems = viaNonEmpty head [x | SomeNode x@(PaginatedIssuesNode _) <- toList elems]

findPullsParent :: NonEmpty (SomeNode Variable) -> Maybe (Node Variable PaginatedPullsT)
findPullsParent elems = viaNonEmpty head [x | SomeNode x@(PaginatedPullsNode _) <- toList elems]

findWorkflowsParent :: NonEmpty (SomeNode Variable) -> Maybe (Node Variable PaginatedWorkflowsT)
findWorkflowsParent elems = viaNonEmpty head [x | SomeNode x@(PaginatedWorkflowsNode _) <- toList elems]

findWorkflowParent :: NonEmpty (SomeNode Variable) -> Maybe (Node Variable SingleWorkflowT)
findWorkflowParent elems = viaNonEmpty head [x | SomeNode x@(SingleWorkflowNode _) <- toList elems]

withRepoDefaultBranch :: MonadIO m => TVar (Fetchable Repo) -> (Maybe Text -> m ()) -> m ()
withRepoDefaultBranch = withRepoDefaultBranch' (return ())

withRepoDefaultBranch' :: MonadIO m => m a -> TVar (Fetchable Repo) -> (Maybe Text -> m a) -> m a
withRepoDefaultBranch' defaultValue fetchableVar action = readTVarIO fetchableVar >>= \case
  Fetched (Repo {..}) -> action repoDefaultBranch
  Fetching (Just (Repo {..})) -> action repoDefaultBranch
  _ -> defaultValue

requestToUrl :: GenRequest mt k a -> Text
requestToUrl req = case req of
  Query paths queryString -> pathsToUrl paths <> formatQueryString queryString
  PagedQuery paths queryString fetchCount -> pathsToUrl paths <> formatQueryString (queryString <> extraQueryItems fetchCount)
  Command method paths _body -> show method <> " " <> pathsToUrl paths
  where
    pathsToUrl :: [Text] -> Text
    pathsToUrl = ("/" <>) . T.intercalate "/"

    formatQueryString :: QueryString -> Text
    formatQueryString queryParams =
      if null queryParams
        then ""
        else "?" <> T.intercalate "&" (map formatParam queryParams)

    formatParam :: (BS.ByteString, [EscapeItem]) -> Text
    formatParam (key, values) = keyText <> "=" <> valuesText
      where
        keyText = decodeUtf8 key
        valuesText = T.intercalate "," $ map formatEscapeItem values

    formatEscapeItem :: EscapeItem -> Text
    formatEscapeItem (QE s) = decodeUtf8 s -- QE is already query-escaped
    formatEscapeItem (QN s) = decodeUtf8 s

    extraQueryItems :: FetchCount -> [(BS.ByteString, [EscapeItem])]
    extraQueryItems (FetchPage pp) = catMaybes [
        (\page -> ("page", [QE (LBS.toStrict $ toLazyByteString $ intDec page)])) <$> pageParamsPage pp
        , (\perPage -> ("per_page", [QE (LBS.toStrict $ toLazyByteString $ intDec perPage)])) <$> pageParamsPerPage pp
        ]
    extraQueryItems _ = []

githubWithLogging :: (HasCallStack, MonadReader BaseContext m, MonadIO m, FromJSON a) => Request k a -> m (Either Error a)
githubWithLogging request = withFrozenCallStack (fmap responseBody <$> githubWithLoggingResponse request)

githubWithLoggingResponse :: (HasCallStack, MonadReader BaseContext m, MonadIO m, FromJSON a) => Request k a -> m (Either Error (Response a))
githubWithLoggingResponse request = withFrozenCallStack (ask >>= flip githubWithLogging'' request)

githubWithLogging' :: (HasCallStack, MonadIO m, FromJSON a) => BaseContext -> Request k a -> m (Either Error a)
githubWithLogging' bc request = withFrozenCallStack (fmap responseBody <$> githubWithLogging'' bc request)

githubWithLogging'' :: (HasCallStack, MonadIO m, FromJSON a) => BaseContext -> Request k a -> m (Either Error (Response a))
githubWithLogging'' (BaseContext {..}) request = withFrozenCallStack $ do
  startTime <- liftIO getCurrentTime
  result <- liftIO $ executeRequestWithMgrAndRes manager auth request
  endTime <- liftIO getCurrentTime
  let duration = diffUTCTime endTime startTime
  logResult eventChan request result (Just duration)
  return result

githubWithLoggingUnit :: (HasCallStack, MonadReader BaseContext m, MonadIO m) => GenRequest 'MtUnit rw () -> m (Either Error ())
githubWithLoggingUnit request = withFrozenCallStack $ do
  BaseContext {..} <- ask
  startTime <- liftIO getCurrentTime
  result <- liftIO $ executeRequestWithMgrAndRes manager auth request
  endTime <- liftIO getCurrentTime
  let duration = diffUTCTime endTime startTime
  logResult eventChan request result (Just duration)
  info [i|result: #{result}|]
  return (fmap responseBody result)

logResult :: (HasCallStack, MonadIO m) => BChan AppEvent -> GenRequest mt k a -> Either Error (Response b) -> Maybe NominalDiffTime -> m ()
logResult eventChan request result maybeDuration = do
  now <- liftIO getCurrentTime
  let url = requestToUrl request
  let level = case result of Left _ -> LevelError; _ -> LevelInfo
  let msg = case result of
        Left err -> "Failed: " <> url <> " - " <> show err
        Right response ->
          let sizeInfo = case getResponseSize response of
                Nothing -> "" -- " " <> show (responseHeaders response)
                Just size -> " (" <> show size <> " bytes)"
          in (url <> sizeInfo)
  let logEntry = LogEntry now level msg maybeDuration (Just callStack)
  liftIO $ writeBChan eventChan (LogEntryAdded logEntry)
  where
    getResponseSize :: Response a -> Maybe Int
    getResponseSize response = do
      contentLengthHeader <- L.lookup hContentLength (responseHeaders response)
      let contentLengthText :: Text = decodeUtf8 contentLengthHeader
      readMaybe (toString contentLengthText)
