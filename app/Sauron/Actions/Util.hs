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

  , openBrowserToUrl

  , findRepoParent
  , findJobParent
) where

import Brick.BChan
import Control.Concurrent.QSem
import Control.Exception.Safe (bracket_)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import Control.Monad.Logger (LogLevel(..))
import Control.Monad.Reader
import Data.Aeson (FromJSON)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Builder (intDec, toLazyByteString)
import qualified Data.List as L
import qualified Data.Text as T
import Data.Time
import GitHub
import Network.HTTP.Client (Response, responseBody, responseHeaders)
import Network.HTTP.Types (EscapeItem(..))
import Network.HTTP.Types.Header (hContentLength)
import Relude
import Sauron.Types
import UnliftIO.Process

#ifdef mingw32_HOST_OS
import UnliftIO.Directory


openBrowserToUrl :: MonadIO m => String -> m ()
openBrowserToUrl url = do
  findExecutable "explorer.exe" >>= \case
    Just p -> void $ readCreateProcessWithExitCode (proc p [url]) ""
    Nothing -> return ()
#elif darwin_HOST_OS
openBrowserToUrl :: MonadIO m => String -> m ()
openBrowserToUrl url =
  void $ readCreateProcessWithExitCode (proc "open" [url]) ""
#else
openBrowserToUrl :: MonadIO m => String -> m ()
openBrowserToUrl url =
  void $ readCreateProcessWithExitCode (proc "xdg-open" [url]) ""
#endif

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

requestToUrl :: Request k a -> Text
requestToUrl req = case req of
  Query paths queryString -> pathsToUrl paths <> formatQueryString queryString
  PagedQuery paths queryString fetchCount -> pathsToUrl paths <> formatQueryString (queryString <> extraQueryItems fetchCount)
  Command _method paths _body -> pathsToUrl paths
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

logResult :: (HasCallStack, MonadIO m) => BChan AppEvent -> Request k a -> Either Error (Response b) -> Maybe NominalDiffTime -> m ()
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
