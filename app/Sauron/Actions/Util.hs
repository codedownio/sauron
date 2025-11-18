{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Sauron.Actions.Util (
  withGithubApiSemaphore
  , withGithubApiSemaphore'

  , githubWithLogging
  , githubWithLoggingResponse
  , githubWithLogging'
  , githubWithLogging''

  , openBrowserToUrl

  , findRepoParent
) where

import Brick.BChan
import Control.Concurrent.QSem
import Control.Exception.Safe (bracket_)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import Control.Monad.Logger (LogLevel(..))
import Control.Monad.Reader
import Data.Aeson (FromJSON)
import qualified Data.List as L
import qualified Data.Text as T
import Data.Time
import GitHub
import Network.HTTP.Client (Response, responseBody, responseHeaders)
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

withGithubApiSemaphore :: (MonadReader BaseContext m, MonadIO m, MonadMask m) => m a -> m a
withGithubApiSemaphore action = do
  sem <- asks requestSemaphore
  withGithubApiSemaphore' sem action

withGithubApiSemaphore' :: (MonadIO m, MonadMask m) => QSem -> m a -> m a
withGithubApiSemaphore' sem = bracket_ (liftIO $ waitQSem sem) (liftIO $ signalQSem sem)

findRepoParent :: NonEmpty (SomeNode Variable) -> Maybe (Node Variable RepoT)
findRepoParent elems = viaNonEmpty head [x | SomeNode x@(RepoNode _) <- toList elems]

requestToUrl :: Request k a -> Text
requestToUrl req = case req of
  Query paths _queryString -> pathsToUrl paths
  PagedQuery paths _queryString _fetchCount -> pathsToUrl paths
  Command _method paths _body -> pathsToUrl paths
  where
    pathsToUrl :: [Text] -> Text
    pathsToUrl = ("/" <>) . T.intercalate "/"

githubWithLogging :: (MonadReader BaseContext m, MonadIO m, FromJSON a) => Request k a -> m (Either Error a)
githubWithLogging request = fmap responseBody <$> githubWithLoggingResponse request

githubWithLoggingResponse :: (MonadReader BaseContext m, MonadIO m, FromJSON a) => Request k a -> m (Either Error (Response a))
githubWithLoggingResponse request = ask >>= flip githubWithLogging'' request

githubWithLogging' :: (MonadIO m, FromJSON a) => BaseContext -> Request k a -> m (Either Error a)
githubWithLogging' bc request = fmap responseBody <$> githubWithLogging'' bc request

githubWithLogging'' :: (MonadIO m, FromJSON a) => BaseContext -> Request k a -> m (Either Error (Response a))
githubWithLogging'' (BaseContext {..}) request = do
  result <- liftIO $ executeRequestWithMgrAndRes manager auth request
  logResult eventChan request result
  return result

logResult :: (MonadIO m) => BChan AppEvent -> Request k a -> Either Error (Response b) -> m ()
logResult eventChan request result = do
  now <- liftIO getCurrentTime
  let url = requestToUrl request
  let (level, msg) = case result of
        Left err -> (LevelError, "Failed: " <> url <> " - " <> show err)
        Right response ->
          let sizeInfo = case getResponseSize response of
                Nothing -> ""
                Just size -> " (" <> show size <> " bytes)"
          in (LevelInfo, url <> sizeInfo)
  let logEntry = LogEntry now level msg
  liftIO $ writeBChan eventChan (LogEntryAdded logEntry)
  where
    getResponseSize :: Response a -> Maybe Int
    getResponseSize response = do
      contentLengthHeader <- L.lookup hContentLength (responseHeaders response)
      let contentLengthText :: Text = decodeUtf8 contentLengthHeader
      readMaybe (toString contentLengthText)
