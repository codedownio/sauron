{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Sauron.Actions.Util (
  withGithubApiSemaphore
  , withGithubApiSemaphore'

  , executeRequestWithLogging
  , executeRequestWithLoggingDirect
  , githubWithLogging
  , githubWithLoggingDirect

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
import qualified Data.Text as T
import Data.Time
import GitHub
import Network.HTTP.Client (Response)
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
    pathsToUrl = T.intercalate "/"

executeRequestWithLogging :: (MonadReader BaseContext m, MonadIO m, FromJSON a) => Request k a -> m (Either Error (Response a))
executeRequestWithLogging request = do
  BaseContext {auth, manager, eventChan} <- ask
  result <- liftIO $ executeRequestWithMgrAndRes manager auth request
  logResult eventChan request result
  return result

githubWithLogging :: (MonadReader BaseContext m, MonadIO m, FromJSON a) => Request k a -> m (Either Error a)
githubWithLogging request = do
  bc <- ask
  githubWithLoggingDirect bc request

executeRequestWithLoggingDirect :: (MonadIO m, FromJSON a) => BaseContext -> Request k a -> m (Either Error (Response a))
executeRequestWithLoggingDirect (BaseContext {..}) request = do
  result <- liftIO $ executeRequestWithMgrAndRes manager auth request
  logResult eventChan request result
  return result

githubWithLoggingDirect :: (MonadIO m, FromJSON a) => BaseContext -> Request k a -> m (Either Error a)
githubWithLoggingDirect (BaseContext {auth, eventChan}) request = do
  result <- liftIO $ github auth request
  logResult eventChan request result
  return result

logResult :: (MonadIO m) => BChan AppEvent -> Request k a -> Either Error b -> m ()
logResult eventChan request result = do
  now <- liftIO getCurrentTime
  let url = requestToUrl request
  let (level, msg) = case result of
        Left err -> (LevelError, "Failed: " <> url <> " - " <> show err)
        Right _ -> (LevelInfo, url)
  let logEntry = LogEntry now level msg
  liftIO $ writeBChan eventChan (LogEntryAdded logEntry)
