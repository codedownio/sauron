module Sauron.Actions.Util (
  withGithubApiSemaphore
  , withGithubApiSemaphore'
) where

import Control.Concurrent.QSem
import Control.Exception.Safe (bracket_)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Relude
import Sauron.Types

withGithubApiSemaphore :: (MonadReader BaseContext m, MonadIO m, MonadMask m) => m a -> m a
withGithubApiSemaphore action = do
  sem <- asks requestSemaphore
  withGithubApiSemaphore' sem action

withGithubApiSemaphore' :: (MonadIO m, MonadMask m) => QSem -> m a -> m a
withGithubApiSemaphore' sem = bracket_ (liftIO $ waitQSem sem) (liftIO $ signalQSem sem)