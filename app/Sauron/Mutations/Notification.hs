{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Sauron.Mutations.Notification (
  markNotificationAsRead
  , markNotificationAsDone
  ) where

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import GitHub
import Relude
import Sauron.Actions.Util
import Sauron.Logging
import Sauron.Types

-- | Returns 'True' on success so the caller can react (e.g. show a toast).
markNotificationAsRead :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Notification -> m Bool
markNotificationAsRead notification =
  withGithubApiSemaphore (githubWithLoggingUnit (markNotificationAsReadR (notificationId notification))) >>= \case
    Left err -> logError ("Failed to mark notification as read: " <> show err) >> return False
    Right _ -> return True

-- | Returns 'True' on success so the caller can react (e.g. show a toast).
markNotificationAsDone :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Notification -> m Bool
markNotificationAsDone notification =
  withGithubApiSemaphore (githubWithLoggingUnit (markNotificationAsDoneR (notificationId notification))) >>= \case
    Left err -> logError ("Failed to mark notification as done: " <> show err) >> return False
    Right _ -> return True
