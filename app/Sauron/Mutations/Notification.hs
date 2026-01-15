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
import Sauron.Actions.Util (withGithubApiSemaphore)
import Sauron.Logging
import Sauron.Types

markNotificationAsRead :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Notification -> m ()
markNotificationAsRead notification = do
  bc@(BaseContext {auth, manager}) <- ask

  withGithubApiSemaphore (liftIO $ executeRequestWithMgrAndRes manager auth (markNotificationAsReadR (notificationId notification))) >>= \case
    Left err -> logError' bc $ "Failed to mark notification as read: " <> show err
    Right _ -> return ()

markNotificationAsDone :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Notification -> m ()
markNotificationAsDone notification = do
  markNotificationAsRead notification
