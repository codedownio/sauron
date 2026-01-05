{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Sauron.Fetch.Notification (
  fetchNotifications
  ) where

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import qualified Data.Vector as V
import GitHub
import Relude
import Sauron.Fetch.Core
import Sauron.Types

fetchNotifications :: (
  HasCallStack, MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Node Variable PaginatedNotificationsT -> m ()
fetchNotifications (PaginatedNotificationsNode (EntityData {..})) = do
  bc <- ask
  fetchPaginatedWithState (getNotificationsR optionsAll) _state $ \case
    Left err -> do
      (s, p, _) <- readTVar _state
      writeTVar _state (s, p, Errored err)
      writeTVar _children []
    Right (notifications, newPageInfo) -> do
      (s, _, _) <- readTVar _state
      writeTVar _state (s, newPageInfo, Fetched (V.length notifications))
      (writeTVar _children =<<) $ forM (V.toList notifications) $ \notification ->
        SingleNotificationNode <$> makeEmptyElemWithState bc notification (Fetched ()) "" (_depth + 1)
