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
  fetchPaginated'' (getNotificationsR optionsAll) _pageInfo _state $ \case
    Left err -> do
      writeTVar _state (Errored (show err))
      writeTVar _children []
    Right (notifications, newPageInfo) -> do
      writeTVar _pageInfo newPageInfo
      writeTVar _state (Fetched notifications)
      (writeTVar _children =<<) $ forM (V.toList notifications) $ \notification ->
        SingleNotificationNode <$> makeEmptyElem bc notification "" (_depth + 1)
