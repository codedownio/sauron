{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Sauron.Fetch.Notification (
  fetchNotifications
  , fetchNotificationContent
  ) where

import Control.Exception.Safe (bracketOnError_)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import Data.Char (isDigit)
import qualified Data.Text as T
import qualified Data.Vector as V
import GitHub
import Network.URI (parseURI, uriPath)
import Relude
import Sauron.Actions.Util (withGithubApiSemaphore, githubWithLogging)
import Sauron.Fetch.Core
import Sauron.Fetch.Issue (fetchIssueCommentsAndEvents)
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
        SingleNotificationNode <$> makeEmptyElemWithState bc notification (NotificationState NotFetched True) "" (_depth + 1)

fetchNotificationContent :: (
  HasCallStack, MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Notification -> TVar NotificationState -> m ()
fetchNotificationContent notification stateVar = do
  let Subject {..} = notificationSubject notification
  let RepoRef {..} = notificationRepo notification
  let SimpleOwner {..} = repoRefOwner
  let owner = mkName (Proxy :: Proxy Owner) (untagName simpleOwnerLogin)
  let repo = repoRefRepo
  case (subjectType, subjectURL) of
    ("Issue", Just subUrl) -> fetchAsIssueOrPull NotificationIssue owner repo subUrl stateVar
    ("PullRequest", Just subUrl) -> fetchAsIssueOrPull NotificationPull owner repo subUrl stateVar
    _ -> atomically $ modifyTVar' stateVar $ \ns -> ns { notificationStateContent = Fetched NotificationOther }

fetchAsIssueOrPull :: (
  HasCallStack, MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => (Issue -> V.Vector TimelineEvent -> NotificationContent)
    -> Name Owner -> Name Repo -> URL -> TVar NotificationState -> m ()
fetchAsIssueOrPull wrap owner repo subUrl stateVar = do
  ctx <- ask
  case extractNumberFromApiUrl (toString $ getUrl subUrl) of
    Nothing -> atomically $ modifyTVar' stateVar $ \ns -> ns { notificationStateContent = Errored "Could not parse issue/PR number from URL" }
    Just num ->
      bracketOnError_ (atomically $ modifyTVar' stateVar $ \ns -> ns { notificationStateContent = case notificationStateContent ns of
                          Fetched x -> Fetching (Just x)
                          Fetching x -> Fetching x
                          _ -> Fetching Nothing })
                      (atomically $ modifyTVar' stateVar $ \ns -> ns { notificationStateContent = Errored "Fetch failed with exception." }) $ do
        withGithubApiSemaphore (githubWithLogging (issueR owner repo (IssueNumber num))) >>= \case
          Left err -> atomically $ modifyTVar' stateVar $ \ns -> ns { notificationStateContent = Errored (show err) }
          Right issue ->
            liftIO (fetchIssueCommentsAndEvents ctx owner repo num) >>= \case
              Left err -> atomically $ modifyTVar' stateVar $ \ns -> ns { notificationStateContent = Errored (show err) }
              Right merged -> atomically $ modifyTVar' stateVar $ \ns -> ns { notificationStateContent = Fetched (wrap issue merged) }

  where
    extractNumberFromApiUrl :: String -> Maybe Int
    extractNumberFromApiUrl url = do
      uri <- parseURI url
      let segments = filter (not . T.null) $ T.splitOn "/" $ toText (uriPath uri)
      case reverse (toList segments) of
        (idStr:_) | T.all isDigit idStr -> readMaybe (toString idStr)
        _ -> Nothing
