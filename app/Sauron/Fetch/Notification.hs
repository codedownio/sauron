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
import Sauron.Fetch.NotificationStates (fetchNotificationSubjectStates, notificationToQueryItem)
import Sauron.Types
import UnliftIO.Async (async)

fetchNotifications :: (
  HasCallStack, MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Node Variable PaginatedNotificationsT -> m ()
fetchNotifications (PaginatedNotificationsNode (EntityData {..})) = do
  bc <- ask

  (_, PageInfo {pageInfoCurrentPage=requestedPage}, _) <- readTVarIO _state

  -- The notifications endpoint has no total-count field, so we fetch the whole list (FetchAll
  -- follows every page) to get an accurate count. We keep every notification in _children (so the
  -- heading's unread-dot reflects all pages, not just the visible one) and let Sauron.Expanding
  -- slice out the current page at display time; here we just derive the page info from the total.
  bracketOnError_ (atomically $ modifyTVar' _state $ \(s, p, f) -> (s, p, Fetching (fetchableCurrent f)))
                  (atomically $ modifyTVar' _state $ \(s, p, _) -> (s, p, Errored "Fetch failed with exception.")) $
    withGithubApiSemaphore (githubWithLogging (getNotificationsR optionsAll FetchAll)) >>= \case
      Left err -> atomically $ do
        (s, p, _) <- readTVar _state
        writeTVar _state (s, p, Errored (show err))
        writeTVar _children []
      Right notifications -> do
        let count = V.length notifications
        let totalPages = max 1 ((count + notificationPageSize - 1) `div` notificationPageSize)
        let currentPage = min (max 1 requestedPage) totalPages
        let pageInfo = PageInfo {
              pageInfoCurrentPage = currentPage
              , pageInfoFirstPage = if currentPage > 1 then Just 1 else Nothing
              , pageInfoPrevPage = if currentPage > 1 then Just (currentPage - 1) else Nothing
              , pageInfoNextPage = if currentPage < totalPages then Just (currentPage + 1) else Nothing
              , pageInfoLastPage = if currentPage < totalPages then Just totalPages else Nothing
              }
        children' <- atomically $ forM (V.toList notifications) $ \notification ->
          SingleNotificationNode <$> makeEmptyElemWithState bc notification (NotificationState NotFetched Nothing True) "" (_depth + 1)
        atomically $ do
          (s, _, _) <- readTVar _state
          writeTVar _state (s, pageInfo, Fetched count)
          writeTVar _children children'

  -- Batch-fetch subject states (open/closed/merged/draft) via a single GraphQL query
  notifChildren <- readTVarIO _children
  void $ liftIO $ async $ do
    let queryItems = catMaybes
          [ notificationToQueryItem ("n" <> show idx) stateVar notification
          | (idx, SingleNotificationNode (EntityData {_static=notification, _state=stateVar})) <- zip [(0 :: Int)..] notifChildren
          ]
    unless (null queryItems) $
      runReaderT (fetchNotificationSubjectStates queryItems) bc

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
    (typ, Just subUrl) | typ == subjectTypeRelease -> fetchAsRelease owner repo subUrl stateVar
    _ -> atomically $ modifyTVar' stateVar $ \ns -> ns { notificationStateContent = Fetched NotificationOther }

fetchAsRelease :: (
  HasCallStack, MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> URL -> TVar NotificationState -> m ()
fetchAsRelease owner repo subUrl stateVar =
  case extractNumberFromApiUrl (toString $ getUrl subUrl) of
    Nothing -> atomically $ modifyTVar' stateVar $ \ns -> ns { notificationStateContent = Errored "Could not parse release ID from URL" }
    Just num ->
      bracketOnError_ (atomically $ modifyTVar' stateVar $ \ns -> ns { notificationStateContent = case notificationStateContent ns of
                          Fetched x -> Fetching (Just x)
                          Fetching x -> Fetching x
                          _ -> Fetching Nothing })
                      (atomically $ modifyTVar' stateVar $ \ns -> ns { notificationStateContent = Errored "Fetch failed with exception." }) $
        withGithubApiSemaphore (githubWithLogging (releaseR owner repo (mkId (Proxy :: Proxy Release) num))) >>= \case
          Left err -> atomically $ modifyTVar' stateVar $ \ns -> ns { notificationStateContent = Errored (show err) }
          Right release -> atomically $ modifyTVar' stateVar $ \ns -> ns { notificationStateContent = Fetched (NotificationRelease release) }

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
                      (atomically $ modifyTVar' stateVar $ \ns -> ns { notificationStateContent = Errored "Fetch failed with exception." }) $
        withGithubApiSemaphore (githubWithLogging (issueR owner repo (IssueNumber num))) >>= \case
          Left err -> atomically $ modifyTVar' stateVar $ \ns -> ns { notificationStateContent = Errored (show err) }
          Right issue ->
            liftIO (fetchIssueCommentsAndEvents ctx owner repo num) >>= \case
              Left err -> atomically $ modifyTVar' stateVar $ \ns -> ns { notificationStateContent = Errored (show err) }
              Right merged -> atomically $ modifyTVar' stateVar $ \ns -> ns { notificationStateContent = Fetched (wrap issue merged) }

extractNumberFromApiUrl :: String -> Maybe Int
extractNumberFromApiUrl url = do
  uri <- parseURI url
  let segments = filter (not . T.null) $ T.splitOn "/" $ toText (uriPath uri)
  case reverse (toList segments) of
    (idStr:_) | T.all isDigit idStr -> readMaybe (toString idStr)
    _ -> Nothing
