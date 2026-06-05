{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Sauron.Event.Open (
  openNode
  ) where

import Control.Monad.IO.Unlift
import Data.Char (isDigit)
import Data.Function
import GitHub
import Network.URI (parseURI, uriPath)
import Relude hiding (Down, pi)
import Sauron.Actions
import Sauron.Fetch.Notification (fetchNotificationContent)
import Sauron.Logging
import Sauron.Types
import UnliftIO.Async (async)


openNode :: (MonadIO m) => BaseContext -> SomeNode Variable -> NonEmpty (SomeNode Variable) -> Node Fixed a -> m ()
openNode bc (SomeNode (SingleNotificationNode (EntityData {_static=notification, _state=stateVar}))) _ _ =
  -- Resolve the notification's web URL in the background, fetching its content first if needed
  -- (the same fetch that runs when the notification is opened) so 'o' always lands on the right
  -- page, whether or not the notification has been opened yet.
  void $ liftIO $ async $ flip runReaderT bc $ do
    (notificationStateContent <$> readTVarIO stateVar) >>= \case
      Fetched _ -> pure ()
      _ -> fetchNotificationContent notification stateVar
    openBrowserToUrl . notificationWebUrl notification =<< readTVarIO stateVar
openNode bc _ elems el = case getNodeUrl el (toList elems) of
  Just url -> openBrowserToUrl url
  Nothing -> warn' bc [i|(#{el}) Couldn't find URL to open node|]

-- | Resolve a notification's web URL. Releases are referenced only by API id in the notification,
-- so we read the real web URL from the fetched release; everything else is derived from the
-- notification subject (which preserves issue/PR comment anchors).
notificationWebUrl :: Notification -> NotificationState -> String
notificationWebUrl notification (NotificationState {notificationStateContent=content}) = case content of
  Fetched (NotificationRelease release) -> toString $ getUrl $ releaseHtmlUrl release
  _ -> getNotificationUrl notification

getNodeUrl :: Node f a -> [SomeNode Variable] -> Maybe String
getNodeUrl (PaginatedIssuesNode _) (findRepoBaseUrl -> Just repoBaseUrl) = Just (repoBaseUrl <> "/issues")
getNodeUrl (PaginatedPullsNode _) (findRepoBaseUrl -> Just repoBaseUrl) = Just (repoBaseUrl <> "/pulls")
getNodeUrl (PaginatedWorkflowsNode _) (findRepoBaseUrl -> Just repoBaseUrl) = Just (repoBaseUrl <> "/actions")
getNodeUrl (PaginatedBranchesNode _) (findRepoBaseUrl -> Just repoBaseUrl) = Just (repoBaseUrl <> "/branches")
getNodeUrl (PaginatedYourBranchesNode _) (findRepoBaseUrl -> Just repoBaseUrl) = Just (repoBaseUrl <> "/branches/yours")
getNodeUrl (PaginatedActiveBranchesNode _) (findRepoBaseUrl -> Just repoBaseUrl) = Just (repoBaseUrl <> "/branches/active")
getNodeUrl (PaginatedStaleBranchesNode _) (findRepoBaseUrl -> Just repoBaseUrl) = Just (repoBaseUrl <> "/branches/stale")
getNodeUrl (PaginatedNotificationsNode _) _ = Just "https://github.com/notifications"
getNodeUrl (SingleNotificationNode (EntityData {_static=notification})) _ = Just (getNotificationUrl notification)
getNodeUrl (SingleIssueNode (EntityData {_static=(Issue {issueHtmlUrl=(Just url)})})) _parents = Just (toString $ getUrl url)
getNodeUrl (SinglePullNode (EntityData {_static=(Issue {issueHtmlUrl=(Just url)})})) _parents = Just (toString $ getUrl url)
getNodeUrl (SingleWorkflowNode (EntityData {_static=workflowRun})) _ = Just (toString $ getUrl $ workflowRunHtmlUrl workflowRun)
getNodeUrl (SingleJobNode (EntityData {_static=job})) _ = Just (toString $ getUrl $ jobHtmlUrl job)
getNodeUrl (JobLogGroupNode _) (findJobParent -> Just (SomeNode node, rest)) = getNodeUrl node rest
getNodeUrl (SingleBranchNode (EntityData {_static=branch})) (findRepoBaseUrl -> Just repoBaseUrl) = Just (repoBaseUrl <> "/tree/" <> toString (branchName branch))
getNodeUrl (SingleBranchWithInfoNode (EntityData {_static=(branchInfo, _columnWidths)})) (findRepoBaseUrl -> Just repoBaseUrl) = Just (repoBaseUrl <> "/tree/" <> toString (branchWithInfoBranchName branchInfo))
getNodeUrl (SingleCommitNode (EntityData {_static=commit})) (findRepoBaseUrl -> Just repoBaseUrl) = Just (repoBaseUrl <> "/commit/" <> toString (untagName (commitSha commit)))
getNodeUrl (RepoNode _) (findRepoBaseUrl -> Just repoBaseUrl) = Just repoBaseUrl
getNodeUrl _ _ = Nothing

findJobParent :: [SomeNode Variable] -> Maybe (SomeNode Variable, [SomeNode Variable])
findJobParent [] = Nothing
findJobParent (n@(SomeNode (SingleJobNode _)) : rest) = Just (n, rest)
findJobParent (_ : rest) = findJobParent rest

findRepoBaseUrl :: [SomeNode Variable] -> Maybe String
findRepoBaseUrl [] = Nothing
findRepoBaseUrl (SomeNode (RepoNode (EntityData {_static=(owner, name)})) : _) =
  Just ("https://github.com/" <> toString (untagName owner) <> "/" <> toString (untagName name))
findRepoBaseUrl (_ : rest) = findRepoBaseUrl rest

getNotificationUrl :: Notification -> String
getNotificationUrl notification = case (subjectLatestCommentURL, subjectURL, subjectType) of
  -- If there's a latest comment URL, try to convert it to a web URL with anchor
  (Just commentUrl, Just subUrl, "Issue") ->
    case (extractIdFromApiUrl (toString $ getUrl commentUrl), extractIdFromApiUrl (toString $ getUrl subUrl)) of
      (Just commentId, Just issueId) -> "https://github.com/" <> toString ownerName <> "/" <> toString repoName <> "/issues/" <> issueId <> "#issuecomment-" <> commentId
      _ -> toString $ getUrl commentUrl
  (Just commentUrl, Just subUrl, "PullRequest") ->
    case (extractIdFromApiUrl (toString $ getUrl commentUrl), extractIdFromApiUrl (toString $ getUrl subUrl)) of
      (Just commentId, Just prId) -> "https://github.com/" <> toString ownerName <> "/" <> toString repoName <> "/pull/" <> prId <> "#issuecomment-" <> commentId
      _ -> toString $ getUrl commentUrl
  -- Fallback to subject URL if no latest comment
  (Nothing, Just subUrl, "Issue") ->
    case extractIdFromApiUrl (toString $ getUrl subUrl) of
      Just issueId -> "https://github.com/" <> toString ownerName <> "/" <> toString repoName <> "/issues/" <> issueId
      Nothing -> toString $ getUrl subUrl
  (Nothing, Just subUrl, "PullRequest") ->
    case extractIdFromApiUrl (toString $ getUrl subUrl) of
      Just prId -> "https://github.com/" <> toString ownerName <> "/" <> toString repoName <> "/pull/" <> prId
      Nothing -> toString $ getUrl subUrl
  -- Best-effort fallback only: 'openNode' fetches the release and opens its exact web URL.
  -- This branch is reached only if that fetch failed, so fall back to the repo's releases page
  -- (the subject URL is an API endpoint, and the tag can't be reconstructed from the release id).
  (_, _, typ) | typ == subjectTypeRelease -> "https://github.com/" <> toString ownerName <> "/" <> toString repoName <> "/releases"
  (_, Just subUrl, _) -> toString $ getUrl subUrl  -- Fallback to API URL for other types
  (_, Nothing, _) -> toString $ getUrl $ notificationUrl notification
  where
    Subject {..} = notificationSubject notification
    RepoRef {..} = notificationRepo notification
    SimpleOwner {..} = repoRefOwner
    repoName = untagName repoRefRepo
    ownerName = untagName simpleOwnerLogin

    extractIdFromApiUrl :: String -> Maybe String
    extractIdFromApiUrl url = do
      uri <- parseURI url
      let pathSegments = filter (not . null) $ splitOn '/' (uriPath uri)
      case reverse pathSegments of
        (idStr:_) -> if all isDigit idStr then Just idStr else Nothing
        _ -> Nothing

    splitOn :: Char -> String -> [String]
    splitOn c s = case dropWhile (== c) s of
      "" -> []
      s' -> w : splitOn c s''
        where (w, s'') = break (== c) s'
