{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Sauron.Event.Open (
  openNode
  ) where

import Control.Monad
import Control.Monad.IO.Unlift
import Data.Char (isDigit)
import Data.Function
import GitHub
import Network.URI (parseURI, uriPath)
import Relude hiding (Down, pi)
import Sauron.Actions
import Sauron.Types


openNode :: (MonadIO m) => BaseContext -> NonEmpty (SomeNode Variable) -> Node Fixed a -> m ()
openNode _baseContext elems (JobLogGroupNode _) = case findParentJobNode (toList elems) of
  Just (SingleJobNode (EntityData {_state})) -> do
    jobState <- readTVarIO _state
    case fetchableCurrent jobState of
      Just job -> openBrowserToUrl (toString $ getUrl $ jobHtmlUrl job)
      Nothing -> return ()
  _ -> return ()
openNode _baseContext elems el = case getNodeUrl el (toList elems) of
  Just url -> openBrowserToUrl url
  Nothing -> return ()

getNodeUrl :: Node Fixed a -> [SomeNode Variable] -> Maybe String
getNodeUrl (PaginatedIssuesNode _) (findRepoBaseUrl -> Just repoBaseUrl) = Just (repoBaseUrl <> "/issues")
getNodeUrl (PaginatedPullsNode _) (findRepoBaseUrl -> Just repoBaseUrl) = Just (repoBaseUrl <> "/pulls")
getNodeUrl (PaginatedWorkflowsNode _) (findRepoBaseUrl -> Just repoBaseUrl) = Just (repoBaseUrl <> "/actions")
getNodeUrl (PaginatedBranchesNode _) (findRepoBaseUrl -> Just repoBaseUrl) = Just (repoBaseUrl <> "/branches")
getNodeUrl (OverallBranchesNode _) (findRepoBaseUrl -> Just repoBaseUrl) = Just (repoBaseUrl <> "/branches")
getNodeUrl (PaginatedYourBranchesNode _) (findRepoBaseUrl -> Just repoBaseUrl) = Just (repoBaseUrl <> "/branches/yours")
getNodeUrl (PaginatedActiveBranchesNode _) (findRepoBaseUrl -> Just repoBaseUrl) = Just (repoBaseUrl <> "/branches/active")
getNodeUrl (PaginatedStaleBranchesNode _) (findRepoBaseUrl -> Just repoBaseUrl) = Just (repoBaseUrl <> "/branches/stale")
getNodeUrl (PaginatedNotificationsNode _) _ = Just "https://github.com/notifications"
getNodeUrl (SingleNotificationNode (EntityData {_static=notification})) _ = Just (getNotificationUrl notification)
getNodeUrl (SingleIssueNode (EntityData {_static=(Issue {issueHtmlUrl=(Just url)})})) _parents = Just (toString $ getUrl url)
getNodeUrl (SinglePullNode (EntityData {_static=(Issue {issueHtmlUrl=(Just url)})})) _parents = Just (toString $ getUrl url)
getNodeUrl (SingleWorkflowNode (EntityData {_static=workflowRun})) _ = Just (toString $ getUrl $ workflowRunHtmlUrl workflowRun)
getNodeUrl (SingleJobNode (EntityData {_state=(fetchableCurrent -> Just job)})) _ = Just (toString $ getUrl $ jobHtmlUrl job)
getNodeUrl (SingleBranchNode (EntityData {_static=branch})) (findRepoBaseUrl -> Just repoBaseUrl) = Just (repoBaseUrl <> "/tree/" <> toString (branchName branch))
getNodeUrl (SingleBranchWithInfoNode (EntityData {_static=(branchInfo, _columnWidths)})) (findRepoBaseUrl -> Just repoBaseUrl) = Just (repoBaseUrl <> "/tree/" <> toString (branchWithInfoBranchName branchInfo))
getNodeUrl (SingleCommitNode (EntityData {_static=commit})) (findRepoBaseUrl -> Just repoBaseUrl) = Just (repoBaseUrl <> "/commit/" <> toString (untagName (commitSha commit)))
getNodeUrl (RepoNode _) (findRepoBaseUrl -> Just repoBaseUrl) = Just repoBaseUrl
getNodeUrl _ _ = Nothing

findRepoBaseUrl :: [SomeNode Variable] -> Maybe String
findRepoBaseUrl [] = Nothing
findRepoBaseUrl (SomeNode (RepoNode (EntityData {_static=(owner, name)})) : _) =
  Just ("https://github.com/" <> toString (untagName owner) <> "/" <> toString (untagName name))
findRepoBaseUrl (_ : rest) = findRepoBaseUrl rest

findParentJobNode :: [SomeNode Variable] -> Maybe (Node Variable SingleJobT)
findParentJobNode [] = Nothing
findParentJobNode (SomeNode (SingleJobNode ed) : _) = Just (SingleJobNode ed)
findParentJobNode (_ : rest) = findParentJobNode rest

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
