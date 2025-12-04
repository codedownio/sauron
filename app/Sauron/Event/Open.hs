{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Sauron.Event.Open (
  openNode
  ) where

import Control.Monad
import Control.Monad.IO.Unlift
import Data.Function
import GitHub
import Relude hiding (Down, pi)
import Sauron.Actions
import Sauron.Types


openNode :: (MonadIO m) => BaseContext -> NonEmpty (SomeNode Variable) -> Node Fixed a -> m ()
openNode baseContext _elems (SingleNotificationNode (EntityData {_static=notification})) =
  liftIO $ fetchNotificationHtmlUrl baseContext notification
openNode _baseContext elems (JobLogGroupNode _) = case findParentJobNode (toList elems) of
  Just (SingleJobNode (EntityData {_state})) -> do
    jobState <- readTVarIO _state
    case fetchableCurrent jobState of
      Just (job, _) -> openBrowserToUrl (toString $ getUrl $ jobHtmlUrl job)
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
getNodeUrl (PaginatedNotificationsNode _) _ = Just "https://github.com/notifications"
getNodeUrl (SingleIssueNode (EntityData {_static=(Issue {issueHtmlUrl=(Just url)})})) _parents = Just (toString $ getUrl url)
getNodeUrl (SinglePullNode (EntityData {_static=(Issue {issueHtmlUrl=(Just url)})})) _parents = Just (toString $ getUrl url)
getNodeUrl (SingleWorkflowNode (EntityData {_static=workflowRun})) _ = Just (toString $ getUrl $ workflowRunHtmlUrl workflowRun)
getNodeUrl (SingleJobNode (EntityData {_state=(fetchableCurrent -> Just (job, _))})) _ = Just (toString $ getUrl $ jobHtmlUrl job)
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

-- | Open notification using GitHub's notification URL
fetchNotificationHtmlUrl :: BaseContext -> Notification -> IO ()
fetchNotificationHtmlUrl _baseContext notification = do
  -- Simply use GitHub's notification URL - this is what GitHub web UI does
  -- and it will redirect to the appropriate issue/PR/comment automatically
  openBrowserToUrl (toString $ getUrl $ notificationUrl notification)
