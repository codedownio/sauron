{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Sauron.Fetch.Pull (
  fetchPulls
  , fetchMyPulls
  , fetchPullComments
  , fetchPullDetailsAndChecks
  , fetchPullCommits
  , fetchPullCommitDetail
  , fetchPullFiles
  , setPullTimeline
  ) where

import Control.Exception.Safe (bracketOnError_)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Map as M
import GitHub
import GitHub.Data.Name (Name(..))
import Relude
import Sauron.Actions.Util (withGithubApiSemaphore, githubWithLogging)
import Sauron.Fetch.Core
import Sauron.GraphQL.PullRequestFiles (queryPullRequestViewedStates)
import Sauron.Fetch.Issue (fetchIssueCommentsAndEvents)
import Sauron.Logging
import Sauron.Types

fetchPulls :: (
  HasCallStack, MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Node Variable PaginatedPullsT -> m ()
fetchPulls owner name (PaginatedPullsNode (EntityData {..})) = do
  (search, _pageInfo, _fetchable) <- readTVarIO _state
  extraTerms <- case search of
    SearchNone -> pure []
    SearchText t -> pure $ T.words t
  let fullQuery = T.intercalate "+" ([i|repo:#{untagName owner}/#{untagName name}|] : extraTerms)
  fetchPulls' fullQuery _state _children _depth

fetchMyPulls :: (
  HasCallStack, MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Node Variable PaginatedPullsT -> m ()
fetchMyPulls (PaginatedPullsNode (EntityData {..})) = do
  (search, _pageInfo, _fetchable) <- readTVarIO _state
  let fullQuery = case search of
        SearchNone -> ""
        SearchText t -> T.intercalate "+" (T.words t)
  fetchPulls' fullQuery _state _children _depth

fetchPulls' :: (
  HasCallStack, MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Text -> TVar (Search, PageInfo, Fetchable TotalCount) -> TVar [Node Variable SinglePullT] -> Int -> m ()
fetchPulls' fullQuery _state _children _depth = do
  bc <- ask

  fetchPaginatedWithState (searchIssuesR fullQuery) _state $ \case
    Left err -> do
      (s, p, _) <- readTVar _state
      writeTVar _state (s, p, Errored err)
      writeTVar _children []
    Right (SearchResult totalCount results, newPageInfo) -> do
      (s, _, _) <- readTVar _state
      writeTVar _state (s, newPageInfo, Fetched totalCount)
      (writeTVar _children =<<) $ forM (V.toList results) $ \issue@(Issue {..}) ->
        SinglePullNode <$> makeEmptyElemWithState bc issue emptyPullNodeState ("/pull/" <> show issueNumber) (_depth + 1)

-- | Apply a function to the timeline part of a pull node's state
setPullTimeline :: TVar PullNodeState -> (Fetchable (V.Vector TimelineEvent) -> Fetchable (V.Vector TimelineEvent)) -> STM ()
setPullTimeline stateVar f = modifyTVar' stateVar (\ps -> ps { pullNodeStateTimeline = f (pullNodeStateTimeline ps) })

fetchPullComments :: (
  HasCallStack, MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> IssueNumber -> TVar PullNodeState -> m ()
fetchPullComments owner name issueNumber stateVar = do
  ctx <- ask
  bracketOnError_ (atomically $ setPullTimeline stateVar (Fetching . fetchableCurrent))
                  (atomically $ setPullTimeline stateVar (const (Errored "Pull comments and events fetch failed with exception."))) $
    -- pullRequestCommentsR returns comments on the "unified diff"
    -- there are also "commit comments" and "issue comments".
    -- The last one are the most common on PRs, so we use commentsR
    liftIO (fetchIssueCommentsAndEvents ctx owner name (unIssueNumber issueNumber)) >>= \case
      Left err -> atomically $ setPullTimeline stateVar (const (Errored (show err)))
      Right merged -> atomically $ setPullTimeline stateVar (const (Fetched merged))

-- | Fetch a pull request's details (head sha, mergeability) and then the CI check runs
-- for its head commit, updating the corresponding parts of the node state.
fetchPullDetailsAndChecks :: (
  HasCallStack, MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> IssueNumber -> TVar PullNodeState -> m ()
fetchPullDetailsAndChecks owner name issueNumber stateVar = do
  let setDetails x = atomically $ modifyTVar' stateVar (\ps -> ps { pullNodeStateDetails = x })
  let setChecks x = atomically $ modifyTVar' stateVar (\ps -> ps { pullNodeStateChecks = x })
  atomically $ modifyTVar' stateVar $ \ps -> ps {
    pullNodeStateDetails = Fetching (fetchableCurrent (pullNodeStateDetails ps))
    , pullNodeStateChecks = Fetching (fetchableCurrent (pullNodeStateChecks ps))
    }
  withGithubApiSemaphore (githubWithLogging (pullRequestR owner name issueNumber)) >>= \case
    Left err -> do
      setDetails (Errored (show err))
      setChecks (Errored (show err))
    Right pr@(PullRequest {pullRequestHead=(PullRequestCommit {pullRequestCommitSha})}) -> do
      setDetails (Fetched pr)
      fetchAllCheckRuns owner name pullRequestCommitSha >>= \case
        Left err -> setChecks (Errored (show err))
        Right runs -> setChecks (Fetched runs)

-- | Fetch every page of check runs for a commit (the API returns at most 100 per page)
fetchAllCheckRuns :: (
  HasCallStack, MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Text -> m (Either Error (V.Vector CheckRun))
fetchAllCheckRuns owner name sha = go 1 []
  where
    go :: (HasCallStack, MonadReader BaseContext m, MonadIO m, MonadMask m) => Int -> [CheckRun] -> m (Either Error (V.Vector CheckRun))
    go page acc =
      withGithubApiSemaphore (githubWithLogging (checkRunsPageForR owner name (N sha) 100 page)) >>= \case
        Left err -> return (Left err)
        Right (CheckRunsResponse {checkRunsTotalCount, checkRunsCheckRuns}) -> do
          let acc' = acc <> V.toList checkRunsCheckRuns
          if length acc' >= checkRunsTotalCount || V.null checkRunsCheckRuns
            then return (Right (V.fromList acc'))
            else go (page + 1) acc'

-- | Fetch the list of commits in a pull request (the Commits tab)
fetchPullCommits :: (
  HasCallStack, MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> IssueNumber -> TVar PullNodeState -> m ()
fetchPullCommits owner name issueNumber stateVar = do
  let setCommits x = atomically $ modifyTVar' stateVar (\ps -> ps { pullNodeStateCommits = x })
  atomically $ modifyTVar' stateVar $ \ps ->
    ps { pullNodeStateCommits = Fetching (fetchableCurrent (pullNodeStateCommits ps)) }
  withGithubApiSemaphore (githubWithLogging (pullRequestCommitsR owner name issueNumber FetchAll)) >>= \case
    Left err -> setCommits (Errored (show err))
    Right commits -> setCommits (Fetched commits)

-- | Fetch one commit's full details (including its patches), for a commit expanded
-- in the Commits tab
fetchPullCommitDetail :: (
  HasCallStack, MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Name Commit -> TVar PullNodeState -> m ()
fetchPullCommitDetail owner name commitSha stateVar = do
  let sha = untagName commitSha
  let setDetail x = atomically $ modifyTVar' stateVar $ \ps ->
        ps { pullNodeStateCommitDetails = M.insert sha x (pullNodeStateCommitDetails ps) }
  setDetail (Fetching Nothing)
  withGithubApiSemaphore (githubWithLogging (commitR owner name commitSha)) >>= \case
    Left err -> setDetail (Errored (show err))
    Right detailedCommit -> setDetail (Fetched detailedCommit)

-- | Fetch the pull request's changed files and their per-file viewed states (the
-- Review tab). The viewed states are GraphQL-only, so they come in a second query.
fetchPullFiles :: (
  HasCallStack, MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> IssueNumber -> TVar PullNodeState -> m ()
fetchPullFiles owner name issueNumber@(IssueNumber number) stateVar = do
  bc <- ask
  let setFiles x = atomically $ modifyTVar' stateVar (\ps -> ps { pullNodeStateFiles = x })
  atomically $ modifyTVar' stateVar $ \ps ->
    ps { pullNodeStateFiles = Fetching (fetchableCurrent (pullNodeStateFiles ps)) }
  withGithubApiSemaphore (githubWithLogging (pullRequestFilesR owner name issueNumber FetchAll)) >>= \case
    Left err -> setFiles (Errored (show err))
    Right files -> do
      setFiles (Fetched files)
      queryPullRequestViewedStates bc owner name number >>= \case
        Left err -> warn [i|Couldn't fetch viewed states for PR \##{number}: #{err}|]
        Right (prId, states) -> atomically $ modifyTVar' stateVar $ \ps -> ps {
          pullNodeStatePullRequestId = Just prId
          , pullNodeStateViewedStates = states
          }
