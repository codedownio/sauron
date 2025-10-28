{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Sauron.Fetch (
  fetchRepo
  , fetchRepos

  , fetchWorkflows

  , fetchPulls
  , fetchPullComments

  , fetchIssues
  , fetchIssue
  , fetchIssueComments

  , fetchBranches
  , fetchBranchCommits
  , fetchCommitDetails

  , fetchNotifications

  , fetchWorkflowJobs
  , fetchJobLogs
  , fetchJob

  , makeEmptyElem
  ) where

import Control.Exception.Safe (bracketOnError_)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import Data.String.Interpolate
import qualified Data.Text as T
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..))
import qualified Data.Vector as V
import GitHub
import Network.HTTP.Conduit
import qualified Network.URI as URI
import Relude
import Sauron.Actions.Util
import Sauron.Event.CommentModal (fetchCommentsAndEvents)
import Sauron.Fetch.Core
import Sauron.Fetch.ParseJobLogs
import Sauron.Types


fetchRepo :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> TVar (Fetchable Repo) -> m ()
fetchRepo owner name repoVar = do
  BaseContext {auth} <- ask
  bracketOnError_ (atomically $ markFetching repoVar)
                  (atomically $ writeTVar repoVar (Errored "Repo fetch failed with exception.")) $
    withGithubApiSemaphore (liftIO $ github auth (repositoryR owner name)) >>= \case
      Left err -> atomically $ writeTVar repoVar (Errored (show err))
      Right x -> atomically $ writeTVar repoVar (Fetched x)

fetchRepos :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Node Variable PaginatedReposT -> m ()
fetchRepos (PaginatedReposNode (EntityData {..})) = do
  terms <- readTVarIO _search >>= \case
    SearchNone -> pure []
    SearchText t -> pure $ T.words t
  let fullQuery = T.intercalate "+" terms

  bc <- ask
  fetchPaginated'' (searchReposR fullQuery) _pageInfo _state $ \case
    Left err -> do
      writeTVar _state (Errored (show err))
      writeTVar _children []
    Right (sr@(SearchResult _totalCount results), newPageInfo) -> do
      writeTVar _pageInfo newPageInfo
      writeTVar _state (Fetched sr)
      (writeTVar _children =<<) $ forM (V.toList results) $ \r -> do
        let nsName = (simpleOwnerLogin $ repoOwner r, repoName r)
        entityData@(EntityData {_state=innerState}) <- makeEmptyElem bc nsName "" (_depth + 1)
        writeTVar innerState (Fetched r)
        return $ RepoNode entityData

fetchIssues :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Node Variable PaginatedIssuesT -> m ()
fetchIssues owner name (PaginatedIssuesNode (EntityData {..})) = do
  extraTerms <- readTVarIO _search >>= \case
    SearchNone -> pure []
    SearchText t -> pure $ T.words t
  let fullQuery = T.intercalate "+" ([i|repo:#{untagName owner}/#{untagName name}|] : extraTerms)

  bc <- ask

  fetchPaginated'' (searchIssuesR fullQuery) _pageInfo _state $ \case
    Left err -> do
      writeTVar _state (Errored (show err))
      writeTVar _children []
    Right (sr@(SearchResult _totalCount results), newPageInfo) -> do
      writeTVar _pageInfo newPageInfo
      writeTVar _state (Fetched sr)
      (writeTVar _children =<<) $ forM (V.toList results) $ \issue@(Issue {..}) ->
        SingleIssueNode <$> makeEmptyElem bc issue ("/issue/" <> show issueNumber) (_depth + 1)

fetchPulls :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Node Variable PaginatedPullsT -> m ()
fetchPulls owner name (PaginatedPullsNode (EntityData {..})) = do
  extraTerms <- readTVarIO _search >>= \case
    SearchNone -> pure []
    SearchText t -> pure $ T.words t
  let fullQuery = T.intercalate "+" ([i|repo:#{untagName owner}/#{untagName name}|] : extraTerms)

  bc <- ask

  fetchPaginated'' (searchIssuesR fullQuery) _pageInfo _state $ \case
    Left err -> do
      writeTVar _state (Errored (show err))
      writeTVar _children []
    Right (sr@(SearchResult _totalCount results), newPageInfo) -> do
      writeTVar _pageInfo newPageInfo
      writeTVar _state (Fetched sr)
      (writeTVar _children =<<) $ forM (V.toList results) $ \issue@(Issue {..}) ->
        SinglePullNode <$> makeEmptyElem bc issue ("/pull/" <> show issueNumber) (_depth + 1)

fetchBranches :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Node Variable PaginatedBranchesT -> m ()
fetchBranches owner name (PaginatedBranchesNode (EntityData {..})) = do
  bc <- ask
  fetchPaginated'' (branchesForR owner name) _pageInfo _state $ \case
    Left err -> do
      writeTVar _state (Errored (show err))
      writeTVar _children []
    Right (branches, newPageInfo) -> do
      writeTVar _pageInfo newPageInfo
      writeTVar _state (Fetched branches)
      (writeTVar _children =<<) $ forM (V.toList branches) $ \branch@(Branch {..}) ->
        SingleBranchNode <$> makeEmptyElem bc branch ("/tree/" <> branchName) (_depth + 1)

fetchNotifications :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
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

fetchBranchCommits :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Node Variable SingleBranchT -> m ()
fetchBranchCommits owner name (SingleBranchNode (EntityData {_static=branch, ..})) = do
  bc@(BaseContext {auth, manager}) <- ask
  let branchSha = branchCommitSha $ branchCommit branch
  bracketOnError_ (atomically $ markFetching _state)
                  (atomically $ writeTVar _state (Errored "Branch commits fetch failed with exception.")) $
    withGithubApiSemaphore (liftIO $ executeRequestWithMgrAndRes manager auth (commitsWithOptionsForR owner name (FetchAtLeast 10) [CommitQuerySha branchSha])) >>= \case
      Left err -> atomically $ do
        writeTVar _state (Errored (show err))
        writeTVar _children []
      Right (responseBody -> commits) -> atomically $ do
        writeTVar _state (Fetched commits)
        (writeTVar _children =<<) $ forM (V.toList commits) $ \commit@(Commit {..}) ->
          SingleCommitNode <$> makeEmptyElem bc commit ("/commit/" <> T.pack (toString (untagName commitSha))) (_depth + 1)

fetchCommitDetails :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Name Commit -> TVar (Fetchable Commit) -> m ()
fetchCommitDetails owner name commitSha commitVar = do
  BaseContext {auth, manager} <- ask
  bracketOnError_ (atomically $ markFetching commitVar)
                  (atomically $ writeTVar commitVar (Errored "Commit details fetch failed with exception.")) $
    withGithubApiSemaphore (liftIO $ executeRequestWithMgrAndRes manager auth (commitR owner name commitSha)) >>= \case
      Left err -> atomically $ writeTVar commitVar (Errored (show err))
      Right (responseBody -> detailedCommit) -> atomically $ writeTVar commitVar (Fetched detailedCommit)

fetchIssue :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> IssueNumber -> TVar (Fetchable Issue) -> m ()
fetchIssue owner name issueNumber issueVar = do
  BaseContext {auth, manager} <- ask
  bracketOnError_ (atomically $ markFetching issueVar)
                  (atomically $ writeTVar issueVar (Errored "Issue fetch failed with exception.")) $
    withGithubApiSemaphore (liftIO $ executeRequestWithMgrAndRes manager auth (issueR owner name issueNumber)) >>= \case
      Left err -> atomically $ writeTVar issueVar (Errored (show err))
      Right x -> atomically $ writeTVar issueVar (Fetched (responseBody x))

fetchWorkflows :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Node Variable PaginatedWorkflowsT -> m ()
fetchWorkflows owner name (PaginatedWorkflowsNode (EntityData {..})) = do
  bc <- ask
  fetchPaginated'' (workflowRunsR owner name mempty) _pageInfo _state $ \case
    Left err -> do
      writeTVar _state (Errored (show err))
      writeTVar _children []
    Right (wtc@(WithTotalCount results _totalCount), newPageInfo) -> do
      writeTVar _pageInfo newPageInfo
      writeTVar _state (Fetched wtc)
      (writeTVar _children =<<) $ forM (V.toList results) $ \workflow@(WorkflowRun {}) ->
        SingleWorkflowNode <$> makeEmptyElem bc workflow "" (_depth + 1)

fetchIssueComments :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> IssueNumber -> TVar (Fetchable (V.Vector (Either IssueEvent IssueComment))) -> m ()
fetchIssueComments owner name issueNumber inner = do
  ctx <- ask
  bracketOnError_ (atomically $ markFetching inner)
                  (atomically $ writeTVar inner (Errored "Issue comments and events fetch failed with exception.")) $
    liftIO (fetchCommentsAndEvents ctx owner name (unIssueNumber issueNumber)) >>= \case
      Left err -> atomically $ writeTVar inner (Errored (show err))
      Right merged -> atomically $ writeTVar inner (Fetched merged)

fetchPullComments :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> IssueNumber -> TVar (Fetchable (V.Vector (Either IssueEvent IssueComment))) -> m ()
fetchPullComments owner name issueNumber inner = do
  ctx <- ask
  bracketOnError_ (atomically $ markFetching inner)
                  (atomically $ writeTVar inner (Errored "Pull comments and events fetch failed with exception.")) $
    -- pullRequestCommentsR returns comments on the "unified diff"
    -- there are also "commit comments" and "issue comments".
    -- The last one are the most common on PRs, so we use commentsR
    liftIO (fetchCommentsAndEvents ctx owner name (unIssueNumber issueNumber)) >>= \case
      Left err -> atomically $ writeTVar inner (Errored (show err))
      Right merged -> atomically $ writeTVar inner (Fetched merged)

fetchWorkflowJobs :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Id WorkflowRun -> Node Variable SingleWorkflowT -> m ()
fetchWorkflowJobs owner name workflowRunId (SingleWorkflowNode (EntityData {..})) = do
  bc@(BaseContext {auth, manager}) <- ask
  bracketOnError_ (atomically $ markFetching _state)
                  (atomically $ writeTVar _state (Errored "Workflow jobs fetch failed with exception.")) $
    -- pullRequestCommentsR returns comments on the "unified diff"
    -- there are also "commit comments" and "issue comments".
    -- The last one are the most common on PRs, so we use commentsR
    withGithubApiSemaphore (liftIO $ executeRequestWithMgrAndRes manager auth (jobsForWorkflowRunR owner name workflowRunId FetchAll)) >>= \case
      Left err -> do
        -- traceM [i|Error fetching workflow jobs: #{err}|]
        atomically $ writeTVar _state (Errored (show err))
      Right (responseBody -> wtc@(WithTotalCount results _totalCount)) -> atomically $ do
        writeTVar _state (Fetched wtc)
        (writeTVar _children =<<) $ forM (V.toList results) $ \job@(Job {}) -> do
          entityData <- makeEmptyElem bc () "" (_depth + 1)
          let EntityData {_state = jobState} = entityData
          writeTVar jobState (Fetched (job, []))
          return $ SingleJobNode entityData
          -- writeTVar (_state child) (PaginatedItemJob)
  -- TODO: do pagination for these jobs? The web UI doesn't seem to...
  -- bc <- ask
  -- fetchPaginated'' (jobsForWorkflowRunR owner name workflowRunId) _pageInfo (writeTVar _state) $ \case
  --   Left err -> do
  --     writeTVar _state (Errored (show err))
  --     writeTVar _children []
  --   Right (wtc@(WithTotalCount results _totalCount), newPageInfo) -> do
  --     writeTVar _pageInfo newPageInfo
  --     writeTVar _state (Fetched (PaginatedItemsJobs wtc))
  --     (writeTVar _children =<<) $ forM (V.toList results) $ \job@(Job {}) ->
  --       makeEmptyElem bc (SingleJob job) "" (_depth + 1)

fetchJobLogs :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Job -> Node Variable SingleJobT -> m ()
fetchJobLogs owner name (Job {jobId, jobSteps}) (SingleJobNode (EntityData {..})) = do
  BaseContext {auth, manager} <- ask
  bracketOnError_ (atomically $ markFetching _state)
                  (atomically $ writeTVar _state (Errored "Job logs fetch failed with exception.")) $ do
    -- First, get the download URI
    withGithubApiSemaphore (liftIO $ executeRequestWithMgrAndRes manager auth (downloadJobLogsR owner name jobId)) >>= \case
      Left err -> atomically $ writeTVar _state (Errored (show err))
      Right response -> do
        let uri = responseBody response
        -- traceM [i|Jobs URI: #{uri}|]
        logs <- simpleHttp (URI.uriToString id uri "")

        let parsedLogs = parseJobLogs (T.splitOn "\n" (decodeUtf8 logs))
        -- traceM [i|parsedLogs: #{parsedLogs}|]

        bc <- ask
        children' <- liftIO $ atomically $ do
          mapM (createJobStepNode bc (_depth + 1) parsedLogs) (V.toList jobSteps)

        atomically $ do
          currentState <- readTVar _state
          case currentState of
            Fetched (job, _) -> writeTVar _state (Fetched (job, parsedLogs))
            Fetching (Just (job, _)) -> writeTVar _state (Fetched (job, parsedLogs))
            Fetching Nothing -> writeTVar _state (Errored "No job data available")
            _ -> writeTVar _state (Errored "Invalid job state")
          writeTVar _children children'

fetchJob :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Id Job -> Node Variable SingleJobT -> m ()
fetchJob owner name jobId (SingleJobNode (EntityData {_state})) = do
  BaseContext {auth, manager} <- ask
  withGithubApiSemaphore (liftIO $ executeRequestWithMgrAndRes manager auth (jobR owner name jobId)) >>= \case
    Left _err -> return () -- Silently fail for health checks
    Right (responseBody -> updatedJob) -> liftIO $ atomically $ do
      currentState <- readTVar _state
      case currentState of
        Fetched (_, logGroups) -> writeTVar _state (Fetched (updatedJob, logGroups))
        Fetching (Just (_, logGroups)) -> writeTVar _state (Fetching (Just (updatedJob, logGroups)))
        Fetching Nothing -> writeTVar _state (Fetching (Just (updatedJob, [])))
        _ -> writeTVar _state (Fetched (updatedJob, []))

-- * Util

makeEmptyElem :: BaseContext -> NodeStatic a -> Text -> Int -> STM (EntityData Variable a)
makeEmptyElem (BaseContext {getIdentifierSTM}) typ' urlSuffix' depth' = do
  stateVar <- newTVar NotFetched
  ident' <- getIdentifierSTM
  toggledVar <- newTVar False
  childrenVar <- newTVar []
  searchVar <- newTVar $ SearchNone
  pageInfoVar <- newTVar emptyPageInfo
  healthCheckVar <- newTVar NotFetched
  healthCheckThreadVar <- newTVar Nothing
  return $ EntityData {
    _static = typ'
    , _state = stateVar

    , _urlSuffix = urlSuffix'

    , _toggled = toggledVar
    , _children = childrenVar

    , _search = searchVar
    , _pageInfo = pageInfoVar

    , _healthCheck = healthCheckVar
    , _healthCheckThread = healthCheckThreadVar

    , _depth = depth'
    , _ident = ident'
}

createJobStepNode :: BaseContext -> Int -> [JobLogGroup] -> JobStep -> STM (Node Variable 'JobLogGroupT)
createJobStepNode bc depth' allLogs jobStep = do
  let stepTitle = untagName (jobStepName jobStep)
  let stepTimestamp = fromMaybe (UTCTime (fromGregorian 1970 1 1) 0) (jobStepStartedAt jobStep)
  let stepStatus = Just $ fromMaybe (jobStepStatus jobStep) (jobStepConclusion jobStep)
  createJobLogGroupChildren bc depth' (JobLogGroup stepTimestamp stepTitle stepStatus logsForStep)
  where
    logsForStep :: [JobLogGroup]
    logsForStep =
      let startTime = jobStepStartedAt jobStep
          endTime = jobStepCompletedAt jobStep
      in filter (logInTimeRange startTime endTime) allLogs

    logInTimeRange :: Maybe UTCTime -> Maybe UTCTime -> JobLogGroup -> Bool
    logInTimeRange startTime endTime logGroup =
      let logTime = getLogTimestamp logGroup
      in case (startTime, endTime) of
           (Just start, Just end) -> logTime >= start && logTime <= end
           (Just start, Nothing) -> logTime >= start
           (Nothing, Just end) -> logTime <= end
           (Nothing, Nothing) -> True

    getLogTimestamp :: JobLogGroup -> UTCTime
    getLogTimestamp (JobLogLines timestamp _) = timestamp
    getLogTimestamp (JobLogGroup timestamp _ _ _) = timestamp

createJobLogGroupChildren :: BaseContext -> Int -> JobLogGroup -> STM (Node Variable 'JobLogGroupT)
createJobLogGroupChildren bc depth' jobLogGroup = do
  stateVar <- newTVar (Fetched ())
  ident' <- getIdentifierSTM bc
  toggledVar <- newTVar False
  searchVar <- newTVar SearchNone
  pageInfoVar <- newTVar emptyPageInfo

  childrenVar <- case jobLogGroup of
    JobLogLines _ _ -> newTVar []
    JobLogGroup _ _ (Just _status) _children' -> newTVar []  -- Top-level job groups don't have children in the tree
    JobLogGroup _ _ Nothing children' -> do  -- Only nested log groups have children
      childElems <- mapM (createJobLogGroupChildren bc (depth' + 1)) children'
      newTVar childElems

  healthCheckVar2 <- newTVar NotFetched
  healthCheckThreadVar2 <- newTVar Nothing
  return $ JobLogGroupNode $ EntityData {
    _static = jobLogGroup
    , _state = stateVar
    , _urlSuffix = ""
    , _toggled = toggledVar
    , _children = childrenVar
    , _search = searchVar
    , _pageInfo = pageInfoVar
    , _healthCheck = healthCheckVar2
    , _healthCheckThread = healthCheckThreadVar2
    , _depth = depth'
    , _ident = ident'
  }
