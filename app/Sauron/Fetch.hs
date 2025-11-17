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
  , fetchIssueCommentsAndEvents

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
import Control.Monad (foldM)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import qualified Data.Map.Strict as Map
import Data.String.Interpolate
import qualified Data.Text as T
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..))
import qualified Data.Vector as V
import GitHub
import Network.HTTP.Conduit hiding (Proxy)
import qualified Network.URI as URI
import Relude
import Sauron.Actions.Util
import Sauron.Fetch.Core
import Sauron.Fetch.ParseJobLogs
import Sauron.Types
import UnliftIO.Async


fetchRepo :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> TVar (Fetchable Repo) -> m ()
fetchRepo owner name repoVar = do
  bracketOnError_ (atomically $ markFetching repoVar)
                  (atomically $ writeTVar repoVar (Errored "Repo fetch failed with exception.")) $
    withGithubApiSemaphore (githubWithLogging (repositoryR owner name)) >>= \case
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
  bc <- ask
  let branchSha = branchCommitSha $ branchCommit branch
  bracketOnError_ (atomically $ markFetching _state)
                  (atomically $ writeTVar _state (Errored "Branch commits fetch failed with exception.")) $
    withGithubApiSemaphore (executeRequestWithLogging (commitsWithOptionsForR owner name (FetchAtLeast 10) [CommitQuerySha branchSha])) >>= \case
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
  bracketOnError_ (atomically $ markFetching commitVar)
                  (atomically $ writeTVar commitVar (Errored "Commit details fetch failed with exception.")) $
    withGithubApiSemaphore (executeRequestWithLogging (commitR owner name commitSha)) >>= \case
      Left err -> atomically $ writeTVar commitVar (Errored (show err))
      Right (responseBody -> detailedCommit) -> atomically $ writeTVar commitVar (Fetched detailedCommit)

fetchIssue :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> IssueNumber -> TVar (Fetchable Issue) -> m ()
fetchIssue owner name issueNumber issueVar = do
  bracketOnError_ (atomically $ markFetching issueVar)
                  (atomically $ writeTVar issueVar (Errored "Issue fetch failed with exception.")) $
    withGithubApiSemaphore (executeRequestWithLogging (issueR owner name issueNumber)) >>= \case
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
    liftIO (fetchIssueCommentsAndEvents ctx owner name (unIssueNumber issueNumber)) >>= \case
      Left err -> atomically $ writeTVar inner (Errored (show err))
      Right merged -> atomically $ writeTVar inner (Fetched merged)

fetchIssueCommentsAndEvents :: BaseContext -> Name Owner -> Name Repo -> Int -> IO (Either Error (V.Vector (Either IssueEvent IssueComment)))
fetchIssueCommentsAndEvents baseContext owner name issueNumber = do
  let fetchComments =
        withGithubApiSemaphore' (requestSemaphore baseContext)
          (executeRequestWithLoggingDirect baseContext
            (commentsR owner name (IssueNumber issueNumber) FetchAll))

  let fetchEvents =
        withGithubApiSemaphore' (requestSemaphore baseContext)
          (executeRequestWithLoggingDirect baseContext
            (eventsForIssueR owner name (GitHub.mkId (Proxy :: Proxy Issue) issueNumber) FetchAll))

  (commentsResult, eventsResult) <- concurrently fetchComments fetchEvents

  case (commentsResult, eventsResult) of
    (Right commentsResponse, Right eventsResponse) -> do
      let comments = responseBody commentsResponse
      let events = responseBody eventsResponse
      return $ Right $ mergeCommentsAndEvents comments events
    (Left err, _) -> return $ Left err
    (_, Left err) -> return $ Left err
  where
    mergeCommentsAndEvents :: V.Vector IssueComment -> V.Vector IssueEvent -> V.Vector (Either IssueEvent IssueComment)
    mergeCommentsAndEvents comments events =
      let commentEntries = fmap (\c -> (issueCommentCreatedAt c, Right c)) comments
          eventEntries = fmap (\e -> (issueEventCreatedAt e, Left e)) events
          allEntries = V.toList commentEntries <> V.toList eventEntries
          sortedEntries = sortOn fst allEntries -- Sort by timestamp, newest first
      in V.fromList (fmap snd sortedEntries)

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
    liftIO (fetchIssueCommentsAndEvents ctx owner name (unIssueNumber issueNumber)) >>= \case
      Left err -> atomically $ writeTVar inner (Errored (show err))
      Right merged -> atomically $ writeTVar inner (Fetched merged)

fetchWorkflowJobs :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Id WorkflowRun -> Node Variable SingleWorkflowT -> m ()
fetchWorkflowJobs owner name workflowRunId (SingleWorkflowNode (EntityData {..})) = do
  bc <- ask
  bracketOnError_ (atomically $ markFetching _state)
                  (atomically $ writeTVar _state (Errored "Workflow jobs fetch failed with exception.")) $
    -- pullRequestCommentsR returns comments on the "unified diff"
    -- there are also "commit comments" and "issue comments".
    -- The last one are the most common on PRs, so we use commentsR
    withGithubApiSemaphore (executeRequestWithLogging (jobsForWorkflowRunR owner name workflowRunId FetchAll)) >>= \case
      Left err -> do
        -- traceM [i|Error fetching workflow jobs: #{err}|]
        atomically $ writeTVar _state (Errored (show err))
      Right (responseBody -> wtc@(WithTotalCount results _totalCount)) -> atomically $ do
        writeTVar _state (Fetched wtc)

        -- Preserve existing job nodes and their toggle state
        existingChildren <- readTVar _children

        -- Build a map of existing jobs by their IDs
        existingJobsMap <- foldM (\acc node -> case node of
          SingleJobNode (EntityData {_state=jobState}) -> do
            currentState <- readTVar jobState
            case currentState of
              Fetched (existingJob, _) -> return $ Map.insert (jobId existingJob) node acc
              Fetching (Just (existingJob, _)) -> return $ Map.insert (jobId existingJob) node acc
              _ -> return acc
          ) (Map.empty :: Map.Map (Id Job) (Node Variable SingleJobT)) existingChildren

        (writeTVar _children =<<) $ forM (V.toList results) $ \job@(Job {jobId}) -> do
          case Map.lookup jobId existingJobsMap of
            Just existingNode@(SingleJobNode existingEntityData) -> do
              -- Update existing node's job data while preserving toggle state
              let EntityData {_state = jobState} = existingEntityData
              currentState <- readTVar jobState
              case currentState of
                Fetched (_, logGroups) -> writeTVar jobState (Fetched (job, logGroups))
                Fetching (Just (_, logGroups)) -> writeTVar jobState (Fetching (Just (job, logGroups)))
                _ -> writeTVar jobState (Fetched (job, []))
              return existingNode
            Nothing -> do
              -- Create new node for new jobs
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
  withGithubApiSemaphore (executeRequestWithLogging (jobR owner name jobId)) >>= \case
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
