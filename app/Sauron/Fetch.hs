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
  , fetchOverallBranches
  , fetchYourBranches
  , fetchActiveBranches
  , fetchStaleBranches
  , fetchBranchCommits
  , fetchBranchWithInfoCommits
  , fetchCommitDetails

  -- REST API branch fetching
  , fetchGitHubBranches
  , fetchGitHubYourBranches
  , fetchGitHubActiveBranches
  , fetchGitHubStaleBranches

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
import Control.Monad.Logger (LogLevel(..))
import Data.Aeson (eitherDecode')
import qualified Data.List as L
import qualified Data.List as List ()
import qualified Data.Map.Strict as Map
import Data.String.Interpolate
import qualified Data.Text as T
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), getCurrentTime, diffUTCTime)
import qualified Data.Vector as V
import GitHub
import Network.HTTP.Conduit hiding (Proxy)
import qualified Network.URI as URI
import Relude
import Sauron.Actions.Util
import Sauron.Fetch.Core
import Sauron.Fetch.ParseJobLogs
import qualified Sauron.GraphQL as GraphQL ()
import Sauron.Types
import Sauron.Types.Branches
import Sauron.UI.BranchWithInfo ()
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


-- Wrapper functions for compatibility with the Action system
fetchYourBranches :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Maybe Text -> Node Variable PaginatedYourBranchesT -> m ()
fetchYourBranches owner name _repoDefaultBranch (PaginatedYourBranchesNode (EntityData {..})) = do
  currentPageInfo <- liftIO $ readTVarIO _pageInfo
  let currentPage = pageInfoCurrentPage currentPageInfo
  fetchGitHubYourBranches owner name currentPage _state

fetchActiveBranches :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Maybe Text -> Node Variable PaginatedActiveBranchesT -> m ()
fetchActiveBranches owner name _repoDefaultBranch (PaginatedActiveBranchesNode (EntityData {..})) = do
  currentPageInfo <- liftIO $ readTVarIO _pageInfo
  let currentPage = pageInfoCurrentPage currentPageInfo
  fetchGitHubActiveBranches owner name currentPage _state

fetchStaleBranches :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Maybe Text -> Node Variable PaginatedStaleBranchesT -> m ()
fetchStaleBranches owner name _repoDefaultBranch (PaginatedStaleBranchesNode (EntityData {..})) = do
  currentPageInfo <- liftIO $ readTVarIO _pageInfo
  let currentPage = pageInfoCurrentPage currentPageInfo
  fetchGitHubStaleBranches owner name currentPage _state

fetchOverallBranches :: (
  MonadReader BaseContext m, MonadIO m
  ) => Name Owner -> Name Repo -> Node Variable OverallBranchesT -> m ()
fetchOverallBranches _owner _name (OverallBranchesNode (EntityData {..})) = do
  bc <- ask
  -- Create categorized branch sections similar to GitHub's interface
  categorizedChildren <- atomically $ do
    yourBranchesEd <- makeEmptyElem bc () "your-branches" (_depth + 1)
    activeBranchesEd <- makeEmptyElem bc () "active-branches" (_depth + 1)
    staleBranchesEd <- makeEmptyElem bc () "stale-branches" (_depth + 1)
    return [
      SomeNode (PaginatedYourBranchesNode yourBranchesEd)
      , SomeNode (PaginatedActiveBranchesNode activeBranchesEd)
      , SomeNode (PaginatedStaleBranchesNode staleBranchesEd)
      ]

  atomically $ do
    writeTVar _state (Fetched ())
    writeTVar _children categorizedChildren

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
    withGithubApiSemaphore (githubWithLogging (commitsWithOptionsForR owner name (FetchAtLeast 10) [CommitQuerySha branchSha])) >>= \case
      Left err -> atomically $ do
        writeTVar _state (Errored (show err))
        writeTVar _children []
      Right commits -> atomically $ do
        writeTVar _state (Fetched commits)
        (writeTVar _children =<<) $ forM (V.toList commits) $ \commit@(Commit {..}) ->
          SingleCommitNode <$> makeEmptyElem bc commit ("/commit/" <> T.pack (toString (untagName commitSha))) (_depth + 1)

fetchBranchWithInfoCommits :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Node Variable SingleBranchWithInfoT -> m ()
fetchBranchWithInfoCommits owner name (SingleBranchWithInfoNode (EntityData {_static=(branchInfo, _columnWidths), ..})) = do
  bc <- ask
  case branchWithInfoCommitOid branchInfo of
    Nothing -> atomically $ writeTVar _state (Errored "No commit OID available for branch with info")
    Just commitOid -> do
      bracketOnError_ (atomically $ markFetching _state)
                      (atomically $ writeTVar _state (Errored "Branch commits fetch failed with exception.")) $
        withGithubApiSemaphore (githubWithLogging (commitsWithOptionsForR owner name (FetchAtLeast 10) [CommitQuerySha commitOid])) >>= \case
          Left err -> atomically $ do
            writeTVar _state (Errored (show err))
            writeTVar _children []
          Right commits -> atomically $ do
            writeTVar _state (Fetched commits)
            (writeTVar _children =<<) $ forM (V.toList commits) $ \commit@(Commit {..}) ->
              SingleCommitNode <$> makeEmptyElem bc commit ("/commit/" <> T.pack (toString (untagName commitSha))) (_depth + 1)

fetchCommitDetails :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Name Commit -> TVar (Fetchable Commit) -> m ()
fetchCommitDetails owner name commitSha commitVar = do
  bracketOnError_ (atomically $ markFetching commitVar)
                  (atomically $ writeTVar commitVar (Errored "Commit details fetch failed with exception.")) $
    withGithubApiSemaphore (githubWithLogging (commitR owner name commitSha)) >>= \case
      Left err -> atomically $ writeTVar commitVar (Errored (show err))
      Right detailedCommit -> atomically $ writeTVar commitVar (Fetched detailedCommit)

fetchIssue :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> IssueNumber -> TVar (Fetchable Issue) -> m ()
fetchIssue owner name issueNumber issueVar = do
  bracketOnError_ (atomically $ markFetching issueVar)
                  (atomically $ writeTVar issueVar (Errored "Issue fetch failed with exception.")) $
    withGithubApiSemaphore (githubWithLogging (issueR owner name issueNumber)) >>= \case
      Left err -> atomically $ writeTVar issueVar (Errored (show err))
      Right x -> atomically $ writeTVar issueVar (Fetched x)

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
          (githubWithLogging' baseContext
            (commentsR owner name (IssueNumber issueNumber) FetchAll))

  let fetchEvents =
        withGithubApiSemaphore' (requestSemaphore baseContext)
          (githubWithLogging' baseContext
            (eventsForIssueR owner name (GitHub.mkId (Proxy :: Proxy Issue) issueNumber) FetchAll))

  (commentsResult, eventsResult) <- concurrently fetchComments fetchEvents

  case (commentsResult, eventsResult) of
    (Right comments, Right events) -> do
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
    withGithubApiSemaphore (githubWithLogging (jobsForWorkflowRunR owner name workflowRunId FetchAll)) >>= \case
      Left err -> do
        -- traceM [i|Error fetching workflow jobs: #{err}|]
        atomically $ writeTVar _state (Errored (show err))
      Right wtc@(WithTotalCount results _totalCount) -> atomically $ do
        writeTVar _state (Fetched wtc)

        -- Preserve existing job nodes and their toggle state
        existingChildren <- readTVar _children

        -- Build a map of existing jobs by their IDs
        existingJobsMap <- foldM (\acc node -> case node of
          SingleJobNode (EntityData {_state=jobState}) -> do
            readTVar jobState >>= \case
              Fetched (existingJob, _) -> return $ Map.insert (jobId existingJob) node acc
              Fetching (Just (existingJob, _)) -> return $ Map.insert (jobId existingJob) node acc
              _ -> return acc
          ) (Map.empty :: Map.Map (Id Job) (Node Variable SingleJobT)) existingChildren

        let jobSortKeyFn = untagName . jobName

        jobNamesAndNodes <- forM (V.toList results) $ \job@(Job {jobId}) -> do
          case Map.lookup jobId existingJobsMap of
            Just existingNode@(SingleJobNode existingEntityData) -> do
              -- Update existing node's job data while preserving toggle state
              let EntityData {_state = jobState} = existingEntityData
              currentState <- readTVar jobState
              case currentState of
                Fetched (_, logGroups) -> writeTVar jobState (Fetched (job, logGroups))
                Fetching (Just (_, logGroups)) -> writeTVar jobState (Fetching (Just (job, logGroups)))
                _ -> writeTVar jobState (Fetched (job, []))
              return (jobSortKeyFn job, existingNode)
            Nothing -> do
              -- Create new node for new jobs
              entityData <- makeEmptyElem bc () "" (_depth + 1)
              let EntityData {_state=jobState} = entityData
              writeTVar jobState (Fetched (job, []))
              return (jobSortKeyFn job, SingleJobNode entityData)

        -- TODO: do pagination for these jobs? The web UI doesn't seem to...

        -- Sort by name
        writeTVar _children $ jobNamesAndNodes
          & L.sortBy (comparing fst)
          & fmap snd


fetchJobLogs :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Job -> Node Variable SingleJobT -> m ()
fetchJobLogs owner name (Job {jobId, jobSteps}) (SingleJobNode (EntityData {..})) = do
  bc@(BaseContext {auth, manager}) <- ask
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
  withGithubApiSemaphore (githubWithLoggingResponse (jobR owner name jobId)) >>= \case
    Left _err -> return () -- Silently fail for health checks
    Right (responseBody -> updatedJob) -> liftIO $ atomically $ do
      currentState <- readTVar _state
      case currentState of
        Fetched (_, logGroups) -> writeTVar _state (Fetched (updatedJob, logGroups))
        Fetching (Just (_, logGroups)) -> writeTVar _state (Fetching (Just (updatedJob, logGroups)))
        Fetching Nothing -> writeTVar _state (Fetching (Just (updatedJob, [])))
        _ -> writeTVar _state (Fetched (updatedJob, []))

-- * Util

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

-- * New GitHub REST API branch fetching

-- | Fetch branches using GitHub's internal REST API endpoints
fetchGitHubBranches :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Text -> Int -> TVar (Fetchable GitHubBranchesPayload) -> m ()
fetchGitHubBranches owner name branchType page responseVar = do
  bc@(BaseContext {auth, manager}) <- ask
  bracketOnError_ (atomically $ markFetching responseVar)
                  (atomically $ writeTVar responseVar (Errored "GitHub branches fetch failed with exception.")) $ do

    -- Construct the GitHub internal API URL
    let url = "https://github.com/" <> toPathPart owner <> "/" <> toPathPart name <> "/branches/" <> branchType
    let urlWithPage = if page > 1 then url <> "?page=" <> T.pack (show page) else url

    logToModal bc LevelDebug ("fetchGitHubBranches: " <> urlWithPage) Nothing

    -- Make the HTTP request with API semaphore
    (response, duration) <- withGithubApiSemaphore $ do
      -- Create HTTP request
      request <- liftIO $ parseRequest (T.unpack urlWithPage)
      let authRequest = case auth of
            OAuth token -> request { requestHeaders = [("Authorization", "token " <> token)] <> requestHeaders request }
            _ -> request
      let finalRequest = authRequest { requestHeaders = [("Accept", "application/json")] <> requestHeaders authRequest }

      startTime <- liftIO getCurrentTime
      response <- liftIO $ httpLbs finalRequest manager
      endTime <- liftIO getCurrentTime
      let duration = diffUTCTime endTime startTime
      return (response, duration)

    logToModal bc LevelDebug ("response body: " <> show (responseBody response)) Nothing

    case eitherDecode' (responseBody response) of
      Left err -> do
        -- Try parsing as the wrapped response format
        case eitherDecode' (responseBody response) :: Either String GitHubBranchesResponse of
          Left err2 -> do
            liftIO $ logToModal bc LevelError ("GitHub branches parse error: " <> T.pack err <> " | " <> T.pack err2) (Just duration)
            liftIO $ atomically $ writeTVar responseVar (Errored $ "Parse error: " <> T.pack err)
          Right wrappedResponse -> do
            liftIO $ logToModal bc LevelInfo ("GitHub branches fetched: " <> T.pack (show (length (gitHubBranchesPayloadBranches (gitHubBranchesResponsePayload wrappedResponse)))) <> " branches") (Just duration)
            liftIO $ atomically $ writeTVar responseVar (Fetched (gitHubBranchesResponsePayload wrappedResponse))
      Right branchesPayload -> do
        liftIO $ logToModal bc LevelInfo ("GitHub branches fetched: " <> T.pack (show (length (gitHubBranchesPayloadBranches branchesPayload))) <> " branches") (Just duration)
        liftIO $ atomically $ writeTVar responseVar (Fetched branchesPayload)

fetchGitHubYourBranches :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Int -> TVar (Fetchable GitHubBranchesPayload) -> m ()
fetchGitHubYourBranches owner name = fetchGitHubBranches owner name "yours"

fetchGitHubActiveBranches :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Int -> TVar (Fetchable GitHubBranchesPayload) -> m ()
fetchGitHubActiveBranches owner name = fetchGitHubBranches owner name "active"

fetchGitHubStaleBranches :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Int -> TVar (Fetchable GitHubBranchesPayload) -> m ()
fetchGitHubStaleBranches owner name = fetchGitHubBranches owner name "stale"
