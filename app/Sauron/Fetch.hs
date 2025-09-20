{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Sauron.Fetch (
  fetchRepo

  , fetchWorkflows

  , fetchPulls
  , fetchPullComments

  , fetchIssues
  , fetchIssue
  , fetchIssueComments

  , fetchWorkflowJobs
  , fetchJobLogs
  ) where

import Control.Exception.Safe (bracketOnError_)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Vector as V
import GitHub
import Network.HTTP.Conduit
import qualified Network.URI as URI
import Relude
import Sauron.Actions.Util
import Sauron.Fetch.Core
import Sauron.Fetch.ParseJobLogs
import Sauron.Types


fetchRepo :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> TVar (Fetchable Repo) -> m ()
fetchRepo owner name repoVar = do
  BaseContext {auth} <- ask
  bracketOnError_ (atomically $ writeTVar repoVar Fetching)
                  (atomically $ writeTVar repoVar (Errored "Repo fetch failed with exception.")) $
    withGithubApiSemaphore (liftIO $ github auth (repositoryR owner name)) >>= \case
      Left err -> atomically $ writeTVar repoVar (Errored (show err))
      Right x -> atomically $ writeTVar repoVar (Fetched x)

fetchIssues :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m, MonadFail m
  ) => Name Owner -> Name Repo -> TVar MainListElemVariable -> m ()
fetchIssues owner name issuesVar = do
  extraTerms <- readTVarIO issuesVar >>= (readTVarIO . _search) >>= \case
    SearchNone -> pure []
    SearchText t -> pure $ T.words t
  let fullQuery = T.intercalate "+" ([i|repo:#{untagName owner}/#{untagName name}|] : extraTerms)

  bc <- ask
  MainListElemItem {..} <- readTVarIO issuesVar

  fetchPaginated'' (searchIssuesR fullQuery) _pageInfo (writeTVar _state) $ \case
    Left err -> do
      writeTVar _state (Errored (show err))
      writeTVar _children []
    Right (sr@(SearchResult _totalCount results), newPageInfo) -> do
      writeTVar _pageInfo newPageInfo
      writeTVar _state (Fetched (PaginatedItemsIssues sr))
      (writeTVar _children =<<) $ forM (V.toList results) $ \issue@(Issue {..}) ->
        makeEmptyElem bc (SingleIssue issue) ("/issue/" <> show issueNumber) (_depth + 1)

fetchPulls :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m, MonadFail m
  ) => Name Owner -> Name Repo -> TVar MainListElemVariable -> m ()
fetchPulls owner name selfVar = do
  extraTerms <- readTVarIO selfVar >>= (readTVarIO . _search) >>= \case
    SearchNone -> pure []
    SearchText t -> pure $ T.words t
  let fullQuery = T.intercalate "+" ([i|repo:#{untagName owner}/#{untagName name}|] : extraTerms)

  bc <- ask
  MainListElemItem {..} <- readTVarIO selfVar

  fetchPaginated'' (searchIssuesR fullQuery) _pageInfo (writeTVar _state) $ \case
    Left err -> do
      writeTVar _state (Errored (show err))
      writeTVar _children []
    Right (sr@(SearchResult _totalCount results), newPageInfo) -> do
      writeTVar _pageInfo newPageInfo
      writeTVar _state (Fetched (PaginatedItemsPulls sr))
      (writeTVar _children =<<) $ forM (V.toList results) $ \issue@(Issue {..}) ->
        makeEmptyElem bc (SinglePull issue) ("/pull/" <> show issueNumber) (_depth + 1)

fetchIssue :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> IssueNumber -> TVar (Fetchable Issue) -> m ()
fetchIssue owner name issueNumber issueVar = do
  BaseContext {auth, manager} <- ask
  bracketOnError_ (atomically $ writeTVar issueVar Fetching)
                  (atomically $ writeTVar issueVar (Errored "Workflows fetch failed with exception.")) $
    withGithubApiSemaphore (liftIO $ executeRequestWithMgrAndRes manager auth (issueR owner name issueNumber)) >>= \case
      Left err -> atomically $ do
        writeTVar issueVar (Errored (show err))
      Right x -> atomically $ do
        writeTVar issueVar (Fetched (responseBody x))

fetchWorkflows :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m, MonadFail m
  ) => Name Owner -> Name Repo -> TVar MainListElemVariable -> m ()
fetchWorkflows owner name selfVar = do
  bc <- ask
  MainListElemItem {..} <- readTVarIO selfVar

  fetchPaginated'' (workflowRunsR owner name mempty) _pageInfo (writeTVar _state) $ \case
    Left err -> do
      writeTVar _state (Errored (show err))
      writeTVar _children []
    Right (wtc@(WithTotalCount results _totalCount), newPageInfo) -> do
      writeTVar _pageInfo newPageInfo
      writeTVar _state (Fetched (PaginatedItemsWorkflows wtc))
      (writeTVar _children =<<) $ forM (V.toList results) $ \workflow@(WorkflowRun {}) ->
        makeEmptyElem bc (SingleWorkflow workflow) "" (_depth + 1)

fetchIssueComments :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> IssueNumber -> TVar (Fetchable NodeState) -> m ()
fetchIssueComments owner name issueNumber inner = do
  BaseContext {auth, manager} <- ask
  bracketOnError_ (atomically $ writeTVar inner Fetching)
                  (atomically $ writeTVar inner (Errored "Issue comments fetch failed with exception.")) $
    withGithubApiSemaphore (liftIO $ executeRequestWithMgrAndRes manager auth (commentsR owner name issueNumber FetchAll)) >>= \case
      Left err -> atomically $ writeTVar inner (Errored (show err))
      Right v -> atomically $ writeTVar inner (Fetched (PaginatedItemIssue (responseBody v)))

fetchPullComments :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> IssueNumber -> TVar (Fetchable NodeState) -> m ()
fetchPullComments owner name issueNumber inner = do
  BaseContext {auth, manager} <- ask
  bracketOnError_ (atomically $ writeTVar inner Fetching)
                  (atomically $ writeTVar inner (Errored "Pull comments fetch failed with exception.")) $
    -- pullRequestCommentsR returns comments on the "unified diff"
    -- there are also "commit comments" and "issue comments".
    -- The last one are the most common on PRs, so we use commentsR
    withGithubApiSemaphore (liftIO $ executeRequestWithMgrAndRes manager auth (commentsR owner name issueNumber FetchAll)) >>= \case
      Left err -> atomically $ writeTVar inner (Errored (show err))
      Right v -> atomically $ writeTVar inner (Fetched (PaginatedItemPull (responseBody v)))

fetchWorkflowJobs :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Id WorkflowRun -> MainListElemVariable -> m ()
fetchWorkflowJobs owner name workflowRunId (MainListElemItem {..}) = do
  bc@(BaseContext {auth, manager}) <- ask
  bracketOnError_ (atomically $ writeTVar _state Fetching)
                  (atomically $ writeTVar _state (Errored "Workflow jobs fetch failed with exception.")) $
    -- pullRequestCommentsR returns comments on the "unified diff"
    -- there are also "commit comments" and "issue comments".
    -- The last one are the most common on PRs, so we use commentsR
    withGithubApiSemaphore (liftIO $ executeRequestWithMgrAndRes manager auth (jobsForWorkflowRunR owner name workflowRunId FetchAll)) >>= \case
      Left err -> do
        -- traceM [i|Error fetching workflow jobs: #{err}|]
        atomically $ writeTVar _state (Errored (show err))
      Right (responseBody -> wtc@(WithTotalCount results _totalCount)) -> atomically $ do
        writeTVar _state (Fetched (PaginatedItemsJobs wtc))
        (writeTVar _children =<<) $ forM (V.toList results) $ \job@(Job {}) -> do
          makeEmptyElem bc (SingleJob job) "" (_depth + 1)
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
fetchWorkflowJobs _owner _name _workflowRunId _ = return ()

fetchJobLogs :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Job -> MainListElemVariable -> m ()
fetchJobLogs owner name (Job {jobId}) (MainListElemItem {..}) = do
  BaseContext {auth, manager} <- ask
  bracketOnError_ (atomically $ writeTVar _state Fetching)
                  (atomically $ writeTVar _state (Errored "Job logs fetch failed with exception.")) $ do
    -- First, get the download URI
    withGithubApiSemaphore (liftIO $ executeRequestWithMgrAndRes manager auth (downloadJobLogsR owner name jobId)) >>= \case
      Left err -> atomically $ writeTVar _state (Errored (show err))
      Right response -> do
        let uri = responseBody response
        traceM [i|Jobs URI: #{uri}|]
        logs <- simpleHttp (URI.uriToString id uri "")

        let parsedLogs = parseJobLogs (T.splitOn "\n" (decodeUtf8 logs))
        traceM [i|parsedLogs: #{parsedLogs}|]

        bc <- ask
        children <- liftIO $ atomically $ mapM (createJobLogGroupChildren bc (_depth + 1)) parsedLogs

        atomically $ do
          writeTVar _state (Fetched (PaginatedItemJob parsedLogs))
          writeTVar _children children


fetchJobLogs _owner _name _ _ = return ()

-- * Util


makeEmptyElem :: BaseContext -> NodeType -> Text -> Int -> STM MainListElemVariable
makeEmptyElem (BaseContext {getIdentifierSTM}) typ' urlSuffix' depth' = do
  stateVar <- newTVar NotFetched
  ident' <- getIdentifierSTM
  toggledVar <- newTVar False
  childrenVar <- newTVar []
  searchVar <- newTVar $ SearchNone
  pageInfoVar <- newTVar emptyPageInfo
  return $ MainListElemItem {
    _typ = typ'
    , _state = stateVar

    , _urlSuffix = urlSuffix'

    , _toggled = toggledVar
    , _children = childrenVar

    , _search = searchVar
    , _pageInfo = pageInfoVar

    , _depth = depth'
    , _ident = ident'
}

createJobLogGroupChildren :: BaseContext -> Int -> JobLogGroup -> STM MainListElemVariable
createJobLogGroupChildren bc depth jobLogGroup = do
  let nodeType = JobLogGroupNode jobLogGroup
  stateVar <- newTVar (Fetched JobLogGroupState)
  ident' <- getIdentifierSTM bc
  toggledVar <- newTVar False
  searchVar <- newTVar SearchNone
  pageInfoVar <- newTVar emptyPageInfo

  childrenVar <- case jobLogGroup of
    JobLogLine _ _ -> newTVar []
    JobLogGroup _ _ children -> do
      childElems <- mapM (createJobLogGroupChildren bc (depth + 1)) children
      newTVar childElems

  return $ MainListElemItem {
    _typ = nodeType
    , _state = stateVar
    , _urlSuffix = ""
    , _toggled = toggledVar
    , _children = childrenVar
    , _search = searchVar
    , _pageInfo = pageInfoVar
    , _depth = depth
    , _ident = ident'
  }
