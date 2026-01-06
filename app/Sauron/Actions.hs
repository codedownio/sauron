{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Sauron.Actions (
  openBrowserToUrl

  , refreshVisibleLines
  , refreshLine
  , refreshSelected
  , refreshOnZoom
  , onOpen
  ) where

import Control.Monad.IO.Class
import qualified Data.Vector as V
import GitHub
import Relude
import Sauron.Actions.Util (findRepoParent, findJobParent, openBrowserToUrl)
import Sauron.Fetch.Branch
import Sauron.Fetch.Issue
import Sauron.Fetch.Job
import Sauron.Fetch.Notification
import Sauron.Fetch.Pull
import Sauron.Fetch.Repo
import Sauron.Fetch.Workflow
import Sauron.HealthCheck.Job (startJobHealthCheckIfNeeded)
import Sauron.HealthCheck.Workflow (startWorkflowHealthCheckIfNeeded)
import Sauron.Types
import UnliftIO.Async


refreshSelected :: (MonadIO m) => BaseContext -> Node Variable a -> NonEmpty (SomeNode Variable) -> m ()
refreshSelected _ _ _ = return ()

refreshOnZoom :: (MonadIO m) => BaseContext -> Node Variable a -> NonEmpty (SomeNode Variable) -> m ()
refreshOnZoom _ _ _ = return ()

refreshLine :: (MonadIO m) => BaseContext -> Node Variable a -> NonEmpty (SomeNode Variable) -> m ()
refreshLine bc (RepoNode (EntityData {_static=(owner, name), _state})) _parents =
  liftIO $ void $ async $ flip runReaderT bc $ fetchRepo owner name _state
refreshLine bc item@(PaginatedIssuesNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchIssues owner name item) bc
refreshLine bc item@(PaginatedPullsNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchPulls owner name item) bc
refreshLine bc item@(PaginatedWorkflowsNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchWorkflows owner name item) bc
refreshLine bc item@(PaginatedBranchesNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchBranches owner name item) bc
refreshLine bc item@(PaginatedYourBranchesNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name), _state}))) =
  withRepoDefaultBranch _state $ \defaultBranch -> liftIO $ void $ async $ runReaderT (fetchYourBranches owner name defaultBranch item) bc
refreshLine bc item@(PaginatedActiveBranchesNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name), _state}))) =
  withRepoDefaultBranch _state $ \defaultBranch -> liftIO $ void $ async $ runReaderT (fetchActiveBranches owner name defaultBranch item) bc
refreshLine bc item@(PaginatedStaleBranchesNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name), _state}))) =
  withRepoDefaultBranch _state $ \defaultBranch -> liftIO $ void $ async $ runReaderT (fetchStaleBranches owner name defaultBranch item) bc
refreshLine _ _ _ = return ()

-- refreshLine bc (SingleIssueNode (EntityData {_static=issue, _state})) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
--   liftIO $ void $ async $ liftIO $ runReaderT (fetchIssueComments owner name (issueNumber issue) _state) bc
-- refreshLine bc (SinglePullNode (EntityData {_static=pull, _state})) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
--   liftIO $ void $ async $ liftIO $ runReaderT (fetchPullComments owner name (issueNumber pull) _state) bc
-- refreshLine bc item@(SingleJobNode (EntityData {_state, _static=jobId})) parents@(findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) = do
--   liftIO $ void $ async $ liftIO $ flip runReaderT bc $ do
--     fetchJob owner name jobId item
--     liftIO $ void $ startJobHealthCheckIfNeeded bc item parents
-- refreshLine bc (SingleCommitNode (EntityData {_static=commit, _state})) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
--   liftIO $ void $ async $ liftIO $ runReaderT (fetchCommitDetails owner name (commitSha commit) _state) bc
-- refreshLine bc (JobLogGroupNode (EntityData {_state})) parents = do
--   case findJobParent (toList parents) of
--     Just jobNode@(SingleJobNode (EntityData {_state=jobState})) ->
--       liftIO $ void $ async $ liftIO $ flip runReaderT bc $ do
--         currentLogState <- readTVarIO _state
--         case currentLogState of
--           NotFetched -> do
--             -- Transition to Fetching state before starting fetch
--             atomically $ writeTVar _state (Fetching Nothing)
--             jobState' <- readTVarIO jobState
--             case jobState' of
--               Fetched job -> do
--                 -- Find the repo parent for owner/name
--                 case findRepoParent parents of
--                   Just (RepoNode (EntityData {_static=(owner, name)})) ->
--                     fetchJobLogsAndReplaceChildren owner name job jobNode
--                   _ -> return ()
--               _ -> return ()
--           _ -> return ()
--     Nothing -> return ()
-- refreshLine _ _ _ = return ()

-- refreshChildren :: (MonadIO m) => BaseContext -> Node Variable a -> NonEmpty (SomeNode Variable) -> m ()
-- refreshChildren _bc (HeadingNode (EntityData {_children})) _parents = return ()
-- refreshChildren bc item@(OverallBranchesNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
--   liftIO $ void $ async $ liftIO $ runReaderT (getOverallBranches owner name item) bc
-- refreshChildren bc item@(PaginatedYourBranchesNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name), _state}))) =
--   withRepoDefaultBranch _state $ \defaultBranch -> liftIO $ void $ async $ runReaderT (fetchYourBranches owner name defaultBranch item) bc
-- refreshChildren bc item@(PaginatedActiveBranchesNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name), _state}))) =
--   withRepoDefaultBranch _state $ \defaultBranch -> liftIO $ void $ async $ runReaderT (fetchActiveBranches owner name defaultBranch item) bc
-- refreshChildren bc item@(PaginatedStaleBranchesNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name), _state}))) =
--   withRepoDefaultBranch _state $ \defaultBranch -> liftIO $ void $ async $ runReaderT (fetchStaleBranches owner name defaultBranch item) bc
-- refreshChildren bc item@(PaginatedNotificationsNode _) _parents =
--   liftIO $ void $ async $ liftIO $ runReaderT (fetchNotifications item) bc
-- refreshChildren bc item@(PaginatedReposNode (EntityData {})) _parents =
--   liftIO $ void $ async $ liftIO $ runReaderT (fetchRepos item) bc
-- refreshChildren bc item@(SingleWorkflowNode (EntityData {_static=workflowRun})) parents@(findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) = do
--   liftIO $ void $ async $ liftIO $ flip runReaderT bc $ do
--     fetchWorkflowJobs owner name (workflowRunWorkflowRunId workflowRun) item
--     liftIO $ void $ startWorkflowHealthCheckIfNeeded bc item parents
-- refreshChildren bc item@(SingleBranchNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
--   liftIO $ void $ async $ liftIO $ runReaderT (fetchBranchCommits owner name item) bc
-- refreshChildren bc item@(SingleBranchWithInfoNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
--   liftIO $ void $ async $ liftIO $ runReaderT (fetchBranchWithInfoCommits owner name item) bc
-- refreshChildren _ _ _ = return ()

refreshVisibleLines :: (
  MonadReader BaseContext m, MonadIO m
  ) => V.Vector (SomeNode Variable) -> m ()
refreshVisibleLines elems = do
  baseContext <- ask
  refreshVisibleLines' baseContext [] (V.toList elems)

refreshVisibleLines' :: MonadIO m => BaseContext -> [SomeNode Variable] -> [SomeNode Variable] -> m ()
refreshVisibleLines' baseContext parents nodes = do
  forM_ nodes $ \someNode@(SomeNode node) -> do
    -- TODO: we used to check if the state is NotFetched before doing this refresh
    refreshLine baseContext node (someNode :| parents)

    whenM (readTVarIO (_toggled (getEntityData node))) $
      atomically (getExistentialChildrenWrapped node)
        >>= refreshVisibleLines' baseContext (someNode : parents)

onOpen :: (MonadIO m) => BaseContext -> Node Variable a -> NonEmpty (SomeNode Variable) -> m ()
-- Container nodes that just organize other nodes - do nothing
onOpen _bc (HeadingNode _) _parents = return ()
onOpen bc item@(RepoNode (EntityData {_children})) parents = do
  readTVarIO _children >>= mapM_ (\(SomeNode x) -> refreshLine bc x (SomeNode item :| toList parents))

-- Paginated nodes - fetch their lists when opened
onOpen bc item@(PaginatedIssuesNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchIssues owner name item) bc
onOpen bc item@(PaginatedPullsNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchPulls owner name item) bc
onOpen bc item@(PaginatedWorkflowsNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchWorkflows owner name item) bc
onOpen bc item@(PaginatedBranchesNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchBranches owner name item) bc
onOpen bc item@(PaginatedYourBranchesNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name), _state}))) =
  withRepoDefaultBranch _state $ \defaultBranch -> liftIO $ void $ async $ runReaderT (fetchYourBranches owner name defaultBranch item) bc
onOpen bc item@(PaginatedActiveBranchesNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name), _state}))) =
  withRepoDefaultBranch _state $ \defaultBranch -> liftIO $ void $ async $ runReaderT (fetchActiveBranches owner name defaultBranch item) bc
onOpen bc item@(PaginatedStaleBranchesNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name), _state}))) =
  withRepoDefaultBranch _state $ \defaultBranch -> liftIO $ void $ async $ runReaderT (fetchStaleBranches owner name defaultBranch item) bc
onOpen bc item@(PaginatedNotificationsNode _) _parents =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchNotifications item) bc
onOpen bc item@(PaginatedReposNode _) _parents =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchRepos item) bc

-- Nodes that fetch their children when opened
onOpen bc item@(SingleWorkflowNode (EntityData {_static=workflowRun})) parents@(findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) = do
  liftIO $ void $ async $ liftIO $ flip runReaderT bc $ do
    fetchWorkflowJobs owner name (workflowRunWorkflowRunId workflowRun) item
    liftIO $ void $ startWorkflowHealthCheckIfNeeded bc item parents
onOpen bc item@(SingleBranchNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchBranchCommits owner name item) bc
onOpen bc item@(SingleBranchWithInfoNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchBranchWithInfoCommits owner name item) bc

-- Content nodes that fetch their details when explicitly opened
onOpen bc (SingleIssueNode (EntityData {_static=issue, _state})) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchIssueComments owner name (issueNumber issue) _state) bc
onOpen bc (SinglePullNode (EntityData {_static=pull, _state})) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchPullComments owner name (issueNumber pull) _state) bc
onOpen bc (SingleCommitNode (EntityData {_static=commit, _state})) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchCommitDetails owner name (commitSha commit) _state) bc

-- Job-related nodes that handle their own complex opening logic
onOpen bc item@(SingleJobNode (EntityData {_state, _static=jobId})) parents@(findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) = do
  liftIO $ void $ async $ liftIO $ flip runReaderT bc $ do
    fetchJob owner name jobId item
    liftIO $ void $ startJobHealthCheckIfNeeded bc item parents
onOpen bc (JobLogGroupNode (EntityData {_state})) parents = do
  case findJobParent (toList parents) of
    Just jobNode@(SingleJobNode (EntityData {_state=jobState})) ->
      liftIO $ void $ async $ liftIO $ flip runReaderT bc $ do
        readTVarIO _state >>= \case
          NotFetched -> do
            -- Transition to Fetching state before starting fetch
            atomically $ writeTVar _state (Fetching Nothing)
            readTVarIO jobState >>= \case
              Fetched job -> do
                -- Find the repo parent for owner/name
                case findRepoParent parents of
                  Just (RepoNode (EntityData {_static=(owner, name)})) ->
                    fetchJobLogsAndReplaceChildren owner name job jobNode
                  _ -> return ()
              _ -> return ()
          _ -> return ()
    Nothing -> return ()

-- Truly lazy nodes
onOpen _bc (SingleNotificationNode _) _parents = return ()

-- Fallback
onOpen _ _ _ = return ()


withRepoDefaultBranch :: MonadIO m => TVar (Fetchable Repo) -> (Maybe Text -> m ()) -> m ()
withRepoDefaultBranch fetchableVar action = readTVarIO fetchableVar >>= \case
  Fetched (Repo {..}) -> action repoDefaultBranch
  Fetching (Just (Repo {..})) -> action repoDefaultBranch
  _ -> return ()
