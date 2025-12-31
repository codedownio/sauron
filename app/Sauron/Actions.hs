{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Sauron.Actions (
  openBrowserToUrl

  , withScroll

  , refresh
  , refreshAll
  , refreshVisibleNodes
  ) where

import Brick as B
import Brick.Widgets.List
import Control.Monad.IO.Class
import Data.String.Interpolate
import qualified Data.Vector as V
import GitHub
import Lens.Micro
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


withScroll :: AppState -> (forall s. ViewportScroll ClickableName -> EventM n s ()) -> EventM n AppState ()
withScroll s action = do
  case listSelectedElement (s ^. appMainList) of
    Just (_, _el@(SomeNode (getEntityData -> EntityData {..}))) -> action $ viewportScroll (InnerViewport [i|viewport_#{_ident}|])
    _ -> return ()

refresh :: (MonadIO m) => BaseContext -> Node Variable a -> NonEmpty (SomeNode Variable) -> m ()
refresh _bc _item@(HeadingNode (EntityData {_children})) _parents = do
  return ()
  -- readTVarIO _children >>= mapM_ (\(SomeNode child) -> refresh bc child ((SomeNode item) :| toList _parents))
refresh bc (RepoNode (EntityData {_static=(owner, name), _state})) _parents = do
  liftIO $ void $ async $ flip runReaderT bc $ fetchRepo owner name _state
refresh bc item@(PaginatedIssuesNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchIssues owner name item) bc
refresh bc item@(PaginatedPullsNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchPulls owner name item) bc
refresh bc item@(PaginatedWorkflowsNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchWorkflows owner name item) bc
refresh bc item@(PaginatedBranchesNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchBranches owner name item) bc
refresh bc item@(OverallBranchesNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ void $ async $ liftIO $ runReaderT (getOverallBranches owner name item) bc
refresh bc item@(PaginatedYourBranchesNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name), _state}))) =
  readTVarIO _state >>= \case
    Fetched (Repo {..}) -> liftIO $ void $ async $ liftIO $ runReaderT (fetchYourBranches owner name repoDefaultBranch item) bc
    Fetching (Just (Repo {..})) -> liftIO $ void $ async $ liftIO $ runReaderT (fetchYourBranches owner name repoDefaultBranch item) bc
    _ -> return ()
refresh bc item@(PaginatedActiveBranchesNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name), _state}))) =
  readTVarIO _state >>= \case
    Fetched (Repo {..}) -> liftIO $ void $ async $ liftIO $ runReaderT (fetchActiveBranches owner name repoDefaultBranch item) bc
    Fetching (Just (Repo {..})) -> liftIO $ void $ async $ liftIO $ runReaderT (fetchActiveBranches owner name repoDefaultBranch item) bc
    _ -> return ()
refresh bc item@(PaginatedStaleBranchesNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name), _state}))) =
  readTVarIO _state >>= \case
    Fetched (Repo {..}) -> liftIO $ void $ async $ liftIO $ runReaderT (fetchStaleBranches owner name repoDefaultBranch item) bc
    Fetching (Just (Repo {..})) -> liftIO $ void $ async $ liftIO $ runReaderT (fetchStaleBranches owner name repoDefaultBranch item) bc
    _ -> return ()
refresh bc item@(PaginatedNotificationsNode _) _parents =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchNotifications item) bc
refresh bc item@(PaginatedReposNode (EntityData {})) _parents =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchRepos item) bc
refresh bc (SingleIssueNode (EntityData {_static=issue, _state})) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchIssueComments owner name (issueNumber issue) _state) bc
refresh bc (SinglePullNode (EntityData {_static=pull, _state})) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchPullComments owner name (issueNumber pull) _state) bc
refresh bc item@(SingleWorkflowNode (EntityData {_static=workflowRun})) parents@(findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) = do
  liftIO $ void $ async $ liftIO $ flip runReaderT bc $ do
    fetchWorkflowJobs owner name (workflowRunWorkflowRunId workflowRun) item
    liftIO $ void $ startWorkflowHealthCheckIfNeeded bc item parents
refresh bc item@(SingleJobNode (EntityData {_state, _static=jobId})) parents@(findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) = do
  liftIO $ void $ async $ liftIO $ flip runReaderT bc $ do
    fetchJob owner name jobId item
    liftIO $ void $ startJobHealthCheckIfNeeded bc item parents
refresh bc (JobLogGroupNode (EntityData {_state})) parents = do
  case findJobParent (toList parents) of
    Just jobNode@(SingleJobNode (EntityData {_state=jobState})) ->
      liftIO $ void $ async $ liftIO $ flip runReaderT bc $ do
        currentLogState <- readTVarIO _state
        case currentLogState of
          NotFetched -> do
            -- Transition to Fetching state before starting fetch
            atomically $ writeTVar _state (Fetching Nothing)
            jobState' <- readTVarIO jobState
            case jobState' of
              Fetched job -> do
                -- Find the repo parent for owner/name
                case findRepoParent parents of
                  Just (RepoNode (EntityData {_static=(owner, name)})) ->
                    fetchJobLogsAndReplaceChildren owner name job jobNode
                  _ -> return ()
              _ -> return ()
          _ -> return ()
    Nothing -> return ()
refresh bc item@(SingleBranchNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchBranchCommits owner name item) bc
refresh bc item@(SingleBranchWithInfoNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchBranchWithInfoCommits owner name item) bc
refresh bc (SingleCommitNode (EntityData {_static=commit, _state})) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchCommitDetails owner name (commitSha commit) _state) bc
refresh _ (SingleNotificationNode _) _ = return ()
refresh _ _ _ = return ()

refreshAll :: (
  MonadReader BaseContext m, MonadIO m
  ) => V.Vector (SomeNode Variable) -> m ()
refreshAll elems = do
  baseContext <- ask
  refreshVisibleNodes baseContext [] (V.toList elems)

refreshVisibleNodes :: MonadIO m => BaseContext -> [SomeNode Variable] -> [SomeNode Variable] -> m ()
refreshVisibleNodes baseContext parents nodes = do
  forM_ nodes $ \someNode@(SomeNode node) -> do
    stateValue <- readTVarIO (_state (getEntityData node))
    unless (isFetchingOrFetched stateValue) $
      refresh baseContext node (someNode :| parents)

    whenM (readTVarIO (_toggled (getEntityData node))) $
      atomically (getExistentialChildrenWrapped node)
        >>= refreshVisibleNodes baseContext (someNode : parents)

  where
    isFetchingOrFetched :: Fetchable a -> Bool
    isFetchingOrFetched (Fetched {}) = True
    isFetchingOrFetched (Fetching {}) = True
    isFetchingOrFetched _ = False
