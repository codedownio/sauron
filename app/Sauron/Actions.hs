{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Sauron.Actions (
  openBrowserToUrl

  , withScroll

  , refresh
  , refreshAll
  ) where

import Brick as B
import Brick.Widgets.List
import Control.Monad.IO.Class
import Data.String.Interpolate
import qualified Data.Vector as V
import GitHub
import Lens.Micro
import Relude
import Sauron.Actions.Util (findRepoParent, openBrowserToUrl)
import Sauron.Fetch
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
refresh bc item@(RepoNode (EntityData {_static=(owner, name), _state})) _parents = do
  liftIO $ void $ async $ flip runReaderT bc $ fetchRepo owner name _state
  liftIO $ atomically (getExistentialChildrenWrapped item) >>= mapM_ (\(SomeNode childItem) -> refresh bc childItem (SomeNode item :| toList _parents))
refresh bc item@(PaginatedIssuesNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchIssues owner name item) bc
refresh bc item@(PaginatedPullsNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchPulls owner name item) bc
refresh bc item@(PaginatedWorkflowsNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchWorkflows owner name item) bc
refresh bc item@(PaginatedBranchesNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchBranches owner name item) bc
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
refresh bc item@(SingleJobNode (EntityData {_state})) parents@(findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) = do
  liftIO $ void $ async $ liftIO $ flip runReaderT bc $ do
    currentState <- readTVarIO _state
    case currentState of
      Fetched (job, _) -> fetchJobLogs owner name job item
      Fetching (Just (job, _)) -> fetchJobLogs owner name job item
      _ -> return ()
    liftIO $ void $ startJobHealthCheckIfNeeded bc item parents
refresh bc item@(SingleBranchNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchBranchCommits owner name item) bc
refresh bc (SingleCommitNode (EntityData {_static=commit, _state})) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchCommitDetails owner name (commitSha commit) _state) bc
refresh _ (SingleNotificationNode _) _ = return ()
refresh _ _ _ = return ()

refreshAll :: (
  MonadReader BaseContext m, MonadIO m
  ) => V.Vector (SomeNode Variable) -> m ()
refreshAll elems = do
  baseContext <- ask
  liftIO $ refreshVisibleNodes baseContext [] (V.toList elems)

  where
    refreshVisibleNodes :: BaseContext -> [SomeNode Variable] -> [SomeNode Variable] -> IO ()
    refreshVisibleNodes baseContext parents nodes = do
      forM_ nodes $ \someNode@(SomeNode node) -> do
        refresh baseContext node (someNode :| parents)

        whenM (readTVarIO (_toggled (getEntityData node))) $
          atomically (getExistentialChildrenWrapped node)
            >>= refreshVisibleNodes baseContext (someNode : parents)
