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
-- import Sauron.HealthCheck.Workflow (startWorkflowHealthCheckIfNeeded)
import Sauron.Types
import UnliftIO.Async


withScroll :: AppState -> (forall s. ViewportScroll ClickableName -> EventM n s ()) -> EventM n AppState ()
withScroll s action = do
  case listSelectedElement (s ^. appMainList) of
    Just (_, _el@(SomeNode (getEntityData -> EntityData {..}))) -> action $ viewportScroll (InnerViewport [i|viewport_#{_ident}|])
    _ -> return ()

refresh :: (MonadIO m) => BaseContext -> Node Variable a -> NonEmpty (SomeNode Variable) -> m ()
refresh bc item@(HeadingNode (EntityData {_children})) _parents =
  readTVarIO _children >>= mapM_ (\(SomeNode child) -> refresh bc child ((SomeNode item) :| toList _parents))
refresh bc item@(RepoNode _) _parents =
  liftIO $ atomically (getExistentialChildrenWrapped item) >>= mapM_ (\(SomeNode childItem) -> refresh bc childItem ((SomeNode item) :| toList _parents))
refresh bc item@(PaginatedIssuesNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchIssues owner name item) bc
refresh bc item@(PaginatedPullsNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchPulls owner name item) bc
refresh bc item@(PaginatedWorkflowsNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchWorkflows owner name item) bc
refresh bc item@(PaginatedBranchesNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchBranches owner name item) bc
refresh bc item@(PaginatedReposNode (EntityData {_static=userLogin})) _parents =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchRepos userLogin item) bc
refresh bc (SingleIssueNode (EntityData {_static=issue, _state})) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchIssueComments owner name (issueNumber issue) _state) bc
refresh bc (SinglePullNode (EntityData {_static=pull, _state})) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchPullComments owner name (issueNumber pull) _state) bc
refresh bc item@(SingleWorkflowNode (EntityData {_static=workflowRun})) _parents@(findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) = do
  liftIO $ void $ async $ liftIO $ flip runReaderT bc $ do
    fetchWorkflowJobs owner name (workflowRunWorkflowRunId workflowRun) item
    -- liftIO $ void $ startWorkflowHealthCheckIfNeeded bc item parents
refresh bc item@(SingleJobNode (EntityData {_static=job})) parents@(findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) = do
  liftIO $ void $ async $ liftIO $ flip runReaderT bc $ do
    fetchJobLogs owner name job item
    liftIO $ void $ startJobHealthCheckIfNeeded bc item parents
refresh bc item@(SingleBranchNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchBranchCommits owner name item) bc
refresh _ _ _ = return ()

refreshAll :: (
  MonadReader BaseContext m, MonadIO m
  ) => V.Vector (SomeNode Variable) -> m ()
refreshAll elems = do
  baseContext <- ask
  allRepos <- liftIO $ collectAllRepos (V.toList elems)

  liftIO $ flip runReaderT baseContext $
    void $ async $ forConcurrently allRepos $ \case
      RepoNode (EntityData {_static=(owner, name), _state}) -> do
        fetchRepo owner name _state
        -- TODO: clear issues, workflows, etc. and re-fetch for open repos?

  where
    collectAllRepos :: [SomeNode Variable] -> IO [Node Variable RepoT]
    collectAllRepos = fmap concat . mapM collectFromNode
      where
        collectFromNode :: SomeNode Variable -> IO [Node Variable RepoT]
        collectFromNode (SomeNode item@(RepoNode {})) = return [item]
        collectFromNode (SomeNode item@(HeadingNode {})) = do
          children <- getExistentialChildren item
          collectAllRepos children
        collectFromNode (SomeNode _item) =
          -- Other node types don't contain repos as children
          pure []
