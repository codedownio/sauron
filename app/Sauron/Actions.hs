{-# LANGUAGE CPP #-}
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
import Sauron.Fetch
import Sauron.Types
import UnliftIO.Async
import UnliftIO.Process


#ifdef mingw32_HOST_OS
import System.Directory

openBrowserToUrl :: MonadIO m => String -> m ()
openBrowserToUrl url = do
  findExecutable "explorer.exe" >>= \case
    Just p -> void $ readCreateProcessWithExitCode (proc p [url]) ""
    Nothing -> return ()
#elif darwin_HOST_OS
openBrowserToUrl :: MonadIO m => String -> m ()
openBrowserToUrl url =
  void $ readCreateProcessWithExitCode (proc "open" [url]) ""
#else
openBrowserToUrl :: MonadIO m => String -> m ()
openBrowserToUrl url =
  void $ readCreateProcessWithExitCode (proc "xdg-open" [url]) ""
#endif

withScroll :: AppState -> (forall s. ViewportScroll ClickableName -> EventM n s ()) -> EventM n AppState ()
withScroll s action = do
  case listSelectedElement (s ^. appMainList) of
    Just (_, el) -> action $ viewportScroll (InnerViewport [i|viewport_#{_ident el}|])
    _ -> return ()

refresh :: (MonadIO m) => BaseContext -> MainListElemVariable -> NonEmpty MainListElemVariable -> m ()
refresh _ (MainListElemItem {_typ = HeadingNode _, ..}) _parents = return () -- Headings don't need refreshing
refresh bc (MainListElemRepo {_namespaceName=(owner, name), _issuesChild, _pullsChild, _workflowsChild}) _parents = liftIO $ do
  void $ async $ liftIO $ runReaderT (fetchIssues owner name _issuesChild) bc
  void $ async $ liftIO $ runReaderT (fetchPulls owner name _pullsChild) bc
  void $ async $ liftIO $ runReaderT (fetchWorkflows owner name _workflowsChild) bc

refresh bc (MainListElemItem {_typ=PaginatedIssues}) (findRepoParent -> Just (MainListElemRepo {_namespaceName=(owner, name), _issuesChild})) = liftIO $
   void $ async $ liftIO $ runReaderT (fetchIssues owner name _issuesChild) bc
refresh bc (MainListElemItem {_typ=PaginatedPulls}) (findRepoParent -> Just (MainListElemRepo {_namespaceName=(owner, name), _pullsChild})) = liftIO $
   void $ async $ liftIO $ runReaderT (fetchPulls owner name _pullsChild) bc
refresh bc (MainListElemItem {_typ=PaginatedWorkflows}) (findRepoParent -> Just (MainListElemRepo {_namespaceName=(owner, name), _workflowsChild})) = liftIO $
   void $ async $ liftIO $ runReaderT (fetchWorkflows owner name _workflowsChild) bc

refresh bc (MainListElemItem {_typ=(SingleIssue (Issue {..})), _state}) (findRepoParent -> Just (MainListElemRepo {_namespaceName=(owner, name)})) =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchIssueComments owner name issueNumber _state) bc
refresh bc (MainListElemItem {_typ=(SinglePull (Issue {..})), _state}) (findRepoParent -> Just (MainListElemRepo {_namespaceName=(owner, name)})) =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchIssueComments owner name issueNumber _state) bc
refresh bc item@(MainListElemItem {_typ=(SingleWorkflow (WorkflowRun {..})), _state}) (findRepoParent -> Just (MainListElemRepo {_namespaceName=(owner, name)})) =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchWorkflowJobs owner name workflowRunWorkflowRunId item) bc
refresh bc item@(MainListElemItem {_typ=(SingleJob job@(Job {})), _state}) (findRepoParent -> Just (MainListElemRepo {_namespaceName=(owner, name)})) =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchJobLogs owner name job item) bc

refresh _ _ _ = return ()

findRepoParent :: NonEmpty MainListElemVariable -> Maybe MainListElemVariable
findRepoParent elems = viaNonEmpty head [x | x@(MainListElemRepo {}) <- toList elems]

-- findWorkflowsParent :: NonEmpty MainListElemVariable -> Maybe MainListElemVariable
-- findWorkflowsParent elems = viaNonEmpty head [x | x@(MainListElemItem {_typ=PaginatedWorkflows}) <- toList elems]

-- getSelfVar :: MainListElemVariable -> NonEmpty MainListElemVariable -> Maybe (TVar MainListElemVariable)
-- getSelfVar (MainListElemItem {_typ=PaginatedIssues}) (findRepoParent -> Just r) = Just $ _issuesChild r
-- getSelfVar (MainListElemItem {_typ=PaginatedPulls}) (findRepoParent -> Just r) = Just $ _pullsChild r
-- getSelfVar (MainListElemItem {_typ=PaginatedWorkflows}) (findRepoParent -> Just r) = Just $ _workflowsChild r
-- -- getSelfVar (MainListElem {_typ=PaginatedIssues, ..}) (findRepoParent -> Just repo) = Just $ _issuesChild repo
-- -- getSelfVar (MainListElem {_typ=PaginatedPulls, ..}) (findRepoParent -> Just repo) = Just $ _pullsChild repo
-- -- getSelfVar (MainListElem {_typ=PaginatedWorkflows, ..}) (findRepoParent -> Just repo) = Just $ _workflowsChild repo
-- getSelfVar _ _ = Nothing

refreshAll :: (
  MonadReader BaseContext m, MonadIO m
  ) => V.Vector MainListElemVariable -> m ()
refreshAll elems = do
  baseContext <- ask
  allRepos <- liftIO $ collectAllRepos (V.toList elems)

  liftIO $ flip runReaderT baseContext $
    void $ async $ forConcurrently allRepos $ \case
      MainListElemRepo {_namespaceName=(owner, name), ..} -> do
        fetchRepo owner name _repo
        -- TODO: clear issues, workflows, etc. and re-fetch for open repos?
      _ -> return () -- Should never happen since collectAllRepos only returns repos

collectAllRepos :: [MainListElemVariable] -> IO [MainListElemVariable]
collectAllRepos = fmap concat . mapM collectFromNode
  where
    collectFromNode :: MainListElemVariable -> IO [MainListElemVariable]
    collectFromNode repo@(MainListElemRepo {}) = return [repo]
    collectFromNode (MainListElemItem {_children, ..}) = do
      children <- readTVarIO _children
      collectAllRepos children
