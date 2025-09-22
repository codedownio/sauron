{-# LANGUAGE CPP #-}
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
import Data.Typeable
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
    Just (_, el@(SomeMainListElem (MainListElemItem {..}))) -> action $ viewportScroll (InnerViewport [i|viewport_#{_ident}|])
    _ -> return ()

refresh :: (MonadIO m) => BaseContext -> MainListElemVariable -> NonEmpty MainListElemVariable -> m ()
refresh bc item@(SomeMainListElem (cast -> Just (MainListElemItem {_typ=(HeadingNode {}), _children} :: MainListElem' Variable HeadingNodeT))) _parents =
  readTVarIO _children >>= mapM_ (\child -> refresh bc child (item :| toList _parents))
refresh bc item@(SomeMainListElem (cast -> Just (MainListElemItem {_typ=(RepoNode {}), _children} :: MainListElem' Variable RepoNodeT))) _parents =
  liftIO $ readTVarIO _children >>= mapM_ (\child -> refresh bc child (item :| toList _parents))

refresh bc item@(SomeMainListElem (cast -> Just x@(MainListElemItem {_typ=PaginatedIssues} :: MainListElem' Variable PaginatedIssuesT))) (findRepoParent -> Just (MainListElemItem {_typ=(RepoNode owner name), _children})) =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchIssues owner name x) bc
refresh bc item@(SomeMainListElem (cast -> Just x@(MainListElemItem {_typ=PaginatedPulls} :: MainListElem' Variable PaginatedPullsT))) (findRepoParent -> Just (MainListElemItem {_typ=(RepoNode owner name), _children})) =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchPulls owner name x) bc
refresh bc item@(SomeMainListElem (cast -> Just x@(MainListElemItem {_typ=PaginatedWorkflows} :: MainListElem' Variable PaginatedWorkflowsT))) (findRepoParent -> Just (MainListElemItem {_typ=(RepoNode owner name), _children})) =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchWorkflows owner name x) bc

refresh bc item@(SomeMainListElem (cast -> Just (MainListElemItem {_typ=(SingleIssue (Issue {..})), _state} :: MainListElem' Variable SingleIssueT))) (findRepoParent -> Just (MainListElemItem {_typ=(RepoNode owner name)})) =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchIssueComments owner name issueNumber _state) bc
refresh bc item@(SomeMainListElem (cast -> Just (MainListElemItem {_typ=(SinglePull (Issue {..})), _state} :: MainListElem' Variable SinglePullT))) (findRepoParent -> Just (MainListElemItem {_typ=(RepoNode owner name)})) =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchPullComments owner name issueNumber _state) bc
refresh bc item@(SomeMainListElem (cast -> Just x@(MainListElemItem {_typ=(SingleWorkflow (WorkflowRun {workflowRunWorkflowRunId})), _state} :: MainListElem' Variable SingleWorkflowT))) (findRepoParent -> Just (MainListElemItem {_typ=(RepoNode owner name)})) =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchWorkflowJobs owner name workflowRunWorkflowRunId x) bc
refresh bc item@(SomeMainListElem (cast -> Just x@(MainListElemItem {_typ=(SingleJob job@(Job {})), _state} :: MainListElem' Variable SingleJobT))) (findRepoParent -> Just (MainListElemItem {_typ=(RepoNode owner name)})) =
  liftIO $ void $ async $ liftIO $ runReaderT (fetchJobLogs owner name job x) bc

refresh _ _ _ = return ()

findRepoParent :: NonEmpty MainListElemVariable -> Maybe (MainListElem' Variable RepoNodeT)
findRepoParent elems = viaNonEmpty head [x | SomeMainListElem (cast -> Just x@(MainListElemItem {_typ=(RepoNode {})} :: MainListElem' Variable RepoNodeT)) <- toList elems]

-- findWorkflowsParent :: NonEmpty MainListElemVariable -> Maybe MainListElemVariable
-- findWorkflowsParent elems = viaNonEmpty head [x | x@(MainListElemItem {_typ=PaginatedWorkflows}) <- toList elems]

refreshAll :: (
  MonadReader BaseContext m, MonadIO m
  ) => V.Vector MainListElemVariable -> m ()
refreshAll elems = do
  baseContext <- ask
  allRepos <- liftIO $ collectAllRepos (V.toList elems)

  liftIO $ flip runReaderT baseContext $
    void $ async $ forConcurrently allRepos $ \case
      MainListElemItem {_typ=(RepoNode owner name), _state} -> do
        fetchRepo owner name _state
        -- TODO: clear issues, workflows, etc. and re-fetch for open repos?
      _ -> return () -- Should never happen since collectAllRepos only returns repos

  where
    collectAllRepos :: [MainListElemVariable] -> IO [MainListElem' Variable RepoNodeT]
    collectAllRepos = fmap concat . mapM collectFromNode
      where
        collectFromNode :: MainListElemVariable -> IO [MainListElem' Variable RepoNodeT]
        collectFromNode (SomeMainListElem (cast -> Just item@(MainListElemItem {_typ=(RepoNode _ _)} :: MainListElem' Variable RepoNodeT))) = return [item]
        collectFromNode (SomeMainListElem (MainListElemItem {..})) = do
          undefined
          -- childNodes <- readTVarIO _children
          -- collectAllRepos (getExistentialChildren _typ childNodes)
