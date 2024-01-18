{-# LANGUAGE RankNTypes #-}

module Sauron.Actions (
  openBrowserToUrl

  , withScroll

  , fetchRepo
  , fetchWorkflows

  , refreshAll

  , withGithubApiSemaphore
  , withGithubApiSemaphore'

  , whenRepoSelected
  ) where

import Brick as B
import Brick.Widgets.List
import Control.Concurrent.QSem
import Control.Exception.Safe (bracket_, bracketOnError_)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import Data.String.Interpolate
import qualified Data.Vector as V
import GitHub
import Lens.Micro
import Relude
import Sauron.Types
import UnliftIO.Async
import UnliftIO.Process


openBrowserToUrl :: MonadIO m => String -> m ()
openBrowserToUrl url =
  void $ readCreateProcessWithExitCode (proc "xdg-open" [url]) ""

withScroll :: AppState -> (forall s. ViewportScroll ClickableName -> EventM n s ()) -> EventM n AppState ()
withScroll s action = do
  case listSelectedElement (s ^. appMainList) of
    Just (_, MainListElemRepo {..}) -> do
      let scroll = viewportScroll (InnerViewport [i|viewport_#{_ident}|])
      action scroll
    _ -> return ()

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

fetchWorkflows :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> TVar (Fetchable (WithTotalCount WorkflowRun)) -> m ()
fetchWorkflows owner name workflowsVar = do
  BaseContext {auth} <- ask
  bracketOnError_ (atomically $ writeTVar workflowsVar Fetching)
                  (atomically $ writeTVar workflowsVar (Errored "Workflows fetch failed with exception.")) $
    withGithubApiSemaphore (liftIO $ github auth (workflowRunsR owner name mempty (FetchAtLeast 10))) >>= \case
      Left err -> atomically $ writeTVar workflowsVar (Errored (show err))
      Right x -> atomically $ writeTVar workflowsVar (Fetched x)

refreshAll :: (
  MonadReader BaseContext m, MonadIO m
  ) => V.Vector MainListElemVariable -> m ()
refreshAll elems = do
  baseContext <- ask

  liftIO $ flip runReaderT baseContext $
    void $ async $ forConcurrently (V.toList elems) $ \case
      MainListElemHeading {} -> return ()
      MainListElemRepo {_namespaceName=(owner, name), ..} -> do
        withGithubApiSemaphore $
          fetchRepo owner name _repo

        withGithubApiSemaphore $
          fetchWorkflows owner name _workflows

withGithubApiSemaphore :: (MonadReader BaseContext m, MonadIO m, MonadMask m) => m a -> m a
withGithubApiSemaphore action = do
  sem <- asks requestSemaphore
  withGithubApiSemaphore' sem action

withGithubApiSemaphore' :: (MonadIO m, MonadMask m) => QSem -> m a -> m a
withGithubApiSemaphore' sem = bracket_ (liftIO $ waitQSem sem) (liftIO $ signalQSem sem)

whenRepoSelected :: Monad f => AppState -> (Repo -> f ()) -> f ()
whenRepoSelected s cb = whenJust (listSelectedElement (s ^. appMainList)) $ \(_i, el) -> case el of
  MainListElemRepo {_repo=(Fetched r)} -> cb r
  _ -> return ()
