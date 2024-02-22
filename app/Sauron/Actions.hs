{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Sauron.Actions (
  openBrowserToUrl

  , withScroll

  , fetchRepo

  , fetchWorkflows

  , fetchIssues
  , fetchIssue

  , refresh
  , refreshAll

  , newHealthCheckThread

  , withGithubApiSemaphore
  , withGithubApiSemaphore'

  , whenRepoSelected
  ) where

import Brick as B
import Brick.Widgets.List
import Control.Concurrent.QSem
import Control.Concurrent.STM (retry)
import Control.Exception.Safe (bracket_, bracketOnError_, handleAny)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import Data.String.Interpolate
import qualified Data.Vector as V
import GitHub
import Lens.Micro
import Relude
import Sauron.Options
import Sauron.Types
import UnliftIO.Async
import UnliftIO.Concurrent
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
  ) => Name Owner -> Name Repo -> TVar (Fetchable (WithTotalCount WorkflowRun)) -> TVar MainListElemVariable -> m ()
fetchWorkflows owner name workflowsVar childrenVar = do
  BaseContext {auth} <- ask
  bracketOnError_ (atomically $ writeTVar workflowsVar Fetching)
                  (atomically $ writeTVar workflowsVar (Errored "Workflows fetch failed with exception.")) $
    withGithubApiSemaphore (liftIO $ github auth (workflowRunsR owner name mempty (FetchAtLeast 10))) >>= \case
      Left err -> atomically $ do
        writeTVar workflowsVar (Errored (show err))

        toggledVar <- newTVar False
        workflowChildrenVar <- newTVar []
        writeTVar childrenVar $ MainListElemWorkflows {
          _workflows = workflowsVar
          , _toggled = toggledVar
          , _children = workflowChildrenVar
          , _depth = 2
          , _ident = 0
          }

      Right wtc@(WithTotalCount x _count) -> atomically $ do
        writeTVar workflowsVar (Fetched wtc)

        toggledVar <- newTVar True
        workflowChildren <- forM (V.toList x) $ \iss -> do
          workflowVar <- newTVar (Fetched iss)
          toggledVar' <- newTVar False
          pure $ MainListElemWorkflow {
            _workflow = workflowVar
            , _toggled = toggledVar'
            , _depth = 3
            , _ident = 0
            }
        workflowChildrenVar <- newTVar workflowChildren

        writeTVar childrenVar (MainListElemWorkflows {
                                  _workflows = workflowsVar
                                  , _toggled = toggledVar -- TODO: inherit from previous value
                                  , _children = workflowChildrenVar
                                  , _depth = 2
                                  , _ident = 0
                                  })

fetchIssues :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> TVar Text -> TVar Int -> TVar (Fetchable (V.Vector Issue)) -> TVar MainListElemVariable -> m ()
fetchIssues owner name issueSearchVar issuePageVar issuesVar childrenVar = do
  BaseContext {auth} <- ask
  let search = mempty
  bracketOnError_ (atomically $ writeTVar issuesVar Fetching)
                  (atomically $ writeTVar issuesVar (Errored "Workflows fetch failed with exception.")) $
    withGithubApiSemaphore (liftIO $ github auth (issuesForRepoR owner name search (FetchAtLeast 10))) >>= \case
      Left err -> atomically $ do
        writeTVar issuesVar (Errored (show err))

        toggledVar <- newTVar False
        issueChildrenVar <- newTVar []
        writeTVar childrenVar $ MainListElemIssues {
          _issues = issuesVar
          , _toggled = toggledVar
          , _children = issueChildrenVar
          , _depth = 2
          , _ident = 0
          }
      Right x -> atomically $ do
        writeTVar issuesVar (Fetched x)

        toggledVar <- newTVar True
        issueChildren <- forM (V.toList x) $ \iss -> do
          issueVar <- newTVar (Fetched iss)
          toggledVar' <- newTVar False
          pure $ MainListElemIssue {
            _issue = issueVar
            , _toggled = toggledVar'
            , _depth = 3
            , _ident = 0
            }
        issueChildrenVar <- newTVar issueChildren

        writeTVar childrenVar (MainListElemIssues {
                                  _issues = issuesVar
                                  , _toggled = toggledVar -- TODO: inherit from previous value
                                  , _children = issueChildrenVar
                                  , _depth = 2
                                  , _ident = 0
                                  })


fetchIssue :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> IssueNumber -> TVar (Fetchable Issue) -> m ()
fetchIssue owner name issueNumber issueVar = do
  BaseContext {auth} <- ask
  bracketOnError_ (atomically $ writeTVar issueVar Fetching)
                  (atomically $ writeTVar issueVar (Errored "Workflows fetch failed with exception.")) $
    withGithubApiSemaphore (liftIO $ github auth (issueR owner name issueNumber)) >>= \case
      Left err -> atomically $ do
        writeTVar issueVar (Errored (show err))
        undefined
      Right x -> atomically $ do
        writeTVar issueVar (Fetched x)
        undefined

refresh :: (MonadIO m) => BaseContext -> MainListElemVariable -> m ()
refresh _ (MainListElemHeading {}) = return () -- TODO
refresh bc (MainListElemRepo {_namespaceName=(owner, name), ..}) = liftIO $ do
  void $ async $ liftIO $ runReaderT (fetchWorkflows owner name _workflows _workflowsChild) bc
  void $ async $ liftIO $ runReaderT (fetchIssues owner name _issuesSearch _issuesPage _issues _issuesChild) bc
refresh _ (MainListElemIssues {}) = return () -- TODO
refresh _ (MainListElemIssue {}) = return () -- TODO
refresh _ (MainListElemWorkflows {}) = return () -- TODO
refresh _ (MainListElemWorkflow {}) = return () -- TODO

refreshAll :: (
  MonadReader BaseContext m, MonadIO m
  ) => V.Vector MainListElemVariable -> m ()
refreshAll elems = do
  baseContext <- ask

  liftIO $ flip runReaderT baseContext $
    void $ async $ forConcurrently (V.toList elems) $ \case
      MainListElemHeading {} -> return ()
      MainListElemRepo {_namespaceName=(owner, name), ..} -> do
        fetchRepo owner name _repo
        -- TODO: clear issues, workflows, etc. and re-fetch for open repos?
      MainListElemIssue {} -> return ()
      MainListElemIssues {} -> return ()
      MainListElemWorkflow {} -> return ()
      MainListElemWorkflows {} -> return ()

newHealthCheckThread ::
  BaseContext
  -> (Name Owner, Name Repo)
  -> TVar (Fetchable Repo)
  -> TVar (Fetchable HealthCheckResult)
  -> PeriodSpec
  -> IO (Async ())
newHealthCheckThread baseContext@(BaseContext {auth}) (owner, name) repoVar healthCheckVar (PeriodSpec period) = async $
  handleAny (\e -> putStrLn [i|Health check thread crashed: #{e}|]) $
  forever $ do
    -- TODO: how to not get "thread blocked indefinitely in an STM transaction"?
    defaultBranch <- atomically $ do
      readTVar repoVar >>= \case
        Fetched (Repo {repoDefaultBranch=(Just branch)}) -> pure branch
        _ -> retry

    liftIO $ flip runReaderT baseContext $
      bracketOnError_ (atomically $ writeTVar healthCheckVar Fetching)
                      (atomically $ writeTVar healthCheckVar (Errored "Health check fetch failed with exception.")) $ do
        let search = optionsWorkflowRunBranch defaultBranch
        withGithubApiSemaphore (liftIO $ github auth (workflowRunsR owner name search (FetchAtLeast 1))) >>= \case
          Left err -> atomically $ writeTVar healthCheckVar (Errored (show err))
          Right (WithTotalCount {withTotalCountItems=(V.toList -> ((WorkflowRun {..}):_))}) -> do
            let result = HealthCheckWorkflowResult (chooseWorkflowStatus (fromMaybe workflowRunStatus workflowRunConclusion))
            atomically $ writeTVar healthCheckVar (Fetched result)
          Right _ -> atomically $ writeTVar healthCheckVar (Fetched HealthCheckNoData)

    threadDelay period

withGithubApiSemaphore :: (MonadReader BaseContext m, MonadIO m, MonadMask m) => m a -> m a
withGithubApiSemaphore action = do
  sem <- asks requestSemaphore
  withGithubApiSemaphore' sem action

-- TODO: add timeout here?
withGithubApiSemaphore' :: (MonadIO m, MonadMask m) => QSem -> m a -> m a
withGithubApiSemaphore' sem = bracket_ (liftIO $ waitQSem sem) (liftIO $ signalQSem sem)

whenRepoSelected :: Monad f => AppState -> (Repo -> f ()) -> f ()
whenRepoSelected s cb = whenJust (listSelectedElement (s ^. appMainList)) $ \(_i, el) -> case el of
  MainListElemRepo {_repo=(Fetched r)} -> cb r
  _ -> return ()
