{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Sauron.Actions (
  openBrowserToUrl

  , withScroll

  , fetchRepo

  , fetchWorkflows
  , fetchWorkflowsWithJobs

  , fetchIssues
  , fetchIssue

  , fetchWorkflowJobs
  , fetchJobs

  , refresh
  , refreshAll
  ) where

import Brick as B
import Brick.Widgets.List
import Control.Exception.Safe (bracketOnError_)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Vector as V
import GitHub
import Lens.Micro
import Network.HTTP.Client (responseBody)
import Relude
import Sauron.Actions.Fetch
import Sauron.Actions.Util
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
fetchIssues owner name childrenVar = do
  extraTerms <- readTVarIO childrenVar >>= (readTVarIO . _search) >>= \case
    SearchNone -> pure []
    SearchText t -> pure $ T.words t

  let fullQuery = T.intercalate "+" ([i|repo:#{untagName owner}/#{untagName name}|] : extraTerms)

  fetchPaginated (searchIssuesR fullQuery) PaginatedItemsIssues childrenVar

fetchPulls :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m, MonadFail m
  ) => Name Owner -> Name Repo -> TVar MainListElemVariable -> m ()
fetchPulls owner name childrenVar = do
  extraTerms <- readTVarIO childrenVar >>= (readTVarIO . _search) >>= \case
    SearchNone -> pure []
    SearchText t -> pure $ T.words t

  let fullQuery = T.intercalate "+" ([i|repo:#{untagName owner}/#{untagName name}|] : extraTerms)

  fetchPaginated (searchIssuesR fullQuery) PaginatedItemsPulls childrenVar

fetchWorkflows :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m, MonadFail m
  ) => Name Owner -> Name Repo -> TVar MainListElemVariable -> m ()
fetchWorkflows owner name childrenVar = do
  fetchPaginated (workflowRunsR owner name mempty) PaginatedItemsWorkflows childrenVar

fetchIssueComments :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> IssueNumber -> TVar (Fetchable PaginatedItemInner) -> m ()
fetchIssueComments owner name issueNumber inner = do
  BaseContext {auth, manager} <- ask
  bracketOnError_ (atomically $ writeTVar inner Fetching)
                  (atomically $ writeTVar inner (Errored "Issue comments fetch failed with exception.")) $
    withGithubApiSemaphore (liftIO $ executeRequestWithMgrAndRes manager auth (commentsR owner name issueNumber FetchAll)) >>= \case
      Left err -> atomically $ writeTVar inner (Errored (show err))
      Right v -> atomically $ writeTVar inner (Fetched (PaginatedItemInnerIssue (responseBody v)))

fetchPullComments :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> IssueNumber -> TVar (Fetchable PaginatedItemInner) -> m ()
fetchPullComments owner name issueNumber inner = do
  BaseContext {auth, manager} <- ask
  bracketOnError_ (atomically $ writeTVar inner Fetching)
                  (atomically $ writeTVar inner (Errored "Pull comments fetch failed with exception.")) $
    -- pullRequestCommentsR returns comments on the "unified diff"
    -- there are also "commit comments" and "issue comments".
    -- The last one are the most common on PRs, so we use commentsR
    withGithubApiSemaphore (liftIO $ executeRequestWithMgrAndRes manager auth (commentsR owner name issueNumber FetchAll)) >>= \case
      Left err -> atomically $ writeTVar inner (Errored (show err))
      Right v -> atomically $ writeTVar inner (Fetched (PaginatedItemInnerPull (responseBody v)))

fetchWorkflowJobs :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Id WorkflowRun -> TVar (Fetchable PaginatedItemInner) -> m ()
fetchWorkflowJobs owner name workflowRunId inner = do
  BaseContext {auth, manager} <- ask
  bracketOnError_ (atomically $ writeTVar inner Fetching)
                  (atomically $ writeTVar inner (Errored "Workflow jobs fetch failed with exception.")) $
    withGithubApiSemaphore (liftIO $ executeRequestWithMgrAndRes manager auth (jobsForWorkflowRunR owner name workflowRunId FetchAll)) >>= \case
      Left err -> atomically $ writeTVar inner (Errored (show err))
      Right v -> atomically $ writeTVar inner (Fetched (PaginatedItemInnerWorkflow (responseBody v)))

fetchWorkflowsWithJobs :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m, MonadFail m
  ) => Name Owner -> Name Repo -> TVar MainListElemVariable -> m ()
fetchWorkflowsWithJobs owner name =
  fetchPaginatedPaginated PaginatedJobs "Jobs" "" (\fc -> workflowRunsR owner name mempty fc) PaginatedItemsWorkflows

fetchJobs :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m, MonadFail m
  ) => Name Owner -> Name Repo -> Id WorkflowRun -> TVar MainListElemVariable -> m ()
fetchJobs owner name workflowRunId = fetchPaginated (jobsForWorkflowRunR owner name workflowRunId) PaginatedItemsJobs

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

refresh :: (MonadIO m) => BaseContext -> MainListElemVariable -> MainListElemVariable -> m ()
refresh _ (MainListElemHeading {}) _ = return () -- TODO: refresh all repos
refresh bc (MainListElemRepo {_namespaceName=(owner, name), _issuesChild, _pullsChild, _workflowsChild}) _ = liftIO $ do
  void $ async $ liftIO $ runReaderT (fetchIssues owner name _issuesChild) bc
  void $ async $ liftIO $ runReaderT (fetchPulls owner name _pullsChild) bc
  void $ async $ liftIO $ runReaderT (fetchWorkflowsWithJobs owner name _workflowsChild) bc
refresh bc (MainListElemPaginated {..}) (MainListElemRepo {_namespaceName=(owner, name), _issuesChild, _pullsChild, _workflowsChild}) = liftIO $ case _typ of
  PaginatedIssues -> void $ async $ liftIO $ runReaderT (fetchIssues owner name _issuesChild) bc
  PaginatedPulls -> void $ async $ liftIO $ runReaderT (fetchPulls owner name _pullsChild) bc
  PaginatedWorkflows -> void $ async $ liftIO $ runReaderT (fetchWorkflows owner name _workflowsChild) bc
  PaginatedJobs -> return () -- Jobs are fetched when workflow items are expanded
refresh bc (MainListElemItem {_item, ..}) (MainListElemRepo {_namespaceName=(owner, name)}) = readTVarIO _item >>= \case
  Fetched (PaginatedItemIssue (Issue {..})) -> liftIO $ void $ async $ liftIO $ runReaderT (fetchIssueComments owner name issueNumber _itemInner) bc
  Fetched (PaginatedItemPull (Issue {..})) -> liftIO $ void $ async $ liftIO $ runReaderT (fetchPullComments owner name issueNumber _itemInner) bc
  Fetched (PaginatedItemWorkflow (WorkflowRun {..})) -> liftIO $ void $ async $ liftIO $ runReaderT (fetchWorkflowJobs owner name workflowRunWorkflowRunId _itemInner) bc
  Fetched (PaginatedItemJob _) -> return ()
  _ -> return ()
refresh _ _ _ = return ()

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
      MainListElemPaginated {} -> return ()
      MainListElemItem {} -> return ()
