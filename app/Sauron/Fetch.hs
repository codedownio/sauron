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
  , fetchJobs
  ) where

import Control.Exception.Safe (bracketOnError_)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import Data.String.Interpolate
import qualified Data.Text as T
import GitHub
import Network.HTTP.Client (responseBody)
import Relude
import Sauron.Actions.Fetch
import Sauron.Actions.Util
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
fetchWorkflows owner name childrenVar =
  fetchPaginated' (workflowRunsR owner name mempty) PaginatedItemsWorkflows makeItemChildren childrenVar
  where
    makeItemChildren parentDepth paginatedItems identifiers = do
      forM (zip paginatedItems identifiers) $ \(PaginatedItemWorkflow (WorkflowRun {..}), identifier) -> do
        childItemsVar <- newTVar NotFetched
        childToggledVar <- newTVar False
        childChildrenVar <- newTVar []
        childSearchVar <- newTVar SearchNone
        childPageInfoVar <- newTVar $ PageInfo 1 Nothing Nothing Nothing Nothing

        return $ MainListElemPaginated {
          _typ = PaginatedJobs
          , _items = childItemsVar
          , _urlSuffix = "workflows"
          , _toggled = childToggledVar
          , _children = childChildrenVar
          , _search = childSearchVar
          , _pageInfo = childPageInfoVar
          , _depth = parentDepth + 1
          , _ident = identifier
          }

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

fetchJobs :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m, MonadFail m
  ) => Name Owner -> Name Repo -> Id WorkflowRun -> STM MainListElemVariable -> (MainListElemVariable -> STM ()) -> m ()
fetchJobs owner name workflowRunId readSelf writeSelf = undefined -- fetchPaginated (jobsForWorkflowRunR owner name workflowRunId) PaginatedItemsJobs

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
