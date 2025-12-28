{-# LANGUAGE DataKinds #-}

module Sauron.Setup.Common (
  newRepoNode
  ) where

import Control.Monad
import Control.Monad.Writer
import Data.Function
import GitHub
import Relude hiding (Down)
import Sauron.Types
import UnliftIO.Async


newRepoNode ::
  (MonadIO m)
  => (Name Owner, Name Repo)
  -> TVar (Fetchable Repo)
  -> TVar (Fetchable HealthCheckResult)
  -> Maybe (Async (), Int)
  -> Int
  -> IO Int
  -> m (Node Variable 'RepoT)
newRepoNode nsName repoVar healthCheckVar hcThread repoDepth getIdentifier = do
  toggledVar <- newTVarIO False

  issuesVar <- newTVarIO NotFetched
  issuesToggledVar <- newTVarIO False
  issuesChildrenVar <- newTVarIO []
  issuesSearchVar <- newTVarIO $ SearchText "is:issue is:open"
  issuesPageInfoVar <- newTVarIO $ PageInfo 1 Nothing Nothing Nothing Nothing
  issuesHealthCheckVar <- newTVarIO NotFetched
  issuesHealthCheckThreadVar <- newTVarIO Nothing
  issuesIdentifier <- liftIO getIdentifier
  let issuesChild = PaginatedIssuesNode $ EntityData {
        _static = ()
        , _state = issuesVar
        , _urlSuffix = "issues"
        , _toggled = issuesToggledVar
        , _children = issuesChildrenVar
        , _search = issuesSearchVar
        , _pageInfo = issuesPageInfoVar
        , _healthCheck = issuesHealthCheckVar
        , _healthCheckThread = issuesHealthCheckThreadVar
        , _depth = repoDepth + 1
        , _ident = issuesIdentifier
        } :: Node Variable PaginatedIssuesT

  pullsVar <- newTVarIO NotFetched
  pullsToggledVar <- newTVarIO False
  pullsChildrenVar <- newTVarIO []
  pullsSearchVar <- newTVarIO $ SearchText "is:pr is:open"
  pullsPageInfoVar <- newTVarIO $ PageInfo 1 Nothing Nothing Nothing Nothing
  pullsHealthCheckVar <- newTVarIO NotFetched
  pullsHealthCheckThreadVar <- newTVarIO Nothing
  pullsIdentifier <- liftIO getIdentifier
  let pullsChild = PaginatedPullsNode $ EntityData {
        _static = ()
        , _state = pullsVar
        , _urlSuffix = "pulls"
        , _toggled = pullsToggledVar
        , _children = pullsChildrenVar
        , _search = pullsSearchVar
        , _pageInfo = pullsPageInfoVar
        , _healthCheck = pullsHealthCheckVar
        , _healthCheckThread = pullsHealthCheckThreadVar
        , _depth = repoDepth + 1
        , _ident = pullsIdentifier
        } :: Node Variable PaginatedPullsT

  workflowsVar <- newTVarIO NotFetched
  workflowsToggledVar <- newTVarIO False
  workflowsChildrenVar <- newTVarIO []
  workflowsSearchVar <- newTVarIO SearchNone
  workflowsPageInfoVar <- newTVarIO $ PageInfo 1 Nothing Nothing Nothing Nothing
  workflowsHealthCheckVar <- newTVarIO NotFetched
  workflowsHealthCheckThreadVar <- newTVarIO Nothing
  workflowsIdentifier <- liftIO getIdentifier
  let workflowsChild = PaginatedWorkflowsNode $ EntityData {
        _static = ()
        , _state = workflowsVar
        , _urlSuffix = "actions"
        , _toggled = workflowsToggledVar
        , _children = workflowsChildrenVar
        , _search = workflowsSearchVar
        , _pageInfo = workflowsPageInfoVar
        , _healthCheck = workflowsHealthCheckVar
        , _healthCheckThread = workflowsHealthCheckThreadVar
        , _depth = repoDepth + 1
        , _ident = workflowsIdentifier
        } :: Node Variable PaginatedWorkflowsT

  -- Traditional flat branches list
  branchesVar <- newTVarIO NotFetched
  branchesToggledVar <- newTVarIO False
  branchesChildrenVar <- newTVarIO []
  branchesSearchVar <- newTVarIO SearchNone
  branchesPageInfoVar <- newTVarIO $ PageInfo 1 Nothing Nothing Nothing Nothing
  branchesHealthCheckVar <- newTVarIO NotFetched
  branchesHealthCheckThreadVar <- newTVarIO Nothing
  branchesIdentifier <- liftIO getIdentifier
  let branchesChild = PaginatedBranchesNode $ EntityData {
        _static = ()
        , _state = branchesVar
        , _urlSuffix = "branches"
        , _toggled = branchesToggledVar
        , _children = branchesChildrenVar
        , _search = branchesSearchVar
        , _pageInfo = branchesPageInfoVar
        , _healthCheck = branchesHealthCheckVar
        , _healthCheckThread = branchesHealthCheckThreadVar
        , _depth = repoDepth + 1
        , _ident = branchesIdentifier
        } :: Node Variable PaginatedBranchesT

  -- New GitHub-style categorized branches
  categorizedBranchesStateVar <- newTVarIO NotFetched
  categorizedBranchesToggledVar <- newTVarIO False
  categorizedBranchesChildrenVar <- newTVarIO []
  categorizedBranchesSearchVar <- newTVarIO SearchNone
  categorizedBranchesPageInfoVar <- newTVarIO $ PageInfo 1 Nothing Nothing Nothing Nothing
  categorizedBranchesHealthCheckVar <- newTVarIO NotFetched
  categorizedBranchesHealthCheckThreadVar <- newTVarIO Nothing
  categorizedBranchesIdentifier <- liftIO getIdentifier
  let categorizedBranchesChild = OverallBranchesNode $ EntityData {
        _static = ()
        , _state = categorizedBranchesStateVar
        , _urlSuffix = "branches/categorized"
        , _toggled = categorizedBranchesToggledVar
        , _children = categorizedBranchesChildrenVar
        , _search = categorizedBranchesSearchVar
        , _pageInfo = categorizedBranchesPageInfoVar
        , _healthCheck = categorizedBranchesHealthCheckVar
        , _healthCheckThread = categorizedBranchesHealthCheckThreadVar
        , _depth = repoDepth + 1
        , _ident = categorizedBranchesIdentifier
        } :: Node Variable OverallBranchesT

  repoIdentifier <- liftIO getIdentifier
  childrenVar <- newTVarIO [SomeNode issuesChild, SomeNode pullsChild, SomeNode workflowsChild, SomeNode branchesChild, SomeNode categorizedBranchesChild]
  searchVar <- newTVarIO SearchNone
  pageInfoVar <- newTVarIO emptyPageInfo
  repoHealthCheckThreadVar <- newTVarIO hcThread
  return $ RepoNode $ EntityData {
    _static = (fst nsName, snd nsName)
    , _state = repoVar
    , _urlSuffix = ""
    , _toggled = toggledVar
    , _children = childrenVar
    , _search = searchVar
    , _pageInfo = pageInfoVar
    , _healthCheck = healthCheckVar
    , _healthCheckThread = repoHealthCheckThreadVar
    , _depth = repoDepth
    , _ident = repoIdentifier
    }
