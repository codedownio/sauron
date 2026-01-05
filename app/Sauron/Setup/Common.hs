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

  issuesVar <- newTVarIO (SearchText "is:issue is:open", PageInfo 1 Nothing Nothing Nothing Nothing, NotFetched)
  issuesToggledVar <- newTVarIO False
  issuesChildrenVar <- newTVarIO []
  issuesHealthCheckVar <- newTVarIO NotFetched
  issuesHealthCheckThreadVar <- newTVarIO Nothing
  issuesIdentifier <- liftIO getIdentifier
  let issuesChild = PaginatedIssuesNode $ EntityData {
        _static = ()
        , _state = issuesVar
        , _urlSuffix = "issues"
        , _toggled = issuesToggledVar
        , _children = issuesChildrenVar
        , _healthCheck = issuesHealthCheckVar
        , _healthCheckThread = issuesHealthCheckThreadVar
        , _depth = repoDepth + 1
        , _ident = issuesIdentifier
        } :: Node Variable PaginatedIssuesT

  pullsVar <- newTVarIO (SearchText "is:pr is:open", PageInfo 1 Nothing Nothing Nothing Nothing, NotFetched)
  pullsToggledVar <- newTVarIO False
  pullsChildrenVar <- newTVarIO []
  pullsHealthCheckVar <- newTVarIO NotFetched
  pullsHealthCheckThreadVar <- newTVarIO Nothing
  pullsIdentifier <- liftIO getIdentifier
  let pullsChild = PaginatedPullsNode $ EntityData {
        _static = ()
        , _state = pullsVar
        , _urlSuffix = "pulls"
        , _toggled = pullsToggledVar
        , _children = pullsChildrenVar
        , _healthCheck = pullsHealthCheckVar
        , _healthCheckThread = pullsHealthCheckThreadVar
        , _depth = repoDepth + 1
        , _ident = pullsIdentifier
        } :: Node Variable PaginatedPullsT

  workflowsVar <- newTVarIO (SearchNone, PageInfo 1 Nothing Nothing Nothing Nothing, NotFetched)
  workflowsToggledVar <- newTVarIO False
  workflowsChildrenVar <- newTVarIO []
  workflowsHealthCheckVar <- newTVarIO NotFetched
  workflowsHealthCheckThreadVar <- newTVarIO Nothing
  workflowsIdentifier <- liftIO getIdentifier
  let workflowsChild = PaginatedWorkflowsNode $ EntityData {
        _static = ()
        , _state = workflowsVar
        , _urlSuffix = "actions"
        , _toggled = workflowsToggledVar
        , _children = workflowsChildrenVar
        , _healthCheck = workflowsHealthCheckVar
        , _healthCheckThread = workflowsHealthCheckThreadVar
        , _depth = repoDepth + 1
        , _ident = workflowsIdentifier
        } :: Node Variable PaginatedWorkflowsT

  -- Traditional flat branches list
  branchesVar <- newTVarIO (SearchNone, emptyPageInfo, NotFetched)
  branchesToggledVar <- newTVarIO False
  branchesChildrenVar <- newTVarIO []
  branchesHealthCheckVar <- newTVarIO NotFetched
  branchesHealthCheckThreadVar <- newTVarIO Nothing
  branchesIdentifier <- liftIO getIdentifier
  let branchesChild = PaginatedBranchesNode $ EntityData {
        _static = ()
        , _state = branchesVar
        , _urlSuffix = "branches"
        , _toggled = branchesToggledVar
        , _children = branchesChildrenVar
        , _healthCheck = branchesHealthCheckVar
        , _healthCheckThread = branchesHealthCheckThreadVar
        , _depth = repoDepth + 1
        , _ident = branchesIdentifier
        } :: Node Variable PaginatedBranchesT

  -- New GitHub-style categorized branches
  categorizedBranchesStateVar <- newTVarIO ()
  categorizedBranchesToggledVar <- newTVarIO False
  categorizedBranchesChildrenVar <- newTVarIO []
  categorizedBranchesHealthCheckVar <- newTVarIO NotFetched
  categorizedBranchesHealthCheckThreadVar <- newTVarIO Nothing
  categorizedBranchesIdentifier <- liftIO getIdentifier
  let categorizedBranchesChild = OverallBranchesNode $ EntityData {
        _static = ()
        , _state = categorizedBranchesStateVar
        , _urlSuffix = "branches/categorized"
        , _toggled = categorizedBranchesToggledVar
        , _children = categorizedBranchesChildrenVar
        , _healthCheck = categorizedBranchesHealthCheckVar
        , _healthCheckThread = categorizedBranchesHealthCheckThreadVar
        , _depth = repoDepth + 1
        , _ident = categorizedBranchesIdentifier
        } :: Node Variable OverallBranchesT

  repoIdentifier <- liftIO getIdentifier
  childrenVar <- newTVarIO [SomeNode issuesChild, SomeNode pullsChild, SomeNode workflowsChild, SomeNode branchesChild, SomeNode categorizedBranchesChild]
  repoHealthCheckThreadVar <- newTVarIO hcThread
  return $ RepoNode $ EntityData {
    _static = (fst nsName, snd nsName)
    , _state = repoVar
    , _urlSuffix = ""
    , _toggled = toggledVar
    , _children = childrenVar
    , _healthCheck = healthCheckVar
    , _healthCheckThread = repoHealthCheckThreadVar
    , _depth = repoDepth
    , _ident = repoIdentifier
    }
