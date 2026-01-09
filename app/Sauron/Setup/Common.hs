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

  -- Your branches
  yourBranchesVar <- newTVarIO (SearchNone, emptyPageInfo, NotFetched)
  yourBranchesToggledVar <- newTVarIO False
  yourBranchesChildrenVar <- newTVarIO []
  yourBranchesHealthCheckVar <- newTVarIO NotFetched
  yourBranchesHealthCheckThreadVar <- newTVarIO Nothing
  yourBranchesIdentifier <- liftIO getIdentifier
  let yourBranchesChild = PaginatedYourBranchesNode $ EntityData {
        _static = ()
        , _state = yourBranchesVar
        , _urlSuffix = "your-branches"
        , _toggled = yourBranchesToggledVar
        , _children = yourBranchesChildrenVar
        , _healthCheck = yourBranchesHealthCheckVar
        , _healthCheckThread = yourBranchesHealthCheckThreadVar
        , _depth = repoDepth + 1
        , _ident = yourBranchesIdentifier
        } :: Node Variable PaginatedYourBranchesT

  -- Active branches
  activeBranchesVar <- newTVarIO (SearchNone, emptyPageInfo, NotFetched)
  activeBranchesToggledVar <- newTVarIO False
  activeBranchesChildrenVar <- newTVarIO []
  activeBranchesHealthCheckVar <- newTVarIO NotFetched
  activeBranchesHealthCheckThreadVar <- newTVarIO Nothing
  activeBranchesIdentifier <- liftIO getIdentifier
  let activeBranchesChild = PaginatedActiveBranchesNode $ EntityData {
        _static = ()
        , _state = activeBranchesVar
        , _urlSuffix = "active-branches"
        , _toggled = activeBranchesToggledVar
        , _children = activeBranchesChildrenVar
        , _healthCheck = activeBranchesHealthCheckVar
        , _healthCheckThread = activeBranchesHealthCheckThreadVar
        , _depth = repoDepth + 1
        , _ident = activeBranchesIdentifier
        } :: Node Variable PaginatedActiveBranchesT

  -- Stale branches
  staleBranchesVar <- newTVarIO (SearchNone, emptyPageInfo, NotFetched)
  staleBranchesToggledVar <- newTVarIO False
  staleBranchesChildrenVar <- newTVarIO []
  staleBranchesHealthCheckVar <- newTVarIO NotFetched
  staleBranchesHealthCheckThreadVar <- newTVarIO Nothing
  staleBranchesIdentifier <- liftIO getIdentifier
  let staleBranchesChild = PaginatedStaleBranchesNode $ EntityData {
        _static = ()
        , _state = staleBranchesVar
        , _urlSuffix = "stale-branches"
        , _toggled = staleBranchesToggledVar
        , _children = staleBranchesChildrenVar
        , _healthCheck = staleBranchesHealthCheckVar
        , _healthCheckThread = staleBranchesHealthCheckThreadVar
        , _depth = repoDepth + 1
        , _ident = staleBranchesIdentifier
        } :: Node Variable PaginatedStaleBranchesT

  repoIdentifier <- liftIO getIdentifier
  childrenVar <- newTVarIO [SomeNode issuesChild, SomeNode pullsChild, SomeNode workflowsChild, SomeNode yourBranchesChild, SomeNode activeBranchesChild, SomeNode staleBranchesChild]
  repoHealthCheckThreadVar <- newTVarIO hcThread
  return $ RepoNode $ EntityData {
    _static = nsName
    , _state = repoVar
    , _urlSuffix = ""
    , _toggled = toggledVar
    , _children = childrenVar
    , _healthCheck = healthCheckVar
    , _healthCheckThread = repoHealthCheckThreadVar
    , _depth = repoDepth
    , _ident = repoIdentifier
    }
