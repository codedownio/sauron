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
  -> Maybe (Async ())
  -> Int
  -> IO Int
  -> m (MainListElem' Variable 'RepoT)
newRepoNode nsName repoVar healthCheckVar hcThread repoDepth getIdentifier = do
  toggledVar <- newTVarIO False

  issuesVar <- newTVarIO NotFetched
  issuesToggledVar <- newTVarIO False
  issuesChildrenVar <- newTVarIO []
  issuesSearchVar <- newTVarIO $ SearchText "is:issue is:open"
  issuesPageInfoVar <- newTVarIO $ PageInfo 1 Nothing Nothing Nothing Nothing
  issuesHealthCheckVar <- newTVarIO NotFetched
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
        , _healthCheckThread = Nothing
        , _depth = repoDepth + 1
        , _ident = issuesIdentifier
        } :: MainListElem' Variable PaginatedIssuesT

  pullsVar <- newTVarIO NotFetched
  pullsToggledVar <- newTVarIO False
  pullsChildrenVar <- newTVarIO []
  pullsSearchVar <- newTVarIO $ SearchText "is:pr is:open"
  pullsPageInfoVar <- newTVarIO $ PageInfo 1 Nothing Nothing Nothing Nothing
  pullsHealthCheckVar <- newTVarIO NotFetched
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
        , _healthCheckThread = Nothing
        , _depth = repoDepth + 1
        , _ident = pullsIdentifier
        } :: MainListElem' Variable PaginatedPullsT

  workflowsVar <- newTVarIO NotFetched
  workflowsToggledVar <- newTVarIO False
  workflowsChildrenVar <- newTVarIO []
  workflowsSearchVar <- newTVarIO SearchNone
  workflowsPageInfoVar <- newTVarIO $ PageInfo 1 Nothing Nothing Nothing Nothing
  workflowsHealthCheckVar <- newTVarIO NotFetched
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
        , _healthCheckThread = Nothing
        , _depth = repoDepth + 1
        , _ident = workflowsIdentifier
        } :: MainListElem' Variable PaginatedWorkflowsT

  repoIdentifier <- liftIO getIdentifier
  childrenVar <- newTVarIO [SomeMainListElem issuesChild, SomeMainListElem pullsChild, SomeMainListElem workflowsChild]
  searchVar <- newTVarIO SearchNone
  pageInfoVar <- newTVarIO emptyPageInfo
  return $ RepoNode $ EntityData {
    _static = (fst nsName, snd nsName)
    , _state = repoVar
    , _urlSuffix = ""
    , _toggled = toggledVar
    , _children = childrenVar
    , _search = searchVar
    , _pageInfo = pageInfoVar
    , _healthCheck = healthCheckVar
    , _healthCheckThread = hcThread
    , _depth = repoDepth
    , _ident = repoIdentifier
    }
