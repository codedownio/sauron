
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
  -> m MainListElemVariable
newRepoNode nsName repoVar healthCheckVar hcThread repoDepth = do
  toggledVar <- newTVarIO False

  issuesVar <- newTVarIO NotFetched
  issuesToggledVar <- newTVarIO False
  issuesChildrenVar <- newTVarIO []
  issuesSearchVar <- newTVarIO $ SearchText "is:issue is:open"
  issuesPageInfoVar <- newTVarIO $ PageInfo 1 Nothing Nothing Nothing Nothing
  issuesChildVar <- newTVarIO $ MainListElemPaginated {
    _typ = PaginatedIssues
    , _items = issuesVar
    , _label = "Issues"
    , _urlSuffix = "issues"
    , _toggled = issuesToggledVar
    , _children = issuesChildrenVar
    , _search = issuesSearchVar
    , _pageInfo = issuesPageInfoVar
    , _depth = 2
    , _ident = 0
    }

  pullsVar <- newTVarIO NotFetched
  pullsToggledVar <- newTVarIO False
  pullsChildrenVar <- newTVarIO []
  pullsSearchVar <- newTVarIO $ SearchText "is:pr is:open"
  pullsPageInfoVar <- newTVarIO $ PageInfo 1 Nothing Nothing Nothing Nothing
  pullsChildVar <- newTVarIO $ MainListElemPaginated {
    _typ = PaginatedPulls
    , _items = pullsVar
    , _label = "Pulls"
    , _urlSuffix = "pulls"
    , _toggled = pullsToggledVar
    , _children = pullsChildrenVar
    , _search = pullsSearchVar
    , _pageInfo = pullsPageInfoVar
    , _depth = 2
    , _ident = 0
    }

  workflowsVar <- newTVarIO NotFetched
  workflowsToggledVar <- newTVarIO False
  workflowsChildrenVar <- newTVarIO []
  workflowsSearchVar <- newTVarIO $ SearchNone
  workflowsPageInfoVar <- newTVarIO $ PageInfo 1 Nothing Nothing Nothing Nothing
  workflowsChildVar <- newTVarIO $ MainListElemPaginated {
    _typ = PaginatedWorkflows
    , _items = workflowsVar
    , _label = "Workflows"
    , _urlSuffix = "actions"
    , _toggled = workflowsToggledVar
    , _children = workflowsChildrenVar
    , _search = workflowsSearchVar
    , _pageInfo = workflowsPageInfoVar
    , _depth = 2
    , _ident = 0
    }

  return $ MainListElemRepo {
    _namespaceName = nsName
    , _repo = repoVar

    , _healthCheck = healthCheckVar
    , _healthCheckThread = hcThread

    , _toggled = toggledVar
    , _issuesChild = issuesChildVar
    , _pullsChild = pullsChildVar
    , _workflowsChild = workflowsChildVar

    , _depth = repoDepth
    , _ident = 0
    }
