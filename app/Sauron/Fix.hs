
module Sauron.Fix (fixMainListElem) where

import Control.Monad
import Data.Function
import Relude hiding (Down)
import Sauron.Types


fixMainListElem :: MainListElemVariable -> STM MainListElem
fixMainListElem (MainListElemHeading {..}) = do
  statusFixed <- readTVar _status
  toggledFixed <- readTVar _toggled

  return $ MainListElemHeading {
    _label = _label
    , _depth = _depth
    , _toggled = toggledFixed
    , _status = statusFixed
    , _ident = _ident
    }
fixMainListElem (MainListElemRepo {..}) = do
  repoFixed <- readTVar _repo

  healthCheckFixed <- readTVar _healthCheck

  issuesSearchFixed <- readTVar _issuesSearch
  issuesPageFixed <- readTVar _issuesPage
  issuesFixed <- readTVar _issues

  workflowsFixed <- readTVar _workflows

  toggledFixed <- readTVar _toggled
  childrenFixed <- readTVar _children >>= mapM fixMainListElem

  return $ MainListElemRepo {
    _namespaceName = _namespaceName
    , _repo = repoFixed

    , _healthCheck = healthCheckFixed
    , _healthCheckThread = _healthCheckThread

    , _workflows = workflowsFixed

    , _issuesSearch = issuesSearchFixed
    , _issuesPage = issuesPageFixed
    , _issues = issuesFixed

    , _toggled = toggledFixed
    , _children = childrenFixed

    , _depth = _depth
    , _ident = _ident
    }
fixMainListElem (MainListElemIssues {..}) = do
  issuesFixed <- readTVar _issues

  toggledFixed <- readTVar _toggled
  childrenFixed <- readTVar _children >>= mapM fixMainListElem

  return $ MainListElemIssues {
    _issues = issuesFixed

    , _toggled = toggledFixed
    , _children = childrenFixed

    , _depth = _depth
    , _ident = _ident
    }
fixMainListElem (MainListElemIssue {..}) = do
  issueFixed <- readTVar _issue

  toggledFixed <- readTVar _toggled

  return $ MainListElemIssue {
    _issue = issueFixed

    , _toggled = toggledFixed

    , _depth = _depth
    , _ident = _ident
    }
