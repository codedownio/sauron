
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

  toggledFixed <- readTVar _toggled
  issuesChildFixed <- readTVar _issuesChild >>= fixMainListElem
  pullsChildFixed <- readTVar _pullsChild >>= fixMainListElem
  workflowsChildFixed <- readTVar _workflowsChild >>= fixMainListElem

  return $ MainListElemRepo {
    _namespaceName = _namespaceName
    , _repo = repoFixed

    , _healthCheck = healthCheckFixed
    , _healthCheckThread = _healthCheckThread

    , _toggled = toggledFixed
    , _issuesChild = issuesChildFixed
    , _pullsChild = pullsChildFixed
    , _workflowsChild = workflowsChildFixed

    , _depth = _depth
    , _ident = _ident
    }
fixMainListElem (MainListElemPaginated {..}) = do
  itemsFixed <- readTVar _items

  toggledFixed <- readTVar _toggled
  childrenFixed <- readTVar _children >>= mapM fixMainListElem

  searchFixed <- readTVar _search
  pageInfoFixed <- readTVar _pageInfo

  return $ MainListElemPaginated {
    _typ = _typ
    , _items = itemsFixed

    , _label = _label
    , _urlSuffix = _urlSuffix
    , _toggled = toggledFixed
    , _children = childrenFixed

    , _search = searchFixed
    , _pageInfo = pageInfoFixed

    , _depth = _depth
    , _ident = _ident
    }
fixMainListElem (MainListElemItem {..}) = do
  itemFixed <- readTVar _item
  itemInnerFixed <- readTVar _itemInner

  toggledFixed <- readTVar _toggled

  return $ MainListElemItem {
    _item = itemFixed
    , _itemInner = itemInnerFixed

    , _toggled = toggledFixed

    , _depth = _depth
    , _ident = _ident
    }
