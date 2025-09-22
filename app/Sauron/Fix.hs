
module Sauron.Fix (fixMainListElem) where

import Relude hiding (Down)
import Sauron.Types


fixMainListElem :: MainListElemVariable -> STM MainListElem
fixMainListElem (SomeMainListElem _item@(getEntityData -> (EntityData {..}))) = do
  _stateFixed <- readTVar _state
  _toggledFixed <- readTVar _toggled
  -- TODO: Implement proper GADT-aware children fixing
  -- This is complex due to the type-level constraints of GADTs
  _childrenFixed <- undefined
  _searchFixed <- readTVar _search
  _pageInfoFixed <- readTVar _pageInfo
  _healthCheckFixed <- readTVar _healthCheck

  undefined

  -- return $ SomeMainListElem $ MainListElemItem {
  --   _typ = _typ
  --   , _state = stateFixed

  --   , _urlSuffix = _urlSuffix
  --   , _toggled = toggledFixed
  --   , _children = childrenFixed

  --   , _search = searchFixed
  --   , _pageInfo = pageInfoFixed

  --   , _healthCheck = healthCheckFixed
  --   , _healthCheckThread = _healthCheckThread

  --   , _depth = _depth
  --   , _ident = _ident
  --   }
