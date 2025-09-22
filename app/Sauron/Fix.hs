
module Sauron.Fix (fixMainListElem) where

import Control.Monad
import Data.Function
import Relude hiding (Down)
import Sauron.Types


fixMainListElem :: MainListElemVariable -> STM MainListElem
fixMainListElem (SomeMainListElem (MainListElemItem {..})) = do
  stateFixed <- readTVar _state

  toggledFixed <- readTVar _toggled
  -- childrenFixed <- readTVar _children >>= mapM fixMainListElem
  childrenFixed <- undefined

  searchFixed <- readTVar _search
  pageInfoFixed <- readTVar _pageInfo

  healthCheckFixed <- readTVar _healthCheck

  return $ SomeMainListElem $ MainListElemItem {
    _typ = _typ
    , _state = stateFixed

    , _urlSuffix = _urlSuffix
    , _toggled = toggledFixed
    , _children = childrenFixed

    , _search = searchFixed
    , _pageInfo = pageInfoFixed

    , _healthCheck = healthCheckFixed
    , _healthCheckThread = _healthCheckThread

    , _depth = _depth
    , _ident = _ident
    }
