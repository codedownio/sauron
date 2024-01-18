
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
  workflowsFixed <- readTVar _workflows
  toggledFixed <- readTVar _toggled
  repoFixed <- readTVar _repo

  return $ MainListElemRepo {
    _namespaceName = _namespaceName
    , _repo = repoFixed
    , _workflows = workflowsFixed
    , _depth = _depth
    , _toggled = toggledFixed
    , _ident = _ident
    }
