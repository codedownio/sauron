
module Sauron.Fix (fixMainListElem) where

import Control.Monad
import Data.Function
import Relude hiding (Down)
import Sauron.Types


fixMainListElem :: MainListElemVariable -> STM MainListElem
fixMainListElem (MainListElemHeading {..}) = do
  statusFixed <- readTVar _status

  return $ MainListElemHeading {
    _label = _label
    , _depth = _depth
    , _toggled = _toggled
    , _status = statusFixed
    , _ident = _ident
    }
fixMainListElem (MainListElemRepo {..}) = do
  workflowsFixed <- readTVar _workflows
  statusFixed <- readTVar _status

  return $ MainListElemRepo {
    _repo = _repo
    , _workflows = workflowsFixed
    , _depth = _depth
    , _toggled = _toggled
    , _status = statusFixed
    , _ident = _ident
    }
