{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Sauron.Fix (fixMainListElem) where

import Data.Typeable
import Relude hiding (Down)
import Sauron.Types


fixMainListElem :: MainListElemVariable -> STM MainListElem
fixMainListElem (SomeMainListElem item@(getEntityData -> ed)) = do
  fixedEntityData <- fixEntityData ed
  return $ SomeMainListElem $ setEntityData fixedEntityData item

fixEntityData :: forall a. Typeable (NodeChildType Variable a) => EntityData Variable a -> STM (EntityData Fixed a)
fixEntityData (EntityData {..}) = do
  stateFixed <- readTVar _state
  toggledFixed <- readTVar _toggled
  searchFixed <- readTVar _search
  pageInfoFixed <- readTVar _pageInfo
  healthCheckFixed <- readTVar _healthCheck

  childrenFixed <- readTVar _children >>= mapM (fixChild @a)

  return $ EntityData {
    _static = _static
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

fixChild :: Typeable (NodeChildType Variable a) => NodeChildType Variable a -> STM (NodeChildType Fixed a)
fixChild (cast -> Just (elem :: MainListElem' Variable SingleIssueT)) = do
  -- SomeMainListElem el <- fixMainListElem (SomeMainListElem elem)
  -- return el

  let ed = getEntityData elem
  ed' <- fixEntityData ed
  -- return $ setEntityData ed' elem
  return undefined
