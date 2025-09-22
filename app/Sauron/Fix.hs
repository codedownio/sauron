-- We have extra strict constraints on fixChildlessNode, to ensure it's only
-- used on nodes where NodeChildType f a ~ (), which normally GHC warns about.
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Sauron.Fix (fixMainListElem) where

import Relude
import Sauron.Types


fixMainListElem :: MainListElemVariable -> STM MainListElem
fixMainListElem (SomeMainListElem item) = SomeMainListElem <$> fixMainListElem' item

fixMainListElem' :: MainListElem' Variable a -> STM (MainListElem' Fixed a)
fixMainListElem' item@(PaginatedIssuesNode ed) = fixTypedNode item ed
fixMainListElem' item@(PaginatedPullsNode ed) = fixTypedNode item ed
fixMainListElem' item@(PaginatedWorkflowsNode ed) = fixTypedNode item ed
fixMainListElem' item@(SingleIssueNode ed) = fixChildlessNode item ed
fixMainListElem' item@(SinglePullNode ed) = fixChildlessNode item ed
fixMainListElem' item@(SingleWorkflowNode ed) = fixTypedNode item ed
fixMainListElem' item@(SingleJobNode ed) = fixTypedNode item ed
fixMainListElem' item@(JobLogGroupNode ed) = fixTypedNode item ed
fixMainListElem' item@(HeadingNode ed) = fixWrappedNode item ed
fixMainListElem' item@(RepoNode ed) = fixWrappedNode item ed

fixTypedNode :: (
  NodeChildType Variable a1 ~ MainListElem' Variable a2
  , NodeChildType Fixed a1 ~ MainListElem' Fixed a2
  ) => MainListElem' f a1 -> EntityData Variable a1 -> STM (MainListElem' Fixed a1)
fixTypedNode item ed = (`setEntityData` item) <$> (readTVar (_children ed) >>= mapM fixMainListElem' >>= flip fixEntityData ed)

fixChildlessNode :: (
  NodeChildType Variable a ~ (), NodeChildType Fixed a ~ ()
  ) => MainListElem' f a -> EntityData Variable a -> STM (MainListElem' Fixed a)
fixChildlessNode item ed = (`setEntityData` item) <$> (fixEntityData [] ed)

fixWrappedNode :: (
  NodeChildType Variable a ~ MainListElemVariable
  , NodeChildType Fixed a ~ MainListElem
  ) => MainListElem' f a -> EntityData Variable a -> STM (MainListElem' Fixed a)
fixWrappedNode item ed = (`setEntityData` item) <$> (readTVar (_children ed) >>= mapM fixMainListElem >>= flip fixEntityData ed)

fixEntityData :: [NodeChildType Fixed a] -> EntityData Variable a -> STM (EntityData Fixed a)
fixEntityData childrenFixed (EntityData {..}) = do
  stateFixed <- readTVar _state
  toggledFixed <- readTVar _toggled
  searchFixed <- readTVar _search
  pageInfoFixed <- readTVar _pageInfo
  healthCheckFixed <- readTVar _healthCheck

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
