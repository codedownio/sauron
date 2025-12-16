-- We have extra strict constraints on fixChildlessNode, to ensure it's only
-- used on nodes where NodeChildType f a ~ (), which normally GHC warns about.
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Sauron.Fix (
  fixSomeNode
  , fixModal
  ) where

import Relude
import Sauron.Types


fixModal :: ModalState Variable -> STM (ModalState Fixed)
fixModal (CommentModalState {..}) = return $ CommentModalState {
  _commentEditor = _commentEditor
  , _commentIssue = _commentIssue
  , _commentIssueComments = _commentIssueComments
  , _issueIsPR = _issueIsPR
  , _commentRepoOwner = _commentRepoOwner
  , _commentRepoName = _commentRepoName
  , _submissionState = _submissionState
  }
fixModal (ZoomModalState sn) = do
  fixedNode <- fixSomeNode sn
  return $ ZoomModalState fixedNode
fixModal (LogModalState logsList) = return (LogModalState logsList)

fixSomeNode :: SomeNode Variable -> STM (SomeNode Fixed)
fixSomeNode (SomeNode item) = SomeNode <$> fixNode item

fixNode :: Node Variable a -> STM (Node Fixed a)
fixNode item@(PaginatedIssuesNode ed) = fixTypedNode item ed
fixNode item@(PaginatedPullsNode ed) = fixTypedNode item ed
fixNode item@(PaginatedWorkflowsNode ed) = fixTypedNode item ed
fixNode item@(PaginatedReposNode ed) = fixTypedNode item ed
fixNode item@(PaginatedBranchesNode ed) = fixTypedNode item ed
fixNode item@(OverallBranchesNode ed) = fixWrappedNode item ed
fixNode item@(PaginatedYourBranchesNode ed) = fixTypedNode item ed
fixNode item@(PaginatedActiveBranchesNode ed) = fixTypedNode item ed
fixNode item@(PaginatedStaleBranchesNode ed) = fixTypedNode item ed
fixNode item@(PaginatedNotificationsNode ed) = fixTypedNode item ed
fixNode item@(SingleIssueNode ed) = fixChildlessNode item ed
fixNode item@(SinglePullNode ed) = fixChildlessNode item ed
fixNode item@(SingleWorkflowNode ed) = fixTypedNode item ed
fixNode item@(SingleJobNode ed) = fixTypedNode item ed
fixNode item@(SingleBranchNode ed) = fixTypedNode item ed
fixNode item@(SingleBranchWithInfoNode ed) = fixTypedNode item ed
fixNode item@(SingleCommitNode ed) = fixChildlessNode item ed
fixNode item@(SingleNotificationNode ed) = fixChildlessNode item ed
fixNode item@(JobLogGroupNode ed) = fixTypedNode item ed
fixNode item@(HeadingNode ed) = fixWrappedNode item ed
fixNode item@(RepoNode ed) = fixWrappedNode item ed

fixTypedNode :: (
  NodeChildType Variable a1 ~ Node Variable a2
  , NodeChildType Fixed a1 ~ Node Fixed a2
  ) => Node f a1 -> EntityData Variable a1 -> STM (Node Fixed a1)
fixTypedNode item ed = (`setEntityData` item) <$> (readTVar (_children ed) >>= mapM fixNode >>= flip fixEntityData ed)

fixChildlessNode :: (
  NodeChildType Variable a ~ (), NodeChildType Fixed a ~ ()
  ) => Node f a -> EntityData Variable a -> STM (Node Fixed a)
fixChildlessNode item ed = (`setEntityData` item) <$> (fixEntityData [] ed)

fixWrappedNode :: (
  NodeChildType Variable a ~ SomeNode Variable
  , NodeChildType Fixed a ~ SomeNode Fixed
  ) => Node f a -> EntityData Variable a -> STM (Node Fixed a)
fixWrappedNode item ed = (`setEntityData` item) <$> (readTVar (_children ed) >>= mapM fixSomeNode >>= flip fixEntityData ed)

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
