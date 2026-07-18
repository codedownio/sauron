{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Sauron.Expanding (
  getExpandedList
  ) where

import Control.Monad.Writer
import Data.Time.Clock (UTCTime)
import qualified Data.Vector as V
import Relude
import Sauron.Types
import Sauron.Workflow.Sorting


getExpandedList :: UTCTime -> V.Vector (SomeNode Fixed) -> V.Vector (SomeNode Fixed)
getExpandedList now = V.fromList . concatMap expandNodes . V.toList
  where
    expandNodes :: (SomeNode Fixed) -> [(SomeNode Fixed)]
    expandNodes x@(SomeNode item) = execWriter $ do
      tell [x]
      when (_toggled (getEntityData item)) $
        case item of
          PaginatedIssuesNode (EntityData {..}) -> expandTyped _children
          PaginatedPullsNode (EntityData {..}) -> expandTyped _children
          PaginatedWorkflowsNode (EntityData {..}) -> expandTyped _children
          PaginatedReposNode (EntityData {..}) -> expandTyped _children
          PaginatedBranchesNode (EntityData {..}) -> expandTyped _children
          PaginatedYourBranchesNode (EntityData {..}) -> expandTyped _children
          PaginatedActiveBranchesNode (EntityData {..}) -> expandTyped _children
          PaginatedStaleBranchesNode (EntityData {..}) -> expandTyped _children
          PaginatedNotificationsNode (EntityData {_state=(_, PageInfo {pageInfoCurrentPage}, _), ..}) -> expandTyped (paginateNotifications pageInfoCurrentPage _children)
          SingleIssueNode (EntityData {..}) -> expandChildless _children
          SinglePullNode (EntityData {..}) -> expandChildless _children
          SingleWorkflowNode (EntityData {..}) ->
            sortWorkflowJobsFixed (workflowNodeStateJobSortBy _state) now _children
            & paginateJobs (workflowNodeStateJobPage _state)
            & expandTyped
          SingleJobNode (EntityData {..}) -> expandTyped _children
          SingleBranchNode (EntityData {..}) -> expandTyped _children
          SingleBranchWithInfoNode (EntityData {..}) -> expandTyped _children
          SingleCommitNode (EntityData {..}) -> expandChildless _children
          SingleNotificationNode (EntityData {..}) -> expandChildless _children
          JobLogGroupNode (EntityData {..}) -> expandTyped _children
          HeadingNode (EntityData {..}) -> expandWrapped _children
          RepoNode (EntityData {..}) -> forM_ _children $ \y -> tell (expandNodes y)

    expandTyped :: (
      Foldable t, MonadWriter [SomeNode Fixed] m, Eq (Node Fixed a), Eq (NodeState a), Typeable a
      ) => t (Node Fixed a) -> m ()
    expandTyped xs = forM_ xs $ \y -> tell (expandNodes (SomeNode y))

    expandWrapped :: (Foldable t, MonadWriter [SomeNode Fixed] m) => t (SomeNode Fixed) -> m ()
    expandWrapped xs = forM_ xs $ \y -> tell (expandNodes y)

    expandChildless :: Monad m => [()] -> m ()
    expandChildless _xs = return ()
