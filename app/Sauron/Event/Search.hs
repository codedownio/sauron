{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Sauron.Event.Search (
  updateSearchForNode
  , getCurrentSearch
  , ensureNonEmptySearch
) where

import Lens.Micro
import Relude
import Sauron.Types


updateSearchForNode :: Node Variable a -> Search -> STM ()
updateSearchForNode (PaginatedIssuesNode (EntityData {_state})) search = modifyTVar' _state (set _1 search)
updateSearchForNode (PaginatedPullsNode (EntityData {_state})) search = modifyTVar' _state (set _1 search)
updateSearchForNode (PaginatedWorkflowsNode (EntityData {_state})) search = modifyTVar' _state (set _1 search)
updateSearchForNode (PaginatedReposNode (EntityData {_state})) search = modifyTVar' _state (set _1 search)
updateSearchForNode (PaginatedYourBranchesNode (EntityData {_state})) search = modifyTVar' _state (set _1 search)
updateSearchForNode (PaginatedActiveBranchesNode (EntityData {_state})) search = modifyTVar' _state (set _1 search)
updateSearchForNode (PaginatedStaleBranchesNode (EntityData {_state})) search = modifyTVar' _state (set _1 search)
updateSearchForNode (PaginatedNotificationsNode (EntityData {_state})) search = modifyTVar' _state (set _1 search)
updateSearchForNode (PaginatedBranchesNode (EntityData {_state})) search = modifyTVar' _state (set _1 search)
updateSearchForNode _ _ = return () -- Non-searchable nodes do nothing

getCurrentSearch :: Node Variable a -> STM Search
getCurrentSearch (PaginatedIssuesNode (EntityData {_state})) = (^. _1) <$> readTVar _state
getCurrentSearch (PaginatedPullsNode (EntityData {_state})) = (^. _1) <$> readTVar _state
getCurrentSearch (PaginatedWorkflowsNode (EntityData {_state})) = (^. _1) <$> readTVar _state
getCurrentSearch (PaginatedReposNode (EntityData {_state})) = (^. _1) <$> readTVar _state
getCurrentSearch (PaginatedYourBranchesNode (EntityData {_state})) = (^. _1) <$> readTVar _state
getCurrentSearch (PaginatedActiveBranchesNode (EntityData {_state})) = (^. _1) <$> readTVar _state
getCurrentSearch (PaginatedStaleBranchesNode (EntityData {_state})) = (^. _1) <$> readTVar _state
getCurrentSearch (PaginatedNotificationsNode (EntityData {_state})) = (^. _1) <$> readTVar _state
getCurrentSearch (PaginatedBranchesNode (EntityData {_state})) = (^. _1) <$> readTVar _state
getCurrentSearch _ = return SearchNone

ensureNonEmptySearch :: Node Variable a -> STM Text
ensureNonEmptySearch node = getCurrentSearch node >>= \case
  SearchNone -> do
    updateSearchForNode node (SearchText "")
    return ""
  SearchText t -> return t
