{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Sauron.Event.Search (
  updateSearchForNode
  , getCurrentSearch
) where

import Relude
import Sauron.Types


fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

updateSearchInTuple :: TVar (Search, PageInfo, Fetchable Int) -> Text -> STM ()
updateSearchInTuple stateVar searchText = do
  (_, _, fetchable) <- readTVar stateVar
  writeTVar stateVar (SearchText searchText, PageInfo 1 Nothing Nothing Nothing Nothing, fetchable)

updateSearchForNode :: Node Variable a -> Text -> STM ()
updateSearchForNode (PaginatedIssuesNode (EntityData {_state})) searchText = updateSearchInTuple _state searchText
updateSearchForNode (PaginatedPullsNode (EntityData {_state})) searchText = updateSearchInTuple _state searchText
updateSearchForNode (PaginatedWorkflowsNode (EntityData {_state})) searchText = updateSearchInTuple _state searchText
updateSearchForNode (PaginatedReposNode (EntityData {_state})) searchText = updateSearchInTuple _state searchText
updateSearchForNode (PaginatedYourBranchesNode (EntityData {_state})) searchText = updateSearchInTuple _state searchText
updateSearchForNode (PaginatedActiveBranchesNode (EntityData {_state})) searchText = updateSearchInTuple _state searchText
updateSearchForNode (PaginatedStaleBranchesNode (EntityData {_state})) searchText = updateSearchInTuple _state searchText
updateSearchForNode (PaginatedNotificationsNode (EntityData {_state})) searchText = updateSearchInTuple _state searchText
updateSearchForNode (PaginatedBranchesNode (EntityData {_state})) searchText = updateSearchInTuple _state searchText
updateSearchForNode _ _ = return () -- Non-searchable nodes do nothing

getCurrentSearch :: Node Variable a -> STM Search
getCurrentSearch (PaginatedIssuesNode (EntityData {_state})) = fst3 <$> readTVar _state
getCurrentSearch (PaginatedPullsNode (EntityData {_state})) = fst3 <$> readTVar _state
getCurrentSearch (PaginatedWorkflowsNode (EntityData {_state})) = fst3 <$> readTVar _state
getCurrentSearch (PaginatedReposNode (EntityData {_state})) = fst3 <$> readTVar _state
getCurrentSearch (PaginatedYourBranchesNode (EntityData {_state})) = fst3 <$> readTVar _state
getCurrentSearch (PaginatedActiveBranchesNode (EntityData {_state})) = fst3 <$> readTVar _state
getCurrentSearch (PaginatedStaleBranchesNode (EntityData {_state})) = fst3 <$> readTVar _state
getCurrentSearch (PaginatedNotificationsNode (EntityData {_state})) = fst3 <$> readTVar _state
getCurrentSearch (PaginatedBranchesNode (EntityData {_state})) = fst3 <$> readTVar _state
getCurrentSearch _ = return SearchNone
