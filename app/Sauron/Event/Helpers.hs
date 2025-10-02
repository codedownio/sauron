{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Sauron.Event.Helpers (
  withFixedElemAndParents
  , withNthChildAndPaginationParent
  , withRepoParent
  , withNthChildAndRepoParent
  ) where

import Brick.Widgets.List
import Control.Monad
import Control.Monad.IO.Unlift
import Data.Function
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as V
import GitHub
import Lens.Micro
import Relude hiding (Down, pred)
import Sauron.Types


withFixedElemAndParents :: (
  MonadIO m
  ) => AppState -> (SomeNode Fixed -> SomeNode Variable -> NonEmpty (SomeNode Variable) -> m ()) -> m ()
withFixedElemAndParents s cb =
  case listSelectedElement (s ^. appMainList) of
    Nothing -> return ()
    Just (n, fixedElem) ->
      atomically (nthChildVector n (s ^. appMainListVariable)) >>= \case
        Nothing -> return ()
        Just elems -> cb fixedElem (head elems) elems

withNthChildAndPaginationParent :: (
  MonadIO m
  ) => AppState -> (SomeNode Fixed -> SomeNode Variable -> (SomeNode Variable, TVar PageInfo) -> NonEmpty (SomeNode Variable) -> m ()) -> m ()
withNthChildAndPaginationParent s cb =
  withFixedElemAndParents s $ \fixedEl variableEl parents ->
    case L.dropWhile (not . isPaginationNode) (toList parents) of
      (el@(getPaginationInfo -> Just pageInfo):rest) ->
        cb fixedEl variableEl (el, pageInfo) (el :| rest)
      _ -> return ()

withNthChildAndRepoParent :: MonadIO m => AppState -> (SomeNode Fixed -> SomeNode Variable -> Node Variable RepoT -> m ()) -> m ()
withNthChildAndRepoParent s cb =
  withFixedElemAndParents s $ \fixedEl _variableEl elems ->
    case viaNonEmpty head [x | (SomeNode x@(RepoNode {})) <- toList elems] of
      Nothing -> return ()
      Just repoNode -> cb fixedEl (head elems) repoNode

withRepoParent :: MonadIO m => AppState -> (Repo -> m ()) -> m ()
withRepoParent s cb = do
  withNthChildAndRepoParent s $ \_ _ (RepoNode (EntityData {_state})) -> readTVarIO _state >>= \case
    Fetched r -> cb r
    _ -> return ()

-- * Util

isPaginationNode :: SomeNode Variable -> Bool
isPaginationNode = isJust . getPaginationInfo

getPaginationInfo :: SomeNode Variable -> Maybe (TVar PageInfo)
getPaginationInfo (SomeNode (PaginatedIssuesNode (EntityData {..}))) = Just _pageInfo
getPaginationInfo (SomeNode (PaginatedPullsNode (EntityData {..}))) = Just _pageInfo
getPaginationInfo (SomeNode (PaginatedWorkflowsNode (EntityData {..}))) = Just _pageInfo
getPaginationInfo (SomeNode (PaginatedReposNode (EntityData {..}))) = Just _pageInfo
getPaginationInfo (SomeNode (PaginatedBranchesNode (EntityData {..}))) = Just _pageInfo
getPaginationInfo (SomeNode (PaginatedNotificationsNode (EntityData {..}))) = Just _pageInfo
getPaginationInfo _ = Nothing

-- * Computing nth child in the presence of expanding

-- | Returns the node at the head of the list, and then its successive parents
-- going up to a tree root.
nthChildVector :: Int -> V.Vector (SomeNode Variable) -> STM (Maybe (NonEmpty (SomeNode Variable)))
nthChildVector n elems = nthChildList n (V.toList elems) >>= \case
  Left _ -> pure Nothing
  Right x -> pure (Just (NE.reverse x))

nthChildList :: Int -> [SomeNode Variable] -> STM (Either Int (NonEmpty (SomeNode Variable)))
nthChildList n (x:xs) = nthChild n x >>= \case
  Right els -> pure $ Right els
  Left n' -> nthChildList n' xs
nthChildList n [] = pure $ Left n

nthChild :: Int -> SomeNode Variable -> STM (Either Int (NonEmpty (SomeNode Variable)))
nthChild 0 el = pure $ Right (el :| [])
nthChild n el@(SomeNode item@(getEntityData -> (EntityData {..}))) = readTVar _toggled >>= \case
  True -> do
    wrappedChildren <- getExistentialChildrenWrapped item
    (fmap ((el :|) . toList)) <$> (nthChildList (n - 1) wrappedChildren)
  False -> pure $ Left (n - 1)
