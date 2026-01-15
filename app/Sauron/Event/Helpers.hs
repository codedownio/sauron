{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Sauron.Event.Helpers (
  withFixedElemAndParents
  , withNthChildAndPaginationParent
  , withRepoParent
  , withNthChildAndRepoParent

  , hasPaginationParent
  , hasRepoParent
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


getFixedElemAndParents :: (
  MonadIO m
  ) => AppState -> m (Maybe (SomeNode Fixed, SomeNode Variable, NonEmpty (SomeNode Variable)))
getFixedElemAndParents s =
  case listSelectedElement (s ^. appMainList) of
    Nothing -> return Nothing
    Just (n, fixedElem) ->
      atomically (nthChildVector n (s ^. appMainListVariable)) >>= \case
        Nothing -> return Nothing
        Just elems -> return $ Just (fixedElem, (head elems), elems)

withFixedElemAndParents :: (
  MonadIO m
  ) => AppState -> (SomeNode Fixed -> SomeNode Variable -> NonEmpty (SomeNode Variable) -> m ()) -> m ()
withFixedElemAndParents s cb =
  getFixedElemAndParents s >>= \case
    Nothing -> return ()
    Just (x, y, z) -> cb x y z

withNthChildAndPaginationParent :: (
  MonadIO m
  ) => AppState -> (SomeNode Fixed -> SomeNode Variable -> (SomeNode Variable, STM PageInfo, PageInfo -> STM ()) -> NonEmpty (SomeNode Variable) -> m ()) -> m ()
withNthChildAndPaginationParent s cb =
  withFixedElemAndParents s $ \fixedEl variableEl parents ->
    case L.dropWhile (not . isPaginationNode) (toList parents) of
      (el@(getPaginationInfo -> Just (readPageInfo, writePageInfo)):rest) ->
        cb fixedEl variableEl (el, readPageInfo, writePageInfo) (el :| rest)
      _ -> return ()

hasPaginationParent :: (
  MonadIO m
  ) => AppState -> m Bool
hasPaginationParent s =
  getFixedElemAndParents s >>= \case
    Nothing -> return False
    Just (_, _, parents) -> return $ any isPaginationNode $ toList parents

withNthChildAndRepoParent :: MonadIO m => AppState -> (SomeNode Fixed -> SomeNode Variable -> Node Variable RepoT -> m ()) -> m ()
withNthChildAndRepoParent s cb =
  withFixedElemAndParents s $ \fixedEl _variableEl elems ->
    case viaNonEmpty head [x | (SomeNode x@(RepoNode {})) <- toList elems] of
      Nothing -> return ()
      Just repoNode -> cb fixedEl (head elems) repoNode

hasRepoParent :: (
  MonadIO m
  ) => AppState -> m Bool
hasRepoParent s =
  getFixedElemAndParents s >>= \case
    Nothing -> return False
    Just (_, _, parents) ->
      case viaNonEmpty head [x | (SomeNode x@(RepoNode {})) <- toList parents] of
        Nothing -> return False
        Just _repoNode -> return True

withRepoParent :: MonadIO m => AppState -> (Repo -> m ()) -> m ()
withRepoParent s cb = do
  withNthChildAndRepoParent s $ \_ _ (RepoNode (EntityData {_state})) -> readTVarIO _state >>= \case
    Fetched r -> cb r
    _ -> return ()

-- * Util

isPaginationNode :: SomeNode Variable -> Bool
isPaginationNode = isJust . getPaginationInfo

getPaginationInfo :: SomeNode Variable -> Maybe (STM PageInfo, PageInfo -> STM ())
getPaginationInfo (SomeNode (PaginatedIssuesNode (EntityData {_state}))) = Just (makePaginationActions _state)
getPaginationInfo (SomeNode (PaginatedPullsNode (EntityData {_state}))) = Just (makePaginationActions _state)
getPaginationInfo (SomeNode (PaginatedWorkflowsNode (EntityData {_state}))) = Just (makePaginationActions _state)
getPaginationInfo (SomeNode (PaginatedReposNode (EntityData {_state}))) = Just (makePaginationActions _state)
getPaginationInfo (SomeNode (PaginatedYourBranchesNode (EntityData {_state}))) = Just (makePaginationActions _state)
getPaginationInfo (SomeNode (PaginatedActiveBranchesNode (EntityData {_state}))) = Just (makePaginationActions _state)
getPaginationInfo (SomeNode (PaginatedStaleBranchesNode (EntityData {_state}))) = Just (makePaginationActions _state)
getPaginationInfo (SomeNode (PaginatedNotificationsNode (EntityData {_state}))) = Just (makePaginationActions _state)
getPaginationInfo (SomeNode (PaginatedBranchesNode (EntityData {_state}))) = Just (makePaginationActions _state)
getPaginationInfo _ = Nothing

makePaginationActions :: TVar (Search, PageInfo, Fetchable Int) -> (STM PageInfo, PageInfo -> STM ())
makePaginationActions stateVar = (readPageInfo, writePageInfo)
  where
    readPageInfo = snd3 <$> readTVar stateVar

    writePageInfo newPageInfo = do
      (search, _, fetchable) <- readTVar stateVar
      writeTVar stateVar (search, newPageInfo, fetchable)

    snd3 :: (a, b, c) -> b
    snd3 (_, b, _) = b

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
