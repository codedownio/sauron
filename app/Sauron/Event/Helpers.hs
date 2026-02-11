{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Sauron.Event.Helpers (
  withFixedElemAndParents
  , withNthChildAndPaginationParent
  , withRepoParent
  , withNthChildAndRepoParent

  , getElemAndParents

  , hasPaginationParent
  , hasRepoParent

  , isPaginationNode
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
      atomically (nthChildVector readTVar getExistentialChildrenWrapped n (s ^. appMainListVariable)) >>= \case
        Nothing -> return Nothing
        Just elems -> return $ Just (fixedElem, (head elems), elems)

getElemAndParents :: AppState -> Maybe (NonEmpty (SomeNode Fixed))
getElemAndParents s =
  case listSelectedElement (s ^. appMainList) of
    Nothing -> Nothing
    Just (n, fixedElem) ->
      case runIdentity (nthChildVector return getExistentialChildrenWrappedIdentity n (listElements (s ^. appMainList))) of
        Nothing -> Nothing
        Just elems -> Just (fixedElem :| toList elems)

withFixedElemAndParents :: (
  MonadIO m
  ) => AppState -> (SomeNode Fixed -> SomeNode Variable -> NonEmpty (SomeNode Variable) -> m ()) -> m ()
withFixedElemAndParents s cb =
  getFixedElemAndParents s >>= \case
    Nothing -> return ()
    Just (x, y, z) -> cb x y z

withNthChildAndPaginationParent :: (
  MonadIO m
  )
  => AppState
  -> (SomeNode Fixed -> SomeNode Variable -> (SomeNode Variable, STM PageInfo, PageInfo -> STM ()) -> NonEmpty (SomeNode Variable) -> m ())
  -> m ()
withNthChildAndPaginationParent s cb =
  withFixedElemAndParents s $ \fixedEl variableEl parents ->
    case L.dropWhile (not . isSomeNodePagination) (toList parents) of
      (el@(getPaginationInfo -> Just (readPageInfo, writePageInfo)):rest) ->
        cb fixedEl variableEl (el, readPageInfo, writePageInfo) (el :| rest)
      _ -> return ()

hasPaginationParent :: (
  MonadIO m
  ) => AppState -> m Bool
hasPaginationParent s =
  getFixedElemAndParents s >>= \case
    Nothing -> return False
    Just (_, _, parents) -> return $ any isSomeNodePagination $ toList parents

isSomeNodePagination :: SomeNode f -> Bool
isSomeNodePagination (SomeNode node) = isPaginationNode node

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

-- | Check if a node is a pagination node (works for both Fixed and Variable)
isPaginationNode :: Node f a -> Bool
isPaginationNode node = isJust (getPaginationState node)

getPaginationInfo :: SomeNode Variable -> Maybe (STM PageInfo, PageInfo -> STM ())
getPaginationInfo (SomeNode node) = makePaginationActions <$> getPaginationState node
  where
    makePaginationActions :: TVar (Search, PageInfo, Fetchable Int) -> (STM PageInfo, PageInfo -> STM ())
    makePaginationActions stateVar = (readPageInfo, writePageInfo)
      where
        readPageInfo = snd3 <$> readTVar stateVar

        writePageInfo newPageInfo = do
          (search, _, fetchable) <- readTVar stateVar
          writeTVar stateVar (search, newPageInfo, fetchable)

        snd3 :: (a, b, c) -> b
        snd3 (_, b, _) = b

getPaginationState :: Node f a -> Maybe (Switchable f (Search, PageInfo, Fetchable TotalCount))
getPaginationState (PaginatedIssuesNode (EntityData {_state})) = Just _state
getPaginationState (PaginatedPullsNode (EntityData {_state})) = Just _state
getPaginationState (PaginatedWorkflowsNode (EntityData {_state})) = Just _state
getPaginationState (PaginatedReposNode (EntityData {_state})) = Just _state
getPaginationState (PaginatedYourBranchesNode (EntityData {_state})) = Just _state
getPaginationState (PaginatedActiveBranchesNode (EntityData {_state})) = Just _state
getPaginationState (PaginatedStaleBranchesNode (EntityData {_state})) = Just _state
getPaginationState (PaginatedNotificationsNode (EntityData {_state})) = Just _state
getPaginationState (PaginatedBranchesNode (EntityData {_state})) = Just _state
getPaginationState _ = Nothing

-- * Computing nth child in the presence of expanding

type GetExistentialChildrenFn f m = forall a. Node f a -> m [SomeNode f]

-- | Returns the node at the head of the list, and then its successive parents
-- going up to a tree root.
nthChildVector :: Monad m => (Switchable f Bool -> m Bool) -> GetExistentialChildrenFn f m -> Int -> V.Vector (SomeNode f) -> m (Maybe (NonEmpty (SomeNode f)))
nthChildVector isToggled gecf n elems = nthChildList isToggled gecf n (V.toList elems) >>= \case
  Left _ -> pure Nothing
  Right x -> pure (Just (NE.reverse x))

nthChildList :: Monad m => (Switchable f Bool -> m Bool) -> GetExistentialChildrenFn f m -> Int -> [SomeNode f] -> m (Either Int (NonEmpty (SomeNode f)))
nthChildList isToggled gecf n (x:xs) = nthChild isToggled gecf n x >>= \case
  Right els -> pure $ Right els
  Left n' -> nthChildList isToggled gecf n' xs
nthChildList _ _ n [] = pure $ Left n

nthChild :: Monad m => (Switchable f Bool -> m Bool) -> GetExistentialChildrenFn f m -> Int -> SomeNode f -> m (Either Int (NonEmpty (SomeNode f)))
nthChild _ _ 0 el = pure $ Right (el :| [])
nthChild isToggled gecf n el@(SomeNode item@(getEntityData -> (EntityData {..}))) = isToggled _toggled >>= \case
  True -> do
    wrappedChildren <- gecf item
    fmap ((el :|) . toList) <$> nthChildList isToggled gecf (n - 1) wrappedChildren
  False -> pure $ Left (n - 1)
