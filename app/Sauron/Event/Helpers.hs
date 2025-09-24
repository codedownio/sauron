{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Sauron.Event.Helpers where

import Brick.Widgets.List
import Control.Monad
import Control.Monad.IO.Unlift
import Data.Function
import GitHub
import Lens.Micro
import Relude hiding (Down, pred)
import Sauron.Expanding
import Sauron.Types


withFixedElemAndParents :: (
  MonadIO m
  ) => AppState -> (SomeNode Fixed -> SomeNode Variable -> NonEmpty (SomeNode Variable) -> m ()) -> m ()
withFixedElemAndParents s cb = do
  case listSelectedElement (s ^. appMainList) of
    Nothing -> return ()
    Just (n, fixedElem) ->
      atomically (nthChildVector n (s ^. appMainListVariable)) >>= \case
        Nothing -> return ()
        Just elems -> cb fixedElem (last elems) elems

withNthChildAndMaybeRepoParent :: (
  MonadIO m
  ) => AppState -> (SomeNode Fixed -> SomeNode Variable -> Maybe (Node Variable 'RepoT) -> m ()) -> m ()
withNthChildAndMaybeRepoParent s cb =
  withFixedElemAndParents s $ \fixedEl _variableEl elems ->
    cb fixedEl (last elems) (viaNonEmpty head [x | (SomeNode x@(RepoNode {})) <- toList elems])

withNthChildAndMaybePaginationParent :: (
  MonadIO m
  ) => AppState -> (SomeNode Fixed -> SomeNode Variable -> Maybe (SomeNode Variable) -> m ()) -> m ()
withNthChildAndMaybePaginationParent s cb =
  withFixedElemAndParents s $ \fixedEl _variableEl elems ->
    cb fixedEl (last elems) (viaNonEmpty head [x | x <- toList elems])

withNthChild :: MonadIO m => AppState -> (SomeNode Fixed -> SomeNode Variable -> m ()) -> m ()
withNthChild s cb = withNthChildAndMaybeRepoParent s $ \fixedEl el _ -> cb fixedEl el

withRepoParent :: MonadIO m => AppState -> (Repo -> m ()) -> m ()
withRepoParent s cb = do
  withNthChildAndMaybeRepoParent s $ \_ _ repoElem -> case repoElem of
    Just (RepoNode (EntityData {_state})) -> readTVarIO _state >>= \case
      Fetched r -> cb r
      _ -> return ()
    _ -> return ()

withNthChildAndRepoParent :: MonadIO m => AppState -> (SomeNode Fixed -> SomeNode Variable -> Node Variable RepoT -> m ()) -> m ()
withNthChildAndRepoParent s cb = withNthChildAndMaybeRepoParent s $ \fixedEl el -> \case
  Nothing -> return ()
  Just x -> cb fixedEl el x

-- withElemByIdentifier :: MonadIO m => AppState -> Int -> (Maybe (SomeNode Variable) -> m ()) -> m ()
-- withElemByIdentifier s identifier cb =
--   atomically (findElemInList (\x -> _ident x == identifier) (V.toList (_appMainListVariable s))) >>= cb

-- findElem :: (SomeNode Variable -> Bool) -> SomeNode Variable -> STM (Maybe (SomeNode Variable))
-- findElem pred el | pred el = pure $ Just el
-- findElem pred (MainListElemPaginated {..}) = readTVar _children >>= findElemInList pred
-- findElem pred (MainListElemRepo {..}) = do
--   ic <- readTVar _issuesChild
--   pc <- readTVar _pullsChild
--   wc <- readTVar _workflowsChild
--   findElemInList pred [ic, pc, wc]
-- findElem _ _ = pure Nothing

-- findElemInList :: (SomeNode Variable -> Bool) -> [SomeNode Variable] -> STM (Maybe (SomeNode Variable))
-- findElemInList pred elems = flip fix elems $ \loop -> \case
--   (x:xs) -> findElem pred x >>= \case
--     Just x' -> pure (Just x')
--     Nothing -> loop xs
--   [] -> pure Nothing
