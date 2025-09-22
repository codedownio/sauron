{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Sauron.Event.Helpers where

import Brick.Widgets.List
import Control.Monad
import Control.Monad.IO.Unlift
import Data.Function
import Data.Typeable
import GitHub
import Lens.Micro
import Relude hiding (Down, pred)
import Sauron.Expanding
import Sauron.Types


withFixedElemAndParents :: (
  MonadIO m
  ) => AppState -> (MainListElem -> MainListElemVariable -> NonEmpty MainListElemVariable -> m ()) -> m ()
withFixedElemAndParents s cb = do
  case listSelectedElement (s ^. appMainList) of
    Nothing -> return ()
    Just (n, fixedElem) ->
      atomically (nthChildVector n (s ^. appMainListVariable)) >>= \case
        Nothing -> return ()
        Just elems -> cb fixedElem (last elems) elems

withNthChildAndMaybeRepoParent :: (
  MonadIO m
  ) => AppState -> (MainListElem -> MainListElemVariable -> Maybe (MainListElem' Variable 'RepoNodeT) -> m ()) -> m ()
withNthChildAndMaybeRepoParent s cb =
  withFixedElemAndParents s $ \fixedEl _variableEl elems ->
    cb fixedEl (last elems) (viaNonEmpty head [x | (SomeMainListElem (cast -> Just x@(MainListElemItem {_typ=(RepoNode {})} :: MainListElem' Variable RepoNodeT))) <- toList elems])

withNthChildAndMaybePaginationParent :: (
  MonadIO m
  ) => AppState -> (MainListElem -> MainListElemVariable -> Maybe MainListElemVariable -> m ()) -> m ()
withNthChildAndMaybePaginationParent s cb =
  withFixedElemAndParents s $ \fixedEl _variableEl elems ->
    cb fixedEl (last elems) (viaNonEmpty head [x | x <- toList elems])

withNthChild :: MonadIO m => AppState -> (MainListElem -> MainListElemVariable -> m ()) -> m ()
withNthChild s cb = withNthChildAndMaybeRepoParent s $ \fixedEl el _ -> cb fixedEl el

withRepoParent :: MonadIO m => AppState -> (Repo -> m ()) -> m ()
withRepoParent s cb = do
  withNthChildAndMaybeRepoParent s $ \_ _ repoElem -> case repoElem of
    Just (MainListElemItem {_typ=(RepoNode {}), _state}) -> readTVarIO _state >>= \case
      Fetched r -> cb r
      _ -> return ()
    _ -> return ()

withNthChildAndRepoParent :: MonadIO m => AppState -> (MainListElem -> MainListElemVariable -> MainListElem' Variable RepoNodeT -> m ()) -> m ()
withNthChildAndRepoParent s cb = withNthChildAndMaybeRepoParent s $ \fixedEl el -> \case
  Nothing -> return ()
  Just x -> cb fixedEl el x

-- withElemByIdentifier :: MonadIO m => AppState -> Int -> (Maybe MainListElemVariable -> m ()) -> m ()
-- withElemByIdentifier s identifier cb =
--   atomically (findElemInList (\x -> _ident x == identifier) (V.toList (_appMainListVariable s))) >>= cb

-- findElem :: (MainListElemVariable -> Bool) -> MainListElemVariable -> STM (Maybe MainListElemVariable)
-- findElem pred el | pred el = pure $ Just el
-- findElem pred (MainListElemPaginated {..}) = readTVar _children >>= findElemInList pred
-- findElem pred (MainListElemRepo {..}) = do
--   ic <- readTVar _issuesChild
--   pc <- readTVar _pullsChild
--   wc <- readTVar _workflowsChild
--   findElemInList pred [ic, pc, wc]
-- findElem _ _ = pure Nothing

-- findElemInList :: (MainListElemVariable -> Bool) -> [MainListElemVariable] -> STM (Maybe MainListElemVariable)
-- findElemInList pred elems = flip fix elems $ \loop -> \case
--   (x:xs) -> findElem pred x >>= \case
--     Just x' -> pure (Just x')
--     Nothing -> loop xs
--   [] -> pure Nothing
