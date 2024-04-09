{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Sauron.Event.Helpers where

import Brick.Widgets.List
import Control.Monad
import Control.Monad.IO.Unlift
import Data.Function
import qualified Data.Vector as V
import GitHub
import Lens.Micro
import Relude hiding (Down, pred)
import Sauron.Actions
import Sauron.Expanding
import Sauron.Types


withFixedElemAndParents :: (
  MonadIO m
  ) => AppState -> (MainListElem -> (NonEmpty MainListElemVariable) -> m ()) -> m ()
withFixedElemAndParents s cb = do
  case listSelectedElement (s ^. appMainList) of
    Nothing -> return ()
    Just (n, fixedElem) ->
      atomically (nthChildVector n (s ^. appMainListVariable)) >>= \case
        Nothing -> return ()
        Just elems -> cb fixedElem elems

withNthChildAndMaybeRepoParent :: (
  MonadIO m
  ) => AppState -> (MainListElem -> MainListElemVariable -> Maybe MainListElemVariable -> m ()) -> m ()
withNthChildAndMaybeRepoParent s cb =
  withFixedElemAndParents s $ \fixedEl elems ->
    cb fixedEl (last elems) (viaNonEmpty head [x | x@(MainListElemRepo {}) <- toList elems])

withNthChildAndMaybePaginationParent :: (
  MonadIO m
  ) => AppState -> (MainListElem -> MainListElemVariable -> Maybe MainListElemVariable -> m ()) -> m ()
withNthChildAndMaybePaginationParent s cb =
  withFixedElemAndParents s $ \fixedEl elems ->
    cb fixedEl (last elems) (viaNonEmpty head [x | x@(MainListElemPaginated {}) <- toList elems])

withNthChild :: MonadIO m => AppState -> (MainListElem -> MainListElemVariable -> m ()) -> m ()
withNthChild s cb = withNthChildAndMaybeRepoParent s $ \fixedEl el _ -> cb fixedEl el

withRepoParent :: MonadIO m => AppState -> (Repo -> m ()) -> m ()
withRepoParent s cb = do
  withNthChildAndMaybeRepoParent s $ \_ _ repoElem -> case repoElem of
    Just (MainListElemRepo {_repo}) -> readTVarIO _repo >>= \case
      Fetched r -> cb r
      _ -> return ()
    _ -> return ()

withNthChildAndRepoParent :: MonadIO m => AppState -> (MainListElem -> MainListElemVariable -> MainListElemVariable -> m ()) -> m ()
withNthChildAndRepoParent s cb = withNthChildAndMaybeRepoParent s $ \fixedEl el -> \case
  Nothing -> return ()
  Just x -> cb fixedEl el x

withElemByIdentifier :: MonadIO m => AppState -> Int -> (Maybe MainListElemVariable -> m ()) -> m ()
withElemByIdentifier s identifier cb =
  atomically (findElemInList (\x -> _ident x == identifier) (V.toList (_appMainListVariable s))) >>= cb

findElem :: (MainListElemVariable -> Bool) -> MainListElemVariable -> STM (Maybe MainListElemVariable)
findElem pred el | pred el = pure $ Just el
findElem pred (MainListElemPaginated {..}) = readTVar _children >>= findElemInList pred
findElem pred (MainListElemRepo {..}) = do
  ic <- readTVar _issuesChild
  pc <- readTVar _pullsChild
  wc <- readTVar _workflowsChild
  findElemInList pred [ic, pc, wc]
findElem _ _ = pure Nothing

findElemInList :: (MainListElemVariable -> Bool) -> [MainListElemVariable] -> STM (Maybe MainListElemVariable)
findElemInList pred elems = flip fix elems $ \loop -> \case
  (x:xs) -> findElem pred x >>= \case
    Just x' -> pure (Just x')
    Nothing -> loop xs
  [] -> pure Nothing

openBrowserToItem :: MonadIO m => PaginatedItem -> m ()
openBrowserToItem (PaginatedItemIssue (Issue {issueHtmlUrl=(Just url)})) = openBrowserToUrl (toString (getUrl url))
openBrowserToItem (PaginatedItemPull (Issue {issueHtmlUrl=(Just url)})) = openBrowserToUrl (toString (getUrl url))
openBrowserToItem (PaginatedItemWorkflow (WorkflowRun {workflowRunHtmlUrl=url})) = openBrowserToUrl (toString (getUrl url))
openBrowserToItem _ = return ()
