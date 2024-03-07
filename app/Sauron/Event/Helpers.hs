{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Sauron.Event.Helpers where

import Brick.Widgets.List
import Control.Monad
import Control.Monad.IO.Unlift
import Data.Function
import GitHub
import Lens.Micro
import Relude hiding (Down)
import Sauron.Actions
import Sauron.Expanding
import Sauron.Types


withNthChildAndMaybeRepoParent :: (
  MonadIO m
  ) => AppState -> (MainListElem -> MainListElemVariable -> Maybe MainListElemVariable -> m ()) -> m ()
withNthChildAndMaybeRepoParent s cb = do
  case listSelectedElement (s ^. appMainList) of
    Nothing -> return ()
    Just (n, fixedElem) ->
      atomically (nthChildVector n (s ^. appMainListVariable)) >>= \case
        Nothing -> return ()
        Just elems -> cb fixedElem (last elems) (viaNonEmpty head [x | x@(MainListElemRepo {}) <- toList elems])

withNthChild :: MonadIO m => AppState -> (MainListElem -> MainListElemVariable -> m ()) -> m ()
withNthChild s cb = withNthChildAndMaybeRepoParent s $ \fixedEl el _ -> cb fixedEl el

withNthChildAndRepoParent :: MonadIO m => AppState -> (MainListElem -> MainListElemVariable -> MainListElemVariable -> m ()) -> m ()
withNthChildAndRepoParent s cb = withNthChildAndMaybeRepoParent s $ \fixedEl el -> \case
  Nothing -> return ()
  Just x -> cb fixedEl el x

openBrowserToItem :: MonadIO m => PaginatedItem -> m ()
openBrowserToItem (PaginatedItemIssue (Issue {issueHtmlUrl=(Just url)})) = openBrowserToUrl (toString (getUrl url))
openBrowserToItem (PaginatedItemWorkflow (WorkflowRun {workflowRunHtmlUrl=url})) = openBrowserToUrl (toString (getUrl url))
openBrowserToItem _ = return ()
