{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Sauron.Fetch.Workflow (
  fetchWorkflows
  ) where

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import qualified Data.Vector as V
import GitHub
import Relude
import Sauron.Fetch.Core
import Sauron.Types

fetchWorkflows :: (
  HasCallStack, MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Node Variable PaginatedWorkflowsT -> m ()
fetchWorkflows owner name (PaginatedWorkflowsNode (EntityData {..})) = do
  bc <- ask
  fetchPaginated'' (workflowRunsR owner name mempty) _pageInfo _state $ \case
    Left err -> do
      writeTVar _state (Errored (show err))
      writeTVar _children []
    Right (wtc@(WithTotalCount results _totalCount), newPageInfo) -> do
      writeTVar _pageInfo newPageInfo
      writeTVar _state (Fetched wtc)
      (writeTVar _children =<<) $ forM (V.toList results) $ \workflow@(WorkflowRun {}) ->
        SingleWorkflowNode <$> makeEmptyElem bc workflow "" (_depth + 1)
