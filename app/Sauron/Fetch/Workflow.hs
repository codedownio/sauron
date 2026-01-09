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
  fetchPaginatedWithState (workflowRunsR owner name mempty) _state $ \case
    Left err -> do
      (s, p, _) <- readTVar _state
      writeTVar _state (s, p, Errored err)
      writeTVar _children []
    Right (WithTotalCount results totalCount, newPageInfo) -> do
      (s, _, _) <- readTVar _state
      writeTVar _state (s, newPageInfo, Fetched totalCount)
      (writeTVar _children =<<) $ forM (V.toList results) $ \workflow@(WorkflowRun {}) ->
        SingleWorkflowNode <$> makeEmptyElemWithState bc workflow NotFetched "" (_depth + 1)
