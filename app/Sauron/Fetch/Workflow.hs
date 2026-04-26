{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Sauron.Fetch.Workflow (
  fetchWorkflows
  ) where

import Control.Monad (foldM)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import qualified Data.Map.Strict as Map
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

      -- Preserve existing workflow nodes (and their health check threads, toggle state etc.)
      existingChildren <- readTVar _children
      existingWorkflowsMap <- (\cb -> foldM cb mempty existingChildren) $ \acc node -> case node of
        SingleWorkflowNode (EntityData {_static=existingWf}) ->
          return $ Map.insert (workflowRunWorkflowRunId existingWf) node acc

      (writeTVar _children =<<) $ forM (V.toList results) $ \workflow@(WorkflowRun {workflowRunWorkflowRunId=wfId}) ->
        case Map.lookup wfId existingWorkflowsMap of
          Just (SingleWorkflowNode existingEd) ->
            return $ SingleWorkflowNode (existingEd { _static = workflow })
          Nothing ->
            SingleWorkflowNode <$> makeEmptyElemWithState bc workflow NotFetched "" (_depth + 1)
