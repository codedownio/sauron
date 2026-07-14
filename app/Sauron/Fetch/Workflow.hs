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
import Sauron.HealthCheck.Stop (cancelGatheredHealthCheckThreads, swapChildrenClearingRemoved)
import Sauron.HealthCheck.Workflow (startWorkflowHealthCheckForNode)
import Sauron.Types

fetchWorkflows :: (
  HasCallStack, MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Node Variable PaginatedWorkflowsT -> m ()
fetchWorkflows owner name (PaginatedWorkflowsNode (EntityData {..})) = do
  bc <- ask

  -- Swap in the new page of workflows, stopping the health checks of any that dropped out of the
  -- list (e.g. after paging). Reused workflows keep their threads; only removed ones are stopped,
  -- so paging back won't spawn a second thread for the same workflow.
  removedThreads <- fetchPaginatedWithState (workflowRunsR owner name mempty) _state $ \case
    Left err -> do
      (s, p, _) <- readTVar _state
      writeTVar _state (s, p, Errored err)
      swapChildrenClearingRemoved _children []
    Right (WithTotalCount results totalCount, newPageInfo) -> do
      (s, _, _) <- readTVar _state
      writeTVar _state (s, newPageInfo, Fetched totalCount)

      -- Preserve existing workflow nodes (and their health check threads, toggle state etc.)
      existingChildren <- readTVar _children
      existingWorkflowsMap <- (\cb -> foldM cb mempty existingChildren) $ \acc node -> case node of
        SingleWorkflowNode (EntityData {_static=existingWf}) ->
          return $ Map.insert (workflowRunWorkflowRunId existingWf) node acc

      newChildren <- forM (V.toList results) $ \workflow@(WorkflowRun {workflowRunWorkflowRunId=wfId}) ->
        case Map.lookup wfId existingWorkflowsMap of
          Just (SingleWorkflowNode existingEd) ->
            return $ SingleWorkflowNode (existingEd { _static = workflow })
          Nothing ->
            SingleWorkflowNode <$> makeEmptyElemWithState bc workflow (WorkflowNodeState NotFetched 1 SortJobsByFailures) "" (_depth + 1)

      swapChildrenClearingRemoved _children newChildren

  cancelGatheredHealthCheckThreads bc removedThreads

  -- Ensure every running/queued workflow has a health check thread, whether or not its node is
  -- expanded. This runs on every fetch (initial open and periodic refresh), so newly-appeared or
  -- newly-running workflows start getting polled too.
  readTVarIO _children >>= mapM_ (liftIO . void . startWorkflowHealthCheckForNode bc owner name _children)
