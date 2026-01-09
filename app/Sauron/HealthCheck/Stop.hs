{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Sauron.HealthCheck.Stop (
  stopHealthCheckThreadIfRunning
  , stopHealthCheckThreadsForChildren
  , healthCheckIndicatorWidget
  ) where

import Brick
import Control.Monad.IO.Class
import Data.String.Interpolate
import Data.Time (NominalDiffTime)
import GitHub
import Relude
import Sauron.Logging
import Sauron.Types
import UnliftIO.Async


stopHealthCheckThreadsForChildren :: MonadIO m => BaseContext -> SomeNode Variable -> m ()
stopHealthCheckThreadsForChildren bc someNode = do
  threads <- liftIO $ atomically $ gatherAndClearChildrenHealthCheckThreads someNode
  let threadCount = length threads
  when (threadCount > 0) $
    info' bc [i|Stopped #{threadCount} child health check threads: #{fmap fst threads}|]
  liftIO $ mapM_ (cancel . snd) threads

stopHealthCheckThreadIfRunning :: MonadIO m => BaseContext -> SomeNode Variable -> m ()
stopHealthCheckThreadIfRunning bc someNode = do
  threads <- liftIO $ atomically $ gatherAndClearHealthCheckThread someNode
  forM_ threads $ \(identifier, thread) -> do
    liftIO $ cancel thread
    info' bc [i|(#{identifier}) Stopped health check thread|]

gatherAndClearAllHealthCheckThreads :: SomeNode Variable -> STM [(Text, Async ())]
gatherAndClearAllHealthCheckThreads someNode@(SomeNode node) = do
  -- Gather thread from this node
  nodeThreads <- gatherAndClearHealthCheckThread someNode

  -- Recursively gather threads from all children
  childNodes <- getExistentialChildrenWrapped node
  childThreads <- concat <$> mapM gatherAndClearAllHealthCheckThreads childNodes

  return (nodeThreads ++ childThreads)

gatherAndClearChildrenHealthCheckThreads :: SomeNode Variable -> STM [(Text, Async ())]
gatherAndClearChildrenHealthCheckThreads (SomeNode node) = do
  -- Only gather threads from children, not from this node itself
  childNodes <- getExistentialChildrenWrapped node
  concat <$> mapM gatherAndClearAllHealthCheckThreads childNodes

gatherAndClearHealthCheckThread :: SomeNode Variable -> STM [(Text, Async ())]
gatherAndClearHealthCheckThread someNode@(SomeNode node) = do
  let entityData = getEntityData node
  readTVar (_healthCheckThread entityData) >>= \case
    Just (thread, _period) -> do
      identifier <- nodeIdentifier someNode
      writeTVar (_healthCheckThread entityData) Nothing
      return [(identifier, thread)]
    Nothing -> return []
  where
    nodeIdentifier :: SomeNode Variable -> STM Text
    nodeIdentifier (SomeNode node') = case node' of
      SingleJobNode (EntityData {_static=job}) ->
        return [i|Job #{untagName (jobName job)}|]
      SingleWorkflowNode (EntityData {_static=workflowRun}) ->
        return [i|Workflow #{untagName $ workflowRunName workflowRun} \##{workflowRunRunNumber workflowRun}|]
      RepoNode (EntityData {_static=(owner, name)}) ->
        return [i|Repo #{untagName owner}/#{untagName name}|]
      PaginatedIssuesNode _ -> return "Issues"
      PaginatedPullsNode _ -> return "Pull Requests"
      PaginatedWorkflowsNode _ -> return "Workflows"
      PaginatedBranchesNode _ -> return "Branches"
      PaginatedNotificationsNode _ -> return "Notifications"
      _ -> return "Unknown"

healthCheckIndicatorWidget :: Maybe (Async (), Int) -> Widget n
healthCheckIndicatorWidget healthCheckThreadData =
  case healthCheckThreadData of
    Just (_, periodMicroseconds) ->
      let diffTime = fromIntegral periodMicroseconds / 1_000_000 :: NominalDiffTime
      in str (" ↻[" <> show diffTime <> "]")
    Nothing -> str ""
