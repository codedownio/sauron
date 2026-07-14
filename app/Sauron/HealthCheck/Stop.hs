{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Sauron.HealthCheck.Stop (
  stopHealthCheckThreadIfRunning
  , stopHealthCheckThreadsForChildren
  , swapChildrenClearingRemoved
  , cancelGatheredHealthCheckThreads
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

-- | Atomically replace a node's children, returning the health check threads of any children
-- (and their descendants) that are no longer present. Nodes are matched by their identity, so a
-- node reused across a fetch keeps its thread; only genuinely removed subtrees are cleared. The
-- caller cancels the returned threads with 'cancelGatheredHealthCheckThreads' once out of STM.
swapChildrenClearingRemoved ::
  (SomeNodeConstraints Variable a)
  => TVar [Node Variable a] -> [Node Variable a] -> STM [(Text, Async ())]
swapChildrenClearingRemoved childrenVar newChildren = do
  oldChildren <- readTVar childrenVar
  writeTVar childrenVar newChildren
  let survivingIdents = fmap (_ident . getEntityData) newChildren
  concat <$> mapM (gatherAndClearAllHealthCheckThreads . SomeNode)
                  (filter ((`notElem` survivingIdents) . _ident . getEntityData) oldChildren)

-- | Cancel health check threads gathered by 'swapChildrenClearingRemoved'.
cancelGatheredHealthCheckThreads :: MonadIO m => BaseContext -> [(Text, Async ())] -> m ()
cancelGatheredHealthCheckThreads bc threads = do
  unless (null threads) $
    info' bc [i|Stopped #{length threads} health check thread(s) for removed nodes: #{fmap fst threads}|]
  liftIO $ mapM_ (cancel . snd) threads

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
      in str (" ⭮[" <> show diffTime <> "]")
    Nothing -> str ""
