{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Sauron.HealthCheck.Stop (
  stopHealthCheckThreadIfRunning,
  stopHealthCheckThreadsForNodeAndChildren,
  healthCheckIndicatorWidget
  ) where

import Brick
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.String.Interpolate
import GitHub (untagName, jobName, workflowRunName)
import Relude
import Sauron.Fetch.Core (logToModal)
import Sauron.Types
import UnliftIO.Async


stopHealthCheckThreadsForNodeAndChildren :: MonadIO m => BaseContext -> SomeNode Variable -> m ()
stopHealthCheckThreadsForNodeAndChildren bc someNode = do
  threads <- liftIO $ atomically $ gatherAndClearAllHealthCheckThreads someNode
  let threadCount = length threads
  when (threadCount > 0) $
    logToModal bc LevelInfo [i|Stopped #{threadCount} health check threads|] Nothing
  liftIO $ mapM_ (cancel . snd) threads

stopHealthCheckThreadIfRunning :: MonadIO m => BaseContext -> SomeNode Variable -> m ()
stopHealthCheckThreadIfRunning bc someNode = do
  threads <- liftIO $ atomically $ gatherAndClearHealthCheckThread someNode
  forM_ threads $ \(identifier, thread) -> do
    liftIO $ cancel thread
    logToModal bc LevelInfo [i|(#{identifier}) Stopped health check thread|] Nothing

gatherAndClearAllHealthCheckThreads :: SomeNode Variable -> STM [(Text, Async ())]
gatherAndClearAllHealthCheckThreads someNode@(SomeNode node) = do
  -- Gather thread from this node
  nodeThreads <- gatherAndClearHealthCheckThread someNode

  -- Recursively gather threads from all children
  childNodes <- getExistentialChildrenWrapped node
  childThreads <- concat <$> mapM gatherAndClearAllHealthCheckThreads childNodes

  return (nodeThreads ++ childThreads)

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
      SingleJobNode (EntityData {_state}) -> do
        readTVar _state >>= \case
          Fetched job -> return [i|Job #{untagName (jobName job)}|]
          Fetching (Just job) -> return [i|Job #{untagName (jobName job)}|]
          _ -> return "Job (unknown)"
      SingleWorkflowNode (EntityData {_static=workflowRun}) ->
        return [i|Workflow #{workflowRunName workflowRun}|]
      RepoNode (EntityData {_static=(owner, name)}) ->
        return [i|Repo #{untagName owner}/#{untagName name}|]
      PaginatedIssuesNode _ -> return "Issues"
      PaginatedPullsNode _ -> return "Pull Requests"
      PaginatedWorkflowsNode _ -> return "Workflows"
      PaginatedBranchesNode _ -> return "Branches"
      PaginatedNotificationsNode _ -> return "Notifications"
      _ -> return "Unknown"

formatPeriodMicroseconds :: Int -> Text
formatPeriodMicroseconds us
  | us >= 1_000_000 =
    let seconds = fromIntegral us / 1_000_000 :: Double
        formatted :: String = if seconds == fromIntegral (round seconds :: Int)
                   then show (round seconds :: Int) <> "s"
                   else show seconds <> "s"
    in toText formatted
  | us >= 1_000 =
    let ms = fromIntegral us / 1_000 :: Double
        formatted :: String = if ms == fromIntegral (round ms :: Int)
                   then show (round ms :: Int) <> "ms"
                   else show ms <> "ms"
    in toText formatted
  | otherwise = toText (show us <> "μs" :: String)

healthCheckIndicatorWidget :: Maybe (Async (), Int) -> Widget n
healthCheckIndicatorWidget healthCheckThreadData =
  case healthCheckThreadData of
    Just (_, periodMicroseconds) -> str (" 👁[" <> toString (formatPeriodMicroseconds periodMicroseconds) <> "]")
    Nothing -> str ""
