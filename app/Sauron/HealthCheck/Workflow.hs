{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Sauron.HealthCheck.Workflow (
  startWorkflowsHealthCheckIfNeeded
  ) where

import Control.Exception.Safe (handleAny)
import Control.Monad.IO.Class
import Data.String.Interpolate
import qualified Data.Vector as V
import GitHub
import Relude
import Sauron.Actions.Util (findRepoParent)
import Sauron.Fetch (fetchWorkflows)
import Sauron.Types
import Sauron.UI.Statuses
import UnliftIO.Async
import UnliftIO.Concurrent

isWorkflowCompleted :: Text -> Bool
isWorkflowCompleted status = case chooseWorkflowStatus status of
  WorkflowSuccess -> True
  WorkflowFailed -> True
  WorkflowCancelled -> True
  WorkflowNeutral -> True
  _ -> False

startWorkflowsHealthCheckIfNeeded ::
  BaseContext
  -> Node Variable 'PaginatedWorkflowsT
  -> NonEmpty (SomeNode Variable)
  -> IO (Maybe (Async ()))
startWorkflowsHealthCheckIfNeeded baseContext node@(PaginatedWorkflowsNode (EntityData {..})) parents = do
  case findRepoParent parents of
    Just (RepoNode (EntityData {_static=(owner, name)})) -> do
      currentState <- readTVarIO _state
      case currentState of
        Fetched workflowsData | hasRunningWorkflows workflowsData -> do
          currentThread <- readTVarIO _healthCheckThread
          case currentThread of
            Nothing -> do
              newThread <- async $ runHealthCheckLoop baseContext owner name node
              atomically $ writeTVar _healthCheckThread (Just newThread)
              return (Just newThread)
            Just thread -> return (Just thread)
        _ -> return Nothing
    Nothing -> return Nothing
  where
    runHealthCheckLoop :: BaseContext -> Name Owner -> Name Repo -> Node Variable 'PaginatedWorkflowsT -> IO ()
    runHealthCheckLoop bc owner name wfNode@(PaginatedWorkflowsNode (EntityData {..})) =
      handleAny (\e -> putStrLn [i|Workflows health check thread crashed: #{e}|]) $
      forever $ do
        currentState <- readTVarIO _state
        case currentState of
          Fetched workflowsData | hasRunningWorkflows workflowsData -> do
            liftIO $ flip runReaderT bc $ fetchWorkflows owner name wfNode
            threadDelay 5_000_000
          _ -> do
            -- No more running workflows, clear the thread reference and stop
            atomically $ writeTVar _healthCheckThread Nothing
            return ()

    hasRunningWorkflows :: WithTotalCount WorkflowRun -> Bool
    hasRunningWorkflows (WithTotalCount {withTotalCountItems=workflows}) =
      V.any (\wr -> not $ isWorkflowCompleted (fromMaybe (workflowRunStatus wr) (workflowRunConclusion wr))) workflows