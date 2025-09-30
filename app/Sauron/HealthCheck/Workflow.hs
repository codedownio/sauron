{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Sauron.HealthCheck.Workflow (
  startWorkflowHealthCheckIfNeeded
  ) where

import Control.Exception.Safe (handleAny)
import Control.Monad.IO.Class
import Data.String.Interpolate
import GitHub
import Relude
import Sauron.Actions.Util (findRepoParent)
import Sauron.Fetch (fetchWorkflowJobs)
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

startWorkflowHealthCheckIfNeeded ::
  BaseContext
  -> Node Variable 'SingleWorkflowT
  -> NonEmpty (SomeNode Variable)
  -> IO (Maybe (Async ()))
startWorkflowHealthCheckIfNeeded baseContext node@(SingleWorkflowNode (EntityData {_static=workflowRun, ..})) parents = do
  case findRepoParent parents of
    Just (RepoNode (EntityData {_static=(owner, name)})) | hasRunningWorkflow workflowRun -> do
      currentThread <- readTVarIO _healthCheckThread
      case currentThread of
        Nothing -> do
          newThread <- async $ runWorkflowHealthCheckLoop baseContext owner name node
          atomically $ writeTVar _healthCheckThread (Just newThread)
          return (Just newThread)
        Just thread -> return (Just thread)
    _ -> return Nothing
  where
    runWorkflowHealthCheckLoop :: BaseContext -> Name Owner -> Name Repo -> Node Variable 'SingleWorkflowT -> IO ()
    runWorkflowHealthCheckLoop bc owner name wfNode@(SingleWorkflowNode (EntityData {_static=staticWorkflowRun, ..})) =
      handleAny (\e -> putStrLn [i|Workflow health check thread crashed: #{e}|]) $
      forever $ do
        if hasRunningWorkflow staticWorkflowRun
          then do
            -- Refresh the workflow jobs
            liftIO $ flip runReaderT bc $
              fetchWorkflowJobs owner name (workflowRunWorkflowRunId staticWorkflowRun) wfNode
            threadDelay (5 * 1000000) -- 5 seconds
          else do
            -- Workflow is completed, clear the thread reference and stop
            atomically $ writeTVar _healthCheckThread Nothing
            return ()

    hasRunningWorkflow :: WorkflowRun -> Bool
    hasRunningWorkflow wr = not $ isWorkflowCompleted $ fromMaybe (workflowRunStatus wr) (workflowRunConclusion wr)
