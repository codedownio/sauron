{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Sauron.HealthCheck.Workflow (
  startWorkflowHealthCheckIfNeeded,
  workflowHealthCheckPeriodUs
  ) where

import Control.Exception.Safe (handleAny)
import Control.Monad.Logger
import Data.String.Interpolate
import GitHub
import Relude
import Sauron.Actions.Util
import Sauron.Logging
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


workflowHealthCheckPeriodUs :: Int
workflowHealthCheckPeriodUs = 5_000_000

startWorkflowHealthCheckIfNeeded ::
  BaseContext
  -> Node Variable 'SingleWorkflowT
  -> NonEmpty (SomeNode Variable)
  -> IO (Maybe (Async ()))
startWorkflowHealthCheckIfNeeded baseContext node@(SingleWorkflowNode (EntityData {_static=workflowRun, ..})) parents = do
  case findRepoParent parents of
    Just (RepoNode (EntityData {_static=(owner, name)})) | hasRunningWorkflow workflowRun -> do
      readTVarIO _healthCheckThread >>= \case
        Nothing -> do
          log baseContext LevelInfo [i|Starting health check thread for workflow: #{untagName $ workflowRunName workflowRun} \##{workflowRunRunNumber workflowRun} (period: #{workflowHealthCheckPeriodUs}us)|] Nothing
          newThread <- async $ runWorkflowHealthCheckLoop baseContext owner name node
          atomically $ writeTVar _healthCheckThread (Just (newThread, workflowHealthCheckPeriodUs))
          return (Just newThread)
        Just (thread, _) -> return (Just thread)
    _ -> return Nothing
  where
    runWorkflowHealthCheckLoop :: BaseContext -> Name Owner -> Name Repo -> Node Variable 'SingleWorkflowT -> IO ()
    runWorkflowHealthCheckLoop bc owner name (SingleWorkflowNode (EntityData {_static=staticWorkflowRun, ..})) =
      flip runReaderT bc $
      handleAny (\e -> putStrLn [i|Workflow health check thread crashed: #{e}|]) $
      fix $ \loop ->
        withGithubApiSemaphore (githubWithLogging (workflowRunR owner name (workflowRunWorkflowRunId staticWorkflowRun))) >>= \case
          Left err -> warn' bc [i|(#{untagName owner}/#{untagName name}) Couldn't fetch workflow run #{workflowRunWorkflowRunId staticWorkflowRun}: #{err}|]
          Right response ->
            case hasRunningWorkflow response of
              True -> do
                threadDelay workflowHealthCheckPeriodUs
                loop
              False -> do
                -- Stop ourselves by clearing the thread reference and returning
                atomically $ writeTVar _healthCheckThread Nothing
                return ()

    hasRunningWorkflow :: WorkflowRun -> Bool
    hasRunningWorkflow wr = not $ isWorkflowCompleted $ fromMaybe (workflowRunStatus wr) (workflowRunConclusion wr)
