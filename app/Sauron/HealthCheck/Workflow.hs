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
import Sauron.Fetch.Job
import Sauron.HealthCheck.Job (isJobCompleted)
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
    Just (RepoNode (EntityData {_static=(owner, name)})) | isRunningWorkflow workflowRun -> do
      info' baseContext [i|Considering whether to start workflow healthcheck thread for run: #{workflowRun}|]
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
    runWorkflowHealthCheckLoop bc owner name (SingleWorkflowNode (EntityData {_static=staticWorkflowRun})) =
      flip runReaderT bc $
      handleAny (\e -> putStrLn [i|Workflow health check thread crashed: #{e}|]) $
      fix $ \loop -> do
        fetchWorkflowJobs owner name (workflowRunWorkflowRunId staticWorkflowRun) node >>= \case
          Left _err -> checkWorkflowForDoneness bc owner name (workflowRunWorkflowRunId staticWorkflowRun) loop
          Right jobs
            | all isJobCompleted jobs -> checkWorkflowForDoneness bc owner name (workflowRunWorkflowRunId staticWorkflowRun) loop
            | otherwise -> do
                -- Save an API call by not checking the workflow for doneness. Just sleep and loop.
                threadDelay workflowHealthCheckPeriodUs
                loop

    checkWorkflowForDoneness bc owner name workflowRunId loop =
      withGithubApiSemaphore (githubWithLogging (workflowRunR owner name workflowRunId)) >>= \case
        Left err -> warn' bc [i|(#{untagName owner}/#{untagName name}) Couldn't fetch workflow run #{workflowRunId}: #{err}|]
        Right workflowRun' ->
          if | isRunningWorkflow workflowRun' -> threadDelay workflowHealthCheckPeriodUs >> loop
             | otherwise -> atomically $ writeTVar _healthCheckThread Nothing

    isRunningWorkflow :: WorkflowRun -> Bool
    isRunningWorkflow wr = not $ isWorkflowCompleted $ fromMaybe (workflowRunStatus wr) (workflowRunConclusion wr)
