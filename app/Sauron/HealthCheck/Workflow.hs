{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Sauron.HealthCheck.Workflow (
  startWorkflowHealthCheckIfNeeded,
  startWorkflowHealthCheckForNode,
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
import Sauron.HealthCheck.Repo (runRepoHealthCheck)
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
startWorkflowHealthCheckIfNeeded baseContext node parents =
  case (findRepoParent parents, findWorkflowsParent parents) of
    (Just (RepoNode (EntityData {_static=(owner, name), _state=repoState, _healthCheck=repoHealthCheck})), Just (PaginatedWorkflowsNode (EntityData {_children=workflowsChildren}))) ->
      startWorkflowHealthCheckForNode baseContext owner name workflowsChildren (runRepoHealthCheck baseContext (owner, name) repoState repoHealthCheck) node
    _ -> return Nothing

-- | Start a health check thread for a single workflow node, if it's still running and doesn't
-- already have one. Keyed on the workflow's state rather than whether its node is expanded, so
-- queued/running workflows get polled even when they aren't opened in the UI.
startWorkflowHealthCheckForNode ::
  BaseContext
  -> Name Owner
  -> Name Repo
  -> TVar [Node Variable 'SingleWorkflowT]
  -> IO ()  -- ^ Refresh the parent repo's health check, run when the workflow finishes.
  -> Node Variable 'SingleWorkflowT
  -> IO (Maybe (Async ()))
startWorkflowHealthCheckForNode baseContext owner name workflowsChildren refreshRepoHealth node@(SingleWorkflowNode (EntityData {_static=workflowRun, _ident=nodeIdent, ..}))
  | isRunningWorkflow workflowRun =
      readTVarIO _healthCheckThread >>= \case
        Nothing -> do
          log baseContext LevelInfo [i|Starting health check thread for workflow: #{untagName $ workflowRunName workflowRun} \##{workflowRunRunNumber workflowRun} (period: #{workflowHealthCheckPeriodUs}us)|] Nothing
          newThread <- async $ runWorkflowHealthCheckLoop baseContext owner name node workflowsChildren nodeIdent
          atomically $ writeTVar _healthCheckThread (Just (newThread, workflowHealthCheckPeriodUs))
          return (Just newThread)
        Just (thread, _) -> return (Just thread)
  | otherwise = return Nothing
  where
    runWorkflowHealthCheckLoop :: BaseContext -> Name Owner -> Name Repo -> Node Variable 'SingleWorkflowT -> TVar [Node Variable 'SingleWorkflowT] -> Int -> IO ()
    runWorkflowHealthCheckLoop bc owner name (SingleWorkflowNode (EntityData {_static=staticWorkflowRun})) workflowsChildren nodeIdent' =
      flip runReaderT bc $
      handleAny (\e -> putStrLn [i|Workflow health check thread crashed: #{e}|]) $
      fix $ \loop ->
        fetchWorkflowJobs owner name (workflowRunWorkflowRunId staticWorkflowRun) node >>= \case
          Left _err ->
            checkWorkflowForDoneness bc owner name (workflowRunWorkflowRunId staticWorkflowRun) loop workflowsChildren nodeIdent'
          Right jobs
            | all isJobCompleted jobs ->
                checkWorkflowForDoneness bc owner name (workflowRunWorkflowRunId staticWorkflowRun) loop workflowsChildren nodeIdent'
            | otherwise -> do
                -- Save an API call by not checking the workflow for doneness. Just sleep and loop.
                threadDelay workflowHealthCheckPeriodUs
                loop

    checkWorkflowForDoneness bc owner name workflowRunId loop workflowsChildren nodeIdent' = do
      withGithubApiSemaphore (githubWithLogging (workflowRunR owner name workflowRunId)) >>= \case
        Left err -> warn' bc [i|(#{untagName owner}/#{untagName name}) Couldn't fetch workflow run #{workflowRunId}: #{err}|]
        Right workflowRun'
          | isRunningWorkflow workflowRun' -> threadDelay workflowHealthCheckPeriodUs >> loop
          | otherwise -> do
              liftIO $ atomically $ do
                -- Update the parent's _children, replacing our node with updated _static
                modifyTVar' workflowsChildren $ map $ \child@(SingleWorkflowNode childEd) ->
                  if _ident childEd == nodeIdent'
                  then SingleWorkflowNode (childEd { _static = workflowRun' })
                  else child
                writeTVar _healthCheckThread Nothing
              -- The workflow just finished, so re-poll the repo's overall health right away instead
              -- of waiting up to the full repo health-check period for the icon to go green/red.
              liftIO refreshRepoHealth

    isRunningWorkflow :: WorkflowRun -> Bool
    isRunningWorkflow wr = not $ isWorkflowCompleted $ fromMaybe (workflowRunStatus wr) (workflowRunConclusion wr)
