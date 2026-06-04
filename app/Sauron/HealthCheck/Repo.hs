{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Sauron.HealthCheck.Repo (
  newRepoHealthCheckThread
  , runRepoHealthCheck
  ) where

import Control.Concurrent.STM (retry)
import Control.Exception.Safe (bracketOnError_, handleAny)
import Data.String.Interpolate
import qualified Data.Vector as V
import GitHub
import Relude
import Sauron.Actions.Util (withGithubApiSemaphore, githubWithLogging)
import Sauron.Logging
import Sauron.Options (PeriodSpec(..))
import Sauron.Types
import UnliftIO.Async
import UnliftIO.Concurrent


newRepoHealthCheckThread :: (
  HasCallStack
  )
  => BaseContext
  -> (Name Owner, Name Repo)
  -> TVar (Fetchable Repo)
  -> TVar (Fetchable HealthCheckResult)
  -> PeriodSpec
  -> IO (Async ())
newRepoHealthCheckThread baseContext (owner, name) repoVar healthCheckVar (PeriodSpec period) = async $ do
  log baseContext LevelInfo [i|Starting health check thread for repo: #{untagName owner}/#{untagName name} (period: #{period}us)|] Nothing
  handleAny (\e -> putStrLn [i|Health check thread crashed: #{e}|]) $ forever $ do
    runRepoHealthCheck baseContext (owner, name) repoVar healthCheckVar
    threadDelay period

-- | Perform a single health check for a repo, updating the health check TVar.
-- Blocks until the repo has been fetched with a default branch.
runRepoHealthCheck :: (
  HasCallStack
  )
  => BaseContext
  -> (Name Owner, Name Repo)
  -> TVar (Fetchable Repo)
  -> TVar (Fetchable HealthCheckResult)
  -> IO ()
runRepoHealthCheck baseContext (owner, name) repoVar healthCheckVar = do
  -- Wait until we have the repo with a default branch
  defaultBranch <- atomically $
    readTVar repoVar >>= \case
      Fetched (Repo {repoDefaultBranch=(Just branch)}) -> pure branch
      _ -> retry

  flip runReaderT baseContext $
    bracketOnError_ (atomically $ markFetching healthCheckVar)
                    (atomically $ writeTVar healthCheckVar (Errored "Health check fetch failed with exception.")) $ do
      -- Fetch both combined status and check runs for the default branch HEAD
      let ref = mkCommitName defaultBranch
      statusResult <- withGithubApiSemaphore (githubWithLogging (statusForR owner name ref))
      checkRunsResult <- withGithubApiSemaphore (githubWithLogging (checkRunsForR owner name ref))

      -- debug' baseContext [i|(#{owner}, #{name}) Health: #{(statusResult, checkRunsResult)}|]

      let result = computeHealthCheckResult statusResult checkRunsResult
      atomically $ writeTVar healthCheckVar (Fetched result)

-- | Compute health check result from combined status and check runs.
-- Green = combined status is success (or no statuses) AND all check runs completed with success.
computeHealthCheckResult :: Either Error CombinedStatus -> Either Error CheckRunsResponse -> HealthCheckResult
computeHealthCheckResult (Left err) _ = HealthCheckUnhealthy (show err)
computeHealthCheckResult _ (Left err) = HealthCheckUnhealthy (show err)
computeHealthCheckResult (Right combinedStatus) (Right checkRunsResp) = case (statusOk, checkRunsStatus) of
  (True, WorkflowSuccess) -> HealthCheckWorkflowResult WorkflowSuccess
  (True, WorkflowUnknown) | null checkRuns && statusCount == 0 -> HealthCheckNoData
  (True, otherStatus) -> HealthCheckWorkflowResult otherStatus
  _ | statusState == StatusPending -> HealthCheckWorkflowResult WorkflowPending
  _ -> HealthCheckWorkflowResult WorkflowFailed
  where
    -- Check combined status state
    statusState = combinedStatusState combinedStatus
    statusCount = combinedStatusTotalCount combinedStatus

    -- Check all check runs
    checkRuns = V.toList (checkRunsCheckRuns checkRunsResp)

    -- Determine overall status based on combined status
    statusOk = statusState == StatusSuccess || statusCount == 0

    -- Determine status from check runs
    checkRunsStatus = aggregateCheckRuns checkRuns

-- | Aggregate check runs into a single workflow status.
-- Returns Success only if all completed check runs succeeded.
-- Returns Pending/Running if any are still in progress.
-- Returns Failed if any failed.
aggregateCheckRuns :: [CheckRun] -> WorkflowStatus
aggregateCheckRuns [] = WorkflowUnknown
aggregateCheckRuns runs
  | WorkflowRunning `elem` statuses = WorkflowRunning
  | WorkflowPending `elem` statuses = WorkflowPending
  | WorkflowFailed `elem` statuses = WorkflowFailed
  -- Neutral/skipped check runs don't block a commit on GitHub (it still shows a green check),
  -- so treat them as passing: any non-failing mix of success/neutral/skipped reads as success.
  | all (`elem` [WorkflowSuccess, WorkflowNeutral]) statuses = WorkflowSuccess
  | otherwise = WorkflowNeutral
  where
    statuses = map checkRunToStatus runs

checkRunToStatus :: CheckRun -> WorkflowStatus
checkRunToStatus cr =
  case checkRunStatus cr of
    CheckRunQueued -> WorkflowPending
    CheckRunInProgress -> WorkflowRunning
    CheckRunCompleted ->
      case checkRunConclusion cr of
        Just CheckRunSuccess -> WorkflowSuccess
        Just CheckRunFailure -> WorkflowFailed
        Just CheckRunTimedOut -> WorkflowFailed
        Just CheckRunCancelled -> WorkflowCancelled
        Just CheckRunNeutral -> WorkflowNeutral
        Just CheckRunSkipped -> WorkflowNeutral
        Just CheckRunActionRequired -> WorkflowPending
        Just CheckRunStartupFailure -> WorkflowFailed
        Just CheckRunStale -> WorkflowNeutral
        Nothing -> WorkflowUnknown
