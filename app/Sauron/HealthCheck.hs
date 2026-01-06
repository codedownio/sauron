{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Sauron.HealthCheck (
  newHealthCheckThread
  ) where

import Control.Concurrent.STM (retry)
import Control.Exception.Safe (bracketOnError_, handleAny)
import Control.Monad.IO.Class
import Data.String.Interpolate
import qualified Data.Vector as V
import GitHub
import Relude
import Sauron.Actions.Util (withGithubApiSemaphore, githubWithLogging)
import Sauron.Logging (log, LogLevel(..))
import Sauron.Options
import Sauron.Types
import Sauron.UI.Statuses
import UnliftIO.Async
import UnliftIO.Concurrent


newHealthCheckThread :: (
  HasCallStack
  )
  => BaseContext
  -> (Name Owner, Name Repo)
  -> TVar (Fetchable Repo)
  -> TVar (Fetchable HealthCheckResult)
  -> PeriodSpec
  -> IO (Async ())
newHealthCheckThread baseContext (owner, name) repoVar healthCheckVar (PeriodSpec period) = async $ do
  log baseContext LevelInfo [i|Starting health check thread for repo: #{untagName owner}/#{untagName name} (period: #{period}us)|] Nothing
  handleAny (\e -> putStrLn [i|Health check thread crashed: #{e}|]) $ forever $ do
    -- TODO: how to not get "thread blocked indefinitely in an STM transaction"?
    defaultBranch <- atomically $ do
      readTVar repoVar >>= \case
        Fetched (Repo {repoDefaultBranch=(Just branch)}) -> pure branch
        _ -> retry

    liftIO $ flip runReaderT baseContext $ do
      bracketOnError_ (atomically $ markFetching healthCheckVar)
                      (atomically $ writeTVar healthCheckVar (Errored "Health check fetch failed with exception.")) $ do
        let search' = optionsWorkflowRunBranch defaultBranch
        withGithubApiSemaphore (githubWithLogging (workflowRunsR owner name search' (FetchAtLeast 1))) >>= \case
          Left err -> atomically $ writeTVar healthCheckVar (Errored (show err))
          Right (WithTotalCount {withTotalCountItems=(V.toList -> ((WorkflowRun {..}):_))}) -> do
            let result = HealthCheckWorkflowResult (chooseWorkflowStatus (fromMaybe workflowRunStatus workflowRunConclusion))
            atomically $ writeTVar healthCheckVar (Fetched result)
          Right _ -> atomically $ writeTVar healthCheckVar (Fetched HealthCheckNoData)

    threadDelay period
