{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Sauron.HealthCheck.Job (
  startJobHealthCheckIfNeeded
  , jobHealthCheckPeriodUs
  , isJobCompleted
  ) where

import Control.Exception.Safe (finally, handleAny)
import Control.Monad.Logger
import Data.String.Interpolate
import GitHub
import Relude
import Sauron.Actions.Util (findRepoParent)
import Sauron.Fetch.Job (fetchJob, fetchJobLogs)
import Sauron.HealthCheck.Common (clearOwnHealthCheckThread)
import Sauron.Logging (log)
import Sauron.Types
import Sauron.UI.Statuses
import UnliftIO.Async
import UnliftIO.Concurrent


isJobCompleted :: Job -> Bool
isJobCompleted j = isJobCompleted' (fromMaybe (jobStatus j) (jobConclusion j))

isJobCompleted' :: Text -> Bool
isJobCompleted' status = case chooseWorkflowStatus status of
  WorkflowSuccess -> True
  WorkflowFailed -> True
  WorkflowCancelled -> True
  WorkflowNeutral -> True
  _ -> False

jobHealthCheckPeriodUs :: Int
jobHealthCheckPeriodUs = 5_000_000

startJobHealthCheckIfNeeded ::
  BaseContext
  -> Node Variable 'SingleJobT
  -> NonEmpty (SomeNode Variable)
  -> IO (Maybe (Async ()))
startJobHealthCheckIfNeeded baseContext node@(SingleJobNode (EntityData {_static=(Job {jobId}), ..})) parents = do
  case findRepoParent parents of
    Just (RepoNode (EntityData {_static=(owner, name)})) ->
      readTVarIO _healthCheckThread >>= \case
        Nothing -> do
          log baseContext LevelInfo [i|Starting health check thread for job: #{jobId} (period: #{jobHealthCheckPeriodUs}us)|] Nothing
          newThread <- async $ runJobHealthCheckLoop baseContext owner name node
          atomically $ writeTVar _healthCheckThread (Just (newThread, jobHealthCheckPeriodUs))
          return (Just newThread)
        Just (thread, _) -> return (Just thread)
    _ -> return Nothing
  where
    runJobHealthCheckLoop :: BaseContext -> Name Owner -> Name Repo -> Node Variable 'SingleJobT -> IO ()
    runJobHealthCheckLoop bc owner name jobNode@(SingleJobNode (EntityData {_state, _healthCheckThread=threadVar})) =
      flip finally (clearOwnHealthCheckThread threadVar) $
      handleAny (\e -> putStrLn [i|Job health check thread crashed: #{e}|]) $
      fix $ \loop ->
        (fetchableCurrent . jnsJob) <$> readTVarIO _state >>= \case
          Just currentJob | not (isJobCompleted currentJob) -> do
            -- Keep the logs and the job's own status up to date. Refetching the job here is what
            -- lets this loop notice that the job finished and exit; without it we'd depend on the
            -- parent workflow's health check still running to update our state.
            flip runReaderT bc $ do
              fetchJobLogs owner name currentJob jobNode Nothing
              fetchJob owner name jobId jobNode
            threadDelay jobHealthCheckPeriodUs
            loop
          -- The job finished (or we never got it), so stop; the finally clears our handle
          _ -> return ()
