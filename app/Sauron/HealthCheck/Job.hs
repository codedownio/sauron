{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Sauron.HealthCheck.Job (
  startJobHealthCheckIfNeeded,
  jobHealthCheckPeriodUs
  ) where

import Control.Exception.Safe (handleAny)
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.String.Interpolate
import GitHub
import Relude
import Sauron.Actions.Util (findRepoParent)
import Sauron.Fetch.Job (fetchJob)
import Sauron.Logging (log)
import Sauron.Types
import Sauron.UI.Statuses
import UnliftIO.Async
import UnliftIO.Concurrent


isJobCompleted :: Text -> Bool
isJobCompleted status = case chooseWorkflowStatus status of
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
startJobHealthCheckIfNeeded baseContext node@(SingleJobNode (EntityData {_state, _static=(Job {jobId}), ..})) parents = do
  case findRepoParent parents of
    Just (RepoNode (EntityData {_static=(owner, name)})) ->
      readTVarIO _healthCheckThread >>= \case
        Nothing -> do
          log baseContext LevelInfo [i|Starting health check thread for job: #{jobId} (period: #{jobHealthCheckPeriodUs}us)|] Nothing
          newThread <- async $ runJobHealthCheckLoop baseContext owner name node parents
          atomically $ writeTVar _healthCheckThread (Just (newThread, jobHealthCheckPeriodUs))
          return (Just newThread)
        Just (thread, _) -> return (Just thread)
    _ -> return Nothing
  where
    runJobHealthCheckLoop :: BaseContext -> Name Owner -> Name Repo -> Node Variable 'SingleJobT -> NonEmpty (SomeNode Variable) -> IO ()
    runJobHealthCheckLoop bc owner name jobNode@(SingleJobNode (EntityData {_state, ..})) _pars =
      handleAny (\e -> putStrLn [i|Job health check thread crashed: #{e}|]) $
      forever $ do
        currentState <- readTVarIO _state
        case currentState of
          Fetched currentJob | hasRunningJob currentJob -> do
            -- Fetch just this individual job to update its status
            liftIO $ flip runReaderT bc $
              fetchJob owner name jobId jobNode
            threadDelay jobHealthCheckPeriodUs
          _ -> do
            -- Job is completed, clear the thread reference and stop
            atomically $ writeTVar _healthCheckThread Nothing
            return ()

    hasRunningJob :: Job -> Bool
    hasRunningJob j = not $ isJobCompleted (fromMaybe (jobStatus j) (jobConclusion j))
