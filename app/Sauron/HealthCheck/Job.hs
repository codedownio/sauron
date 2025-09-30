{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Sauron.HealthCheck.Job (
  startJobHealthCheckIfNeeded
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

isJobCompleted :: Text -> Bool
isJobCompleted status = case chooseWorkflowStatus status of
  WorkflowSuccess -> True
  WorkflowFailed -> True
  WorkflowCancelled -> True
  WorkflowNeutral -> True
  _ -> False

startJobHealthCheckIfNeeded ::
  BaseContext
  -> Node Variable 'SingleJobT
  -> NonEmpty (SomeNode Variable)
  -> IO (Maybe (Async ()))
startJobHealthCheckIfNeeded baseContext node@(SingleJobNode (EntityData {_static=job, ..})) parents = do
  case findRepoParent parents of
    Just (RepoNode (EntityData {_static=(owner, name)})) | hasRunningJob job -> do
      currentThread <- readTVarIO _healthCheckThread
      case currentThread of
        Nothing -> do
          newThread <- async $ runJobHealthCheckLoop baseContext owner name node parents
          atomically $ writeTVar _healthCheckThread (Just newThread)
          return (Just newThread)
        Just thread -> return (Just thread)
    _ -> return Nothing
  where
    runJobHealthCheckLoop :: BaseContext -> Name Owner -> Name Repo -> Node Variable 'SingleJobT -> NonEmpty (SomeNode Variable) -> IO ()
    runJobHealthCheckLoop bc owner name _jobNode@(SingleJobNode (EntityData {_static=staticJob, ..})) pars =
      handleAny (\e -> putStrLn [i|Job health check thread crashed: #{e}|]) $
      forever $ do
        if hasRunningJob staticJob
          then do
            -- Find the workflow parent and refresh it (which will refresh all jobs)
            case find isWorkflowParent pars of
              Just (SomeNode workflowNode@(SingleWorkflowNode (EntityData {_static=workflowRun}))) -> do
                liftIO $ flip runReaderT bc $
                  fetchWorkflowJobs owner name (workflowRunWorkflowRunId workflowRun) workflowNode
              _ -> return ()
            threadDelay (5 * 1000000) -- 5 seconds
          else do
            -- Job is completed, clear the thread reference and stop
            atomically $ writeTVar _healthCheckThread Nothing
            return ()

    hasRunningJob :: Job -> Bool
    hasRunningJob j = not $ isJobCompleted (fromMaybe (jobStatus j) (jobConclusion j))

    isWorkflowParent :: SomeNode Variable -> Bool
    isWorkflowParent (SomeNode (SingleWorkflowNode _)) = True
    isWorkflowParent _ = False