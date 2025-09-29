{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Sauron.HealthCheck.NodeHealthCheck (
  startWorkflowsHealthCheckIfNeeded
  , startJobHealthCheckIfNeeded
  , stopHealthCheckThread
  ) where

import Control.Exception.Safe (handleAny)
import Control.Monad.IO.Class
import Data.String.Interpolate
import qualified Data.Vector as V
import GitHub
import Relude
import Sauron.Actions.Util (findRepoParent)
import Sauron.Fetch (fetchWorkflows, fetchWorkflowJobs)
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

isJobCompleted :: Text -> Bool
isJobCompleted = isWorkflowCompleted


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
          case _healthCheckThread of
            Nothing -> (Just <$>) $ async $ runHealthCheckLoop baseContext owner name node
            Just _ -> return _healthCheckThread
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
          _ -> return ()

    hasRunningWorkflows :: WithTotalCount WorkflowRun -> Bool
    hasRunningWorkflows (WithTotalCount {withTotalCountItems=workflows}) =
      V.any (\wr -> not $ isWorkflowCompleted (fromMaybe (workflowRunStatus wr) (workflowRunConclusion wr))) workflows

startJobHealthCheckIfNeeded ::
  BaseContext
  -> Node Variable 'SingleJobT
  -> NonEmpty (SomeNode Variable)
  -> IO (Maybe (Async ()))
startJobHealthCheckIfNeeded baseContext node@(SingleJobNode (EntityData {_static=job, ..})) parents = do
  case findRepoParent parents of
    Just (RepoNode (EntityData {_static=(owner, name)})) | hasRunningJob job -> do
      case _healthCheckThread of
        Nothing -> (Just <$>) $ async $ runJobHealthCheckLoop baseContext owner name node parents
        Just _ -> return _healthCheckThread
    _ -> return Nothing
  where
    runJobHealthCheckLoop :: BaseContext -> Name Owner -> Name Repo -> Node Variable 'SingleJobT -> NonEmpty (SomeNode Variable) -> IO ()
    runJobHealthCheckLoop bc owner name _jobNode@(SingleJobNode (EntityData {_static=staticJob})) pars =
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
          else return () -- Job is completed, stop the thread

    hasRunningJob :: Job -> Bool
    hasRunningJob j = not $ isJobCompleted (fromMaybe (jobStatus j) (jobConclusion j))

    isWorkflowParent :: SomeNode Variable -> Bool
    isWorkflowParent (SomeNode (SingleWorkflowNode _)) = True
    isWorkflowParent _ = False

stopHealthCheckThread :: Maybe (Async ()) -> IO ()
stopHealthCheckThread Nothing = return ()
stopHealthCheckThread (Just thread) = cancel thread
