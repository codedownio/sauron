{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Sauron.Fetch.Job (
  fetchWorkflowJobs
  , fetchJob
  , fetchJobLogs
  ) where

import Control.Exception.Safe (bracketOnError_)
import Control.Monad (foldM)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..))
import qualified Data.Vector as V
import GitHub
import Network.HTTP.Conduit hiding (Proxy)
import qualified Network.URI as URI
import Relude
import Sauron.Actions.Util
import Sauron.Fetch.Core
import Sauron.Fetch.ParseJobLogs
import Sauron.Logging
import Sauron.Types


fetchWorkflowJobs :: (
  HasCallStack, MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Id WorkflowRun -> Node Variable SingleWorkflowT -> m ()
fetchWorkflowJobs owner name workflowRunId (SingleWorkflowNode (EntityData {..})) = do
  bc <- ask
  bracketOnError_ (atomically $ markFetching _state)
                  (atomically $ writeTVar _state (Errored "Workflow jobs fetch failed with exception.")) $
    -- pullRequestCommentsR returns comments on the "unified diff"
    -- there are also "commit comments" and "issue comments".
    -- The last one are the most common on PRs, so we use commentsR
    withGithubApiSemaphore (githubWithLogging (jobsForWorkflowRunR owner name workflowRunId FetchAll)) >>= \case
      Left err -> do
        -- traceM [i|Error fetching workflow jobs: #{err}|]
        atomically $ writeTVar _state (Errored (show err))
      Right (WithTotalCount results totalCount) -> atomically $ do
        writeTVar _state (Fetched totalCount)

        -- Preserve existing job nodes and their toggle state
        existingChildren <- readTVar _children

        -- Build a map of existing jobs by their IDs
        existingJobsMap <- foldM (\acc node -> case node of
          SingleJobNode (EntityData {_state=jobState}) -> do
            readTVar jobState >>= \case
              Fetched existingJob -> return $ Map.insert (jobId existingJob) node acc
              Fetching (Just existingJob) -> return $ Map.insert (jobId existingJob) node acc
              _ -> return acc
          ) (Map.empty :: Map.Map (Id Job) (Node Variable SingleJobT)) existingChildren

        (writeTVar _children =<<) $ forM (V.toList results) $ \job@(Job {jobId}) -> do
          case Map.lookup jobId existingJobsMap of
            Just existingNode@(SingleJobNode existingEntityData) -> do
              -- Update existing node's job data while preserving toggle state
              let EntityData {_state=jobState} = existingEntityData
              writeTVar jobState (Fetched job)
              return existingNode
            Nothing -> do
              -- Create new node for new jobs
              entityData <- makeEmptyElemWithState bc job NotFetched "" (_depth + 1)
              let EntityData {_state=jobState, _children=jobChildren} = entityData
              writeTVar jobState (Fetched job)

              -- Create dummy child for log fetching
              let dummyLogGroup = JobLogLines (UTCTime (fromGregorian 1970 1 1) 0) ["Loading job logs..."]
              dummyLogEntityData <- makeEmptyElemWithState bc dummyLogGroup () "" (_depth + 2)
              writeTVar jobChildren [JobLogGroupNode dummyLogEntityData]

              return $ SingleJobNode entityData

fetchJob :: (
  HasCallStack, MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Id Job -> Node Variable SingleJobT -> m ()
fetchJob owner name jobId (SingleJobNode (EntityData {_state})) = do
  withGithubApiSemaphore (githubWithLoggingResponse (jobR owner name jobId)) >>= \case
    Left _err -> return () -- Silently fail for health checks
    Right (responseBody -> updatedJob) ->
      atomically $ writeTVar _state (Fetched updatedJob)

fetchJobLogs :: (
  HasCallStack, MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Job -> Node Variable SingleJobT -> m ()
fetchJobLogs owner name (Job {jobId, jobSteps}) (SingleJobNode (EntityData {..})) = do
  bc@(BaseContext {auth, manager}) <- ask
  bracketOnError_ (atomically $ markFetching _state)
                  (atomically $ writeTVar _state (Errored "Job logs fetch failed with exception.")) $ do
    withGithubApiSemaphore (liftIO $ executeRequestWithMgrAndRes manager auth (downloadJobLogsR owner name jobId)) >>= \case
      Left err -> atomically $ writeTVar _state (Errored (show err))
      Right response -> do
        logs <- simpleHttp (URI.uriToString id (responseBody response) "")

        let parsedLogs = parseJobLogs (T.splitOn "\n" (decodeUtf8 logs))
        children' <- liftIO $ atomically $ do
          mapM (createJobStepNode bc (_depth + 1) parsedLogs) (V.toList jobSteps)

        atomically $ writeTVar _children children'

-- * Util

createJobStepNode :: BaseContext -> Int -> [JobLogGroup] -> JobStep -> STM (Node Variable 'JobLogGroupT)
createJobStepNode bc depth' allLogs jobStep = do
  let stepTitle = untagName (jobStepName jobStep)
  let stepTimestamp = fromMaybe (UTCTime (fromGregorian 1970 1 1) 0) (jobStepStartedAt jobStep)
  let stepStatus = Just $ fromMaybe (jobStepStatus jobStep) (jobStepConclusion jobStep)
  createJobLogGroupChildren bc depth' (JobLogGroup stepTimestamp stepTitle stepStatus logsForStep)
  where
    logsForStep :: [JobLogGroup]
    logsForStep =
      let startTime = jobStepStartedAt jobStep
          endTime = jobStepCompletedAt jobStep
      in filter (logInTimeRange startTime endTime) allLogs

    logInTimeRange :: Maybe UTCTime -> Maybe UTCTime -> JobLogGroup -> Bool
    logInTimeRange startTime endTime logGroup =
      let logTime = getLogTimestamp logGroup
      in case (startTime, endTime) of
           (Just start, Just end) -> logTime >= start && logTime <= end
           (Just start, Nothing) -> logTime >= start
           (Nothing, Just end) -> logTime <= end
           (Nothing, Nothing) -> True

    getLogTimestamp :: JobLogGroup -> UTCTime
    getLogTimestamp (JobLogLines timestamp _) = timestamp
    getLogTimestamp (JobLogGroup timestamp _ _ _) = timestamp

createJobLogGroupChildren :: BaseContext -> Int -> JobLogGroup -> STM (Node Variable 'JobLogGroupT)
createJobLogGroupChildren bc depth' jobLogGroup = do
  stateVar <- newTVar ()
  ident' <- getIdentifierSTM bc
  toggledVar <- newTVar False

  childrenVar <- case jobLogGroup of
    JobLogLines _ _ -> newTVar []
    JobLogGroup _ _ (Just _status) _children' -> newTVar []  -- Top-level job groups don't have children in the tree
    JobLogGroup _ _ Nothing children' -> do  -- Only nested log groups have children
      childElems <- mapM (createJobLogGroupChildren bc (depth' + 1)) children'
      newTVar childElems

  healthCheckVar <- newTVar NotFetched
  healthCheckThreadVar <- newTVar Nothing
  return $ JobLogGroupNode $ EntityData {
    _static = jobLogGroup
    , _state = stateVar
    , _urlSuffix = ""
    , _toggled = toggledVar
    , _children = childrenVar
    , _healthCheck = healthCheckVar
    , _healthCheckThread = healthCheckThreadVar
    , _depth = depth'
    , _ident = ident'
  }
