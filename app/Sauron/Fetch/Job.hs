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
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), diffUTCTime, getCurrentTime)
import qualified Data.Vector as V
import GitHub
import Network.HTTP.Conduit hiding (Proxy)
import Network.HTTP.Types.Status (statusCode)
import qualified Network.URI as URI
import Relude
import Sauron.Actions.Util
import Sauron.Fetch.Core
import Sauron.Fetch.ParseJobLogs
import Sauron.Fetch.ParseWorkflowRunLogs
import Sauron.Logging
import Sauron.Types
import System.IO.Temp (emptySystemTempFile)


fetchWorkflowJobs :: (
  HasCallStack, MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Id WorkflowRun -> Node Variable SingleWorkflowT -> m (Either Error (V.Vector Job))
fetchWorkflowJobs owner name workflowRunId (SingleWorkflowNode (EntityData {..})) = do
  bc <- ask
  bracketOnError_ (atomically $ markFetching _state)
                  (atomically $ writeTVar _state (Errored "Workflow jobs fetch failed with exception.")) $
    withGithubApiSemaphore (githubWithLogging (jobsForWorkflowRunR owner name workflowRunId FetchAll)) >>= \case
      Left err -> do
        atomically $ writeTVar _state (Errored (show err))
        return $ Left err
      Right (WithTotalCount results totalCount) -> atomically $ do
        writeTVar _state (Fetched totalCount)

        -- Preserve existing job nodes and their toggle state
        existingChildren <- readTVar _children

        -- Build a map of existing jobs by their IDs
        existingJobsMap <- foldM (\acc node -> case node of
          SingleJobNode (EntityData {_state=jobState}) -> do
            (jobFetchable, _) <- readTVar jobState
            case jobFetchable of
              Fetched existingJob -> return $ Map.insert (jobId existingJob) node acc
              Fetching (Just existingJob) -> return $ Map.insert (jobId existingJob) node acc
              _ -> return acc
          ) (Map.empty :: Map.Map (Id Job) (Node Variable SingleJobT)) existingChildren

        (writeTVar _children =<<) $ forM (V.toList results) $ \job@(Job {jobId}) -> do
          case Map.lookup jobId existingJobsMap of
            Just existingNode@(SingleJobNode existingEntityData) -> do
              -- Update existing node's job data while preserving toggle state
              let EntityData {_state=jobState} = existingEntityData
              (_, logsFetchable) <- readTVar jobState
              writeTVar jobState (Fetched job, logsFetchable)
              return existingNode
            Nothing -> do
              -- Create new node for new jobs
              entityData <- makeEmptyElemWithState bc job (NotFetched, NotFetched) "" (_depth + 1)
              let EntityData {_state=jobState, _children=jobChildren} = entityData
              writeTVar jobState (Fetched job, NotFetched)

              -- Create dummy child for log fetching
              let dummyLogGroup = JobLogLines (UTCTime (fromGregorian 1970 1 1) 0) ["Loading job logs..."]
              dummyLogEntityData <- makeEmptyElemWithState bc dummyLogGroup () "" (_depth + 2)
              writeTVar jobChildren [JobLogGroupNode dummyLogEntityData]

              return $ SingleJobNode entityData

        return $ Right results

fetchJob :: (
  HasCallStack, MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Id Job -> Node Variable SingleJobT -> m ()
fetchJob owner name jobId (SingleJobNode (EntityData {_state})) = do
  withGithubApiSemaphore (githubWithLoggingResponse (jobR owner name jobId)) >>= \case
    Left _err -> return () -- Silently fail for health checks
    Right (responseBody -> updatedJob) -> atomically $ do
      (_, logsFetchable) <- readTVar _state
      writeTVar _state (Fetched updatedJob, logsFetchable)

fetchJobLogs :: (
  HasCallStack, MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Job -> Node Variable SingleJobT -> Maybe (Node Variable SingleWorkflowT) -> m ()
fetchJobLogs owner name job@(Job {jobConclusion, jobName=jn}) node workflowParent = do
  bc <- ask
  -- Use workflow run logs (zip) when the job is completed, since it has
  -- logs properly split by step. Fall back to per-job logs when running.
  case jobConclusion of
    Just conclusion -> do
      info' bc [i|fetchJobLogs: job "#{untagName jn}" is completed (#{conclusion}), using workflow run logs|]
      fetchJobLogsFromWorkflowRun owner name job node workflowParent
    Nothing -> do
      info' bc [i|fetchJobLogs: job "#{untagName jn}" is still running, using per-job logs|]
      fetchJobLogsPerJob owner name job node

-- | Fetch logs for a single job using the per-job logs endpoint.
-- Used when the workflow is still running (workflow run logs aren't available yet).
fetchJobLogsPerJob :: (
  HasCallStack, MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Job -> Node Variable SingleJobT -> m ()
fetchJobLogsPerJob owner name (Job {jobId, jobSteps}) (SingleJobNode (EntityData {..})) = do
  bc@(BaseContext {auth, manager}) <- ask
  bracketOnError_ (atomically $ do
                     (jobFetchable, _logsFetchable) <- readTVar _state
                     writeTVar _state (jobFetchable, Fetching Nothing))
                  (atomically $ do
                     (jobFetchable, _logsFetchable) <- readTVar _state
                     writeTVar _state (jobFetchable, Errored "Job logs fetch failed with exception.")) $ do
    withGithubApiSemaphore (liftIO $ executeRequestWithMgrAndRes manager auth (downloadJobLogsR owner name jobId)) >>= \case
      Left err -> do
        logError' bc [i|fetchJobLogsPerJob: downloadJobLogsR failed: #{err}|]
        now <- liftIO getCurrentTime
        let errorMessage = formatJobLogsError err
        atomically $ do
          (jobFetchable, _logsFetchable) <- readTVar _state
          writeTVar _state (jobFetchable, Errored errorMessage)
          childNode <- createJobLogLinesNode bc (_depth + 1) (JobLogLines now (T.splitOn "\n" errorMessage))
          writeTVar _children [childNode]
      Right response -> do
        debug' bc [i|fetchJobLogsPerJob: got logs download URL: #{responseBody response}|]
        logs <- simpleHttp (URI.uriToString id (responseBody response) "")

        tempFile <- liftIO $ emptySystemTempFile "sauron-job-logs-.txt"
        liftIO $ writeFileLBS tempFile logs
        debug' bc [i|fetchJobLogsPerJob: wrote raw logs to #{tempFile}|]

        let parsedLogs = parseJobLogs (T.splitOn "\n" (decodeUtf8 logs))
        let stepsList = V.toList jobSteps
            stepsWithNextStart = zipWith (\step nextStep -> (step, jobStepStartedAt nextStep))
                                         stepsList (drop 1 stepsList)
                              ++ [(s, Nothing) | s <- take 1 (reverse stepsList)]
        children' <- liftIO $ atomically $ do
          mapM (\(step, nextStart) -> createJobStepNode bc (_depth + 1) parsedLogs step nextStart) stepsWithNextStart

        atomically $ do
          writeTVar _children children'
          (jobFetchable, _logsFetchable) <- readTVar _state
          writeTVar _state (jobFetchable, Fetched (parsedLogs, FlatLogTimestampSplit))

-- | Fetch logs for a job using the workflow run logs endpoint.
-- This returns a zip file with logs split by step (for multi-step jobs),
-- avoiding the timestamp-based splitting issues with the per-job endpoint.
-- When a workflow parent is available, populates logs for all sibling jobs too.
fetchJobLogsFromWorkflowRun :: (
  HasCallStack, MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Job -> Node Variable SingleJobT -> Maybe (Node Variable SingleWorkflowT) -> m ()
fetchJobLogsFromWorkflowRun owner name (Job {jobRunId, jobName}) (SingleJobNode (EntityData {..})) workflowParent = do
  bc@(BaseContext {auth, manager}) <- ask
  let jobNameText = untagName jobName
  info' bc [i|fetchJobLogsFromWorkflowRun: starting for job "#{jobNameText}", runId=#{jobRunId}|]
  bracketOnError_ (atomically $ do
                     (jobFetchable, _logsFetchable) <- readTVar _state
                     writeTVar _state (jobFetchable, Fetching Nothing))
                  (atomically $ do
                     (jobFetchable, _logsFetchable) <- readTVar _state
                     writeTVar _state (jobFetchable, Errored "Job logs fetch failed with exception.")) $ do
    -- Check if another job already fetched and populated our logs
    alreadyFetched <- liftIO $ atomically $ do
      (_, logsFetchable) <- readTVar _state
      case logsFetchable of
        Fetched _ -> return True
        _ -> return False
    if alreadyFetched then
      info' bc [i|fetchJobLogsFromWorkflowRun: logs already fetched for "#{jobNameText}", skipping|]
    else do
      info' bc [i|fetchJobLogsFromWorkflowRun: requesting workflow run logs URL|]
      withGithubApiSemaphore (liftIO $ executeRequestWithMgrAndRes manager auth (downloadWorkflowRunLogsR owner name jobRunId)) >>= \case
        Left err -> do
          logError' bc [i|fetchJobLogsFromWorkflowRun: downloadWorkflowRunLogsR failed: #{err}|]
          now <- liftIO getCurrentTime
          let errorMessage = formatJobLogsError err
          atomically $ do
            (jobFetchable, _logsFetchable) <- readTVar _state
            writeTVar _state (jobFetchable, Errored errorMessage)
            childNode <- createJobLogLinesNode bc (_depth + 1) (JobLogLines now (T.splitOn "\n" errorMessage))
            writeTVar _children [childNode]
        Right response -> do
          info' bc [i|fetchJobLogsFromWorkflowRun: got redirect URL, downloading zip...|]
          zipBytes <- simpleHttp (URI.uriToString id (responseBody response) "")
          info' bc [i|fetchJobLogsFromWorkflowRun: downloaded zip (#{BL.length zipBytes} bytes)|]

          tempFile <- liftIO $ emptySystemTempFile "sauron-workflow-run-logs-.zip"
          liftIO $ BL.writeFile tempFile zipBytes
          info' bc [i|fetchJobLogsFromWorkflowRun: wrote zip to #{tempFile}|]

          let entryPaths = listZipEntryPaths zipBytes
          let entryPathsStr = T.intercalate ", " (map toText entryPaths)
          info' bc [i|fetchJobLogsFromWorkflowRun: zip contains #{length entryPaths} entries: [#{entryPathsStr}]|]

          -- Populate logs for all sibling jobs from this zip
          siblingJobNodes <- case workflowParent of
            Just (SingleWorkflowNode (EntityData {_children=workflowChildren})) -> do
              nodes <- liftIO $ readTVarIO workflowChildren
              info' bc [i|fetchJobLogsFromWorkflowRun: found #{length nodes} sibling job nodes from workflow parent|]
              return nodes
            Nothing -> do
              info' bc [i|fetchJobLogsFromWorkflowRun: no workflow parent, only populating this job|]
              return []

          let allJobNodes = if null siblingJobNodes
                then [(SingleJobNode (EntityData {..}))]  -- just this job
                else [n | n@(SingleJobNode _) <- siblingJobNodes]

          info' bc [i|fetchJobLogsFromWorkflowRun: populating logs for #{length allJobNodes} jobs|]

          forM_ allJobNodes $ \(SingleJobNode (EntityData {_state=sibState, _children=sibChildren, _depth=sibDepth, _static=Job {jobName=sibJobName', jobSteps=sibJobSteps'}})) -> do
            let sibJobName = untagName sibJobName'
            let sibJobSteps = V.toList sibJobSteps'
            case extractJobLogsFromZip zipBytes sibJobName of
              Nothing ->
                info' bc [i|fetchJobLogsFromWorkflowRun: WARNING: no zip entry found for job "#{sibJobName}"|]
              Just (ZipJobStepLogs stepEntries) -> do
                info' bc [i|fetchJobLogsFromWorkflowRun: job "#{sibJobName}": extracted #{length stepEntries} per-step log files|]
                let allParsedLogs = concatMap (\(_, _, logs) -> logs) stepEntries
                children' <- liftIO $ atomically $
                  mapM (\(stepNum, stepName, parsedLogs) ->
                    createZipStepNode bc (sibDepth + 1) stepNum stepName parsedLogs
                  ) stepEntries

                atomically $ do
                  (sibJobFetchable, sibLogsFetchable) <- readTVar sibState
                  case sibLogsFetchable of
                    Fetched _ -> return ()
                    _ -> do
                      writeTVar sibChildren children'
                      writeTVar sibState (sibJobFetchable, Fetched (allParsedLogs, PerStepLogs))

              Just (ZipJobFlatLogs parsedLogs) -> do
                info' bc [i|fetchJobLogsFromWorkflowRun: job "#{sibJobName}": using flat log file, splitting by step timestamps|]
                let stepsWithNextStart = zipWith (\step nextStep -> (step, jobStepStartedAt nextStep))
                                                  sibJobSteps (drop 1 sibJobSteps)
                                       ++ [(s, Nothing) | s <- take 1 (reverse sibJobSteps)]
                children' <- liftIO $ atomically $
                  mapM (\(step, nextStart) -> createJobStepNode bc (sibDepth + 1) parsedLogs step nextStart) stepsWithNextStart

                atomically $ do
                  (sibJobFetchable, sibLogsFetchable) <- readTVar sibState
                  case sibLogsFetchable of
                    Fetched _ -> return ()
                    _ -> do
                      writeTVar sibChildren children'
                      writeTVar sibState (sibJobFetchable, Fetched (parsedLogs, FlatLogTimestampSplit))

          info' bc [i|fetchJobLogsFromWorkflowRun: done populating all jobs|]

-- * Util

-- | Create a step node from zip-extracted per-step logs (already split by step).
createZipStepNode :: BaseContext -> Int -> Int -> Text -> [JobLogGroup] -> STM (Node Variable 'JobLogGroupT)
createZipStepNode bc depth' _stepNum stepName parsedLogs = do
  let stepTimestamp = case parsedLogs of
        (g:_) -> getFirstTimestamp g
        [] -> UTCTime (fromGregorian 1970 1 1) 0
  createJobLogGroupChildren bc depth' (JobLogGroup stepTimestamp stepName Nothing Nothing parsedLogs)
  where
    getFirstTimestamp (JobLogLines t _) = t
    getFirstTimestamp (JobLogGroup t _ _ _ _) = t

createJobStepNode :: BaseContext -> Int -> [JobLogGroup] -> JobStep -> Maybe UTCTime -> STM (Node Variable 'JobLogGroupT)
createJobStepNode bc depth' allLogs jobStep nextStepStart = do
  let stepTitle = untagName (jobStepName jobStep)
  let stepTimestamp = fromMaybe (UTCTime (fromGregorian 1970 1 1) 0) (jobStepStartedAt jobStep)
  let stepStatus = Just $ fromMaybe (jobStepStatus jobStep) (jobStepConclusion jobStep)
  let stepDuration = diffUTCTime <$> jobStepCompletedAt jobStep <*> jobStepStartedAt jobStep
  createJobLogGroupChildren bc depth' (JobLogGroup stepTimestamp stepTitle stepStatus stepDuration logsForStep)
  where
    logsForStep :: [JobLogGroup]
    logsForStep =
      let startTime = jobStepStartedAt jobStep
      in filter (logInTimeRange startTime nextStepStart) allLogs

    logInTimeRange :: Maybe UTCTime -> Maybe UTCTime -> JobLogGroup -> Bool
    logInTimeRange startTime endTime logGroup =
      let logTime = getLogTimestamp logGroup
      in case (startTime, endTime) of
           (Just start, Just end) -> logTime >= start && logTime < end
           (Just start, Nothing) -> logTime >= start
           (Nothing, Just end) -> logTime < end
           (Nothing, Nothing) -> True

    getLogTimestamp :: JobLogGroup -> UTCTime
    getLogTimestamp (JobLogLines timestamp _) = timestamp
    getLogTimestamp (JobLogGroup timestamp _ _ _ _) = timestamp

createJobLogGroupChildren :: BaseContext -> Int -> JobLogGroup -> STM (Node Variable 'JobLogGroupT)
createJobLogGroupChildren bc depth' jobLogGroup = do
  stateVar <- newTVar ()
  ident' <- getIdentifierSTM bc
  toggledVar <- newTVar False

  childrenVar <- case jobLogGroup of
    JobLogLines _ _ -> newTVar []
    JobLogGroup _ _ (Just _status) _ _children' -> newTVar []  -- Top-level job groups don't have children in the tree
    JobLogGroup _ _ Nothing _ children' -> do  -- Only nested log groups have children
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

createJobLogLinesNode :: BaseContext -> Int -> JobLogGroup -> STM (Node Variable 'JobLogGroupT)
createJobLogLinesNode bc depth' jobLogGroup = do
  stateVar <- newTVar ()
  ident' <- getIdentifierSTM bc
  toggledVar <- newTVar False
  childrenVar <- newTVar []

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

-- | Format an error from job logs fetch into a user-friendly message.
-- Handles specific HTTP status codes with meaningful explanations.
formatJobLogsError :: Error -> Text
formatJobLogsError (HTTPError (HttpExceptionRequest _ (StatusCodeException response _))) =
  formatStatusCode (statusCode (responseStatus response))
formatJobLogsError (HTTPError httpErr) = "Network error: " <> toText (show httpErr :: String)
formatJobLogsError (ParseError msg) = "Parse error: " <> msg
formatJobLogsError (JsonError msg) = "JSON error: " <> msg
formatJobLogsError (UserError msg) = "Error: " <> msg

-- | Format a status code into a user-friendly message
formatStatusCode :: Int -> Text
formatStatusCode 410 = "Logs unavailable (HTTP 410): logs have been deleted."
formatStatusCode 404 = "Logs not found (HTTP 404): the job may have been deleted or never generated logs.\n\nNote that we can't retrieve the logs from currently running jobs; see https://github.com/codedownio/sauron/issues/24."
formatStatusCode 403 = "Access denied (HTTP 403): you may not have permission to view these logs."
formatStatusCode 502 = "GitHub server error (HTTP 502): please try again later."
formatStatusCode 503 = "GitHub service unavailable (HTTP 503): please try again later."
formatStatusCode code = "HTTP error " <> show code <> ": failed to fetch logs."
