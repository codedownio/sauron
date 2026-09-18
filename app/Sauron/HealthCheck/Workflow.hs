{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

-- | Watching a repo's in-flight workflow runs.
--
-- There used to be one polling thread per running workflow, each fetching its own run's jobs over
-- REST every few seconds. That doesn't scale: half a dozen runs at once is enough to exhaust the
-- hourly REST allowance. Instead a repo gets a single poller, which asks GraphQL for the status of
-- all of its running runs in one request per tick (see 'Sauron.GraphQL.WorkflowRuns'), and only
-- spends REST calls on the jobs of workflows that are actually expanded in the UI.
module Sauron.HealthCheck.Workflow (
  startWorkflowHealthCheckIfNeeded,
  ensureWorkflowRunPoller,
  restartWorkflowHealthCheckIfJobsRunning,
  workflowHealthCheckPeriodUs
  ) where

import Control.Exception.Safe (finally, handleAny)
import Control.Monad.Logger
import qualified Data.Map.Strict as M
import Data.String.Interpolate
import qualified Data.Vector as V
import GitHub
import Relude
import Sauron.Actions.Util
import Sauron.Fetch.Job
import Sauron.GraphQL.WorkflowRuns
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

-- | Make sure the repo a workflow node belongs to has its run poller going.
startWorkflowHealthCheckIfNeeded ::
  BaseContext
  -> Node Variable 'SingleWorkflowT
  -> NonEmpty (SomeNode Variable)
  -> IO ()
startWorkflowHealthCheckIfNeeded baseContext _node parents =
  case (findRepoParent parents, findWorkflowsParent parents) of
    (Just (RepoNode (EntityData {_static=(owner, name), _state=repoState, _healthCheck=repoHealthCheck})), Just (PaginatedWorkflowsNode (EntityData {_children=workflowsChildren, _healthCheckThread=pollerVar}))) ->
      ensureWorkflowRunPoller baseContext owner name pollerVar workflowsChildren
        (runRepoHealthCheck baseContext (owner, name) repoState repoHealthCheck)
    _ -> return ()

-- | Start the repo's workflow run poller if any of its runs are still going and it isn't already
-- running. The poller's handle lives on the workflows list node, and it also publishes itself on
-- each running workflow node so the UI can show which runs are being watched -- which is also what
-- makes collapsing the workflows list stop the polling, since that cancels its children's threads.
ensureWorkflowRunPoller ::
  BaseContext
  -> Name Owner
  -> Name Repo
  -> TVar (Maybe (Async (), Int))  -- ^ The workflows list node's thread handle, which owns the poller
  -> TVar [Node Variable 'SingleWorkflowT]
  -> IO ()  -- ^ Refresh the parent repo's health check, run when a run finishes.
  -> IO ()
ensureWorkflowRunPoller baseContext owner name pollerVar workflowsChildren refreshRepoHealth = do
  anyRunning <- any isRunningNode <$> readTVarIO workflowsChildren
  existing <- readTVarIO pollerVar
  needPoller <- case existing of
    Nothing -> return True
    -- A cancelled poller can leave its handle behind (cancelling the workflow nodes' copies doesn't
    -- clear the workflows node's), so ask the thread itself whether it's still going
    Just (thread, _) -> isJust <$> poll thread

  when (anyRunning && needPoller) $ do
    -- The loop publishes its own handle on the nodes it watches, so it has to be able to see it
    selfVar <- newEmptyTMVarIO
    thread <- async $ do
      self <- atomically (readTMVar selfVar)
      runPoller self

    -- Only take the slot if nobody else claimed it since we looked, so two callers racing here
    -- can't leave two pollers running
    claimed <- atomically $ do
      current <- readTVar pollerVar
      if (asyncThreadId . fst <$> current) == (asyncThreadId . fst <$> existing)
        then do
          putTMVar selfVar thread
          writeTVar pollerVar (Just (thread, workflowHealthCheckPeriodUs))
          return True
        else return False

    if claimed
      then log baseContext LevelInfo [i|Starting workflow run poller for #{untagName owner}/#{untagName name} (period: #{workflowHealthCheckPeriodUs}us)|] Nothing
      else cancel thread
  where
    runPoller self =
      flip finally (clearWatchedMarkers self) $
      flip runReaderT baseContext $
      handleAny (\e -> putStrLn [i|Workflow run poller crashed: #{e}|]) $
      fix $ \loop -> do
        runningNodes <- filter isRunningNode <$> readTVarIO workflowsChildren
        -- Nothing left in flight, so stop; the next fetch or expand starts a fresh poller
        unless (null runningNodes) $ do
          atomically $ forM_ runningNodes $ \(SingleWorkflowNode (EntityData {_healthCheckThread})) ->
            writeTVar _healthCheckThread (Just (self, workflowHealthCheckPeriodUs))
          pollRunningWorkflows runningNodes
          threadDelay (workflowHealthCheckPeriodUs - minimumPollDisplayUs)
          loop

    -- | One GraphQL request covers every running run's status, and the check runs it brings back
    -- with them keep the job statuses current. REST is only spent on the job list itself: once per
    -- run when we first see it, and each tick while its node is expanded.
    pollRunningWorkflows runningNodes = flip finally (setPolling runningNodes False) $ do
      setPolling runningNodes True

      statuses <- case auth baseContext of
        -- Without an OAuth token there's no GraphQL, so every run falls through to REST below
        OAuth _ -> queryWorkflowRunStatuses baseContext owner name [(untagId (workflowRunWorkflowRunId wf), workflowRunHeadSha wf) | SingleWorkflowNode (EntityData {_static=wf}) <- runningNodes] >>= \case
          Right statuses -> return statuses
          Left err -> do
            warn' baseContext [i|(#{untagName owner}/#{untagName name}) Couldn't fetch workflow run statuses: #{err}|]
            return mempty
        _ -> return mempty

      -- Each run's work here is independent, and on the first tick every one of them does a REST
      -- job-list fetch. In sequence that's a round trip apiece, so the summaries trickle in one
      -- workflow at a time; run them together and let the API semaphore bound what's in flight.
      finished <- forConcurrently runningNodes $ \node@(SingleWorkflowNode (EntityData {_static=wf, _ident=nodeIdent, _state, _children=jobChildren, _toggled, _healthCheckThread})) -> do
        let maybeStatus = M.lookup (untagId (workflowRunWorkflowRunId wf)) statuses
        maybeUpdated <- case maybeStatus of
          Just status -> return (Just (applyRunStatus wf status))
          -- GraphQL had nothing for this run (a fork's head commit, say), so ask REST about it
          Nothing -> fetchRunOverRest wf

        whenJust maybeUpdated $ \updated -> when (updated /= wf) $
          atomically $ modifyTVar' workflowsChildren $ map $ \child@(SingleWorkflowNode childEd) ->
            if _ident childEd == nodeIdent then SingleWorkflowNode (childEd { _static = updated }) else child

        -- A collapsed workflow still shows a job summary, so its jobs have to exist and stay
        -- current. Fetching the list is a REST call, but refreshing their statuses isn't: the
        -- check runs from the batch query carry those.
        expanded <- readTVarIO _toggled
        noJobsYet <- (not . isFetchedJobs . workflowNodeStateFetchable) <$> readTVarIO _state
        if expanded || noJobsYet
          then void $ fetchWorkflowJobs owner name (workflowRunWorkflowRunId wf) node
          else whenJust maybeStatus $ \status ->
            readTVarIO jobChildren >>= applyJobStatuses (runStatusJobs status)

        -- Just finished: stop watching it
        case maybeUpdated of
          Just updated | not (isRunningWorkflow updated) -> do
            atomically $ writeTVar _healthCheckThread Nothing
            return True
          _ -> return False

      -- Something finished, so get the repo's icon to its final colour now rather than after the
      -- repo health check's own (much longer) period. Once, however many runs finished this tick.
      when (or finished) $ liftIO refreshRepoHealth

      -- Hold the spinner on for a few animation frames; a poll is over in a fraction of one, and a
      -- glyph that moves for a single frame reads as a glitch rather than as activity
      threadDelay minimumPollDisplayUs

    setPolling :: MonadIO m => [Node Variable 'SingleWorkflowT] -> Bool -> m ()
    setPolling nodes polling = atomically $ forM_ nodes $ \(SingleWorkflowNode (EntityData {_state})) ->
      modifyTVar' _state $ \wns -> wns { workflowNodeStatePolling = polling }

    -- | Refresh the statuses of job nodes we already have from the run's check runs, which are the
    -- same underlying objects as its jobs (a job's check run shares its id).
    applyJobStatuses :: MonadIO m => Map Int JobStatus -> [Node Variable 'SingleJobT] -> m ()
    applyJobStatuses jobStatuses jobNodes = forM_ jobNodes $ \(SingleJobNode (EntityData {_state})) ->
      atomically $ modifyTVar' _state $ \jns -> jns { jnsJob = patchJob (jnsJob jns) }
      where
        patchJob (Fetched job) = Fetched (applyJobStatus jobStatuses job)
        patchJob (Fetching (Just job)) = Fetching (Just (applyJobStatus jobStatuses job))
        patchJob other = other

    fetchRunOverRest wf =
      withGithubApiSemaphore (githubWithLogging (workflowRunR owner name (workflowRunWorkflowRunId wf))) >>= \case
        Right updated -> return (Just updated)
        Left err -> do
          warn' baseContext [i|(#{untagName owner}/#{untagName name}) Couldn't fetch workflow run #{workflowRunWorkflowRunId wf}: #{err}|]
          return Nothing

    clearWatchedMarkers self = atomically $ do
      children <- readTVar workflowsChildren
      forM_ (pollerVar : [v | SingleWorkflowNode (EntityData {_healthCheckThread=v}) <- children]) $ \var ->
        readTVar var >>= \case
          Just (thread, _) | asyncThreadId thread == asyncThreadId self -> writeTVar var Nothing
          _ -> return ()

-- | How long a poll keeps the spinner going, whatever the request actually took.
minimumPollDisplayUs :: Int
minimumPollDisplayUs = 600_000

isFetchedJobs :: Fetchable a -> Bool
isFetchedJobs (Fetched _) = True
isFetchedJobs _ = False

applyJobStatus :: Map Int JobStatus -> Job -> Job
applyJobStatus jobStatuses job = case M.lookup (untagId (jobId job)) jobStatuses of
  Nothing -> job
  Just (JobStatus {..}) -> job { jobStatus = jobStatusStatus, jobConclusion = jobStatusConclusion }

applyRunStatus :: WorkflowRun -> RunStatus -> WorkflowRun
applyRunStatus wf (RunStatus {..}) = wf {
  workflowRunStatus = runStatusStatus
  , workflowRunConclusion = runStatusConclusion
  , workflowRunUpdatedAt = fromMaybe (workflowRunUpdatedAt wf) runStatusUpdatedAt
  }

restartWorkflowHealthCheckIfJobsRunning ::
  BaseContext
  -> Node Variable 'SingleWorkflowT
  -> NonEmpty (SomeNode Variable)
  -> V.Vector Job
  -> IO ()
restartWorkflowHealthCheckIfJobsRunning baseContext (SingleWorkflowNode (EntityData {_static=workflowRun, _ident=nodeIdent})) parents jobs
  | isRunningWorkflow workflowRun = return ()
  | all isJobCompleted jobs = return ()
  | otherwise = case (findRepoParent parents, findWorkflowsParent parents) of
      (Just (RepoNode (EntityData {_static=(owner, name), _state=repoState, _healthCheck=repoHealthCheck})), Just (PaginatedWorkflowsNode (EntityData {_children=workflowsChildren, _healthCheckThread=pollerVar}))) -> do
        runReaderT (withGithubApiSemaphore (githubWithLogging (workflowRunR owner name (workflowRunWorkflowRunId workflowRun)))) baseContext >>= \case
          Left err -> warn' baseContext [i|(#{untagName owner}/#{untagName name}) Couldn't fetch workflow run #{workflowRunWorkflowRunId workflowRun}: #{err}|]
          Right workflowRun' -> do
            log baseContext LevelInfo [i|Workflow #{untagName $ workflowRunName workflowRun} \##{workflowRunRunNumber workflowRun} has jobs running again; picking up its new status|] Nothing
            atomically $ modifyTVar' workflowsChildren $ map $ \child@(SingleWorkflowNode childEd) ->
              if _ident childEd == nodeIdent
              then SingleWorkflowNode (childEd { _static = workflowRun' })
              else child
            let refreshRepoHealth = runRepoHealthCheck baseContext (owner, name) repoState repoHealthCheck
            ensureWorkflowRunPoller baseContext owner name pollerVar workflowsChildren refreshRepoHealth
            -- Same reasoning as when a workflow finishes: get the repo icon back to running now
            -- rather than after the full repo health-check period.
            refreshRepoHealth
      _ -> return ()

isRunningNode :: Node Variable 'SingleWorkflowT -> Bool
isRunningNode (SingleWorkflowNode (EntityData {_static=wf})) = isRunningWorkflow wf

isRunningWorkflow :: WorkflowRun -> Bool
isRunningWorkflow wr = not $ isWorkflowCompleted $ fromMaybe (workflowRunStatus wr) (workflowRunConclusion wr)
