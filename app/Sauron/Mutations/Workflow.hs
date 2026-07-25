{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Sauron.Mutations.Workflow (
  cancelWorkflowRun
  , rerunWorkflowRun
  , rerunFailedJobs
  ) where

import Brick.BChan (writeBChan)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import GitHub
import Relude
import Sauron.Actions.Util
import Sauron.Logging
import Sauron.Types


cancelWorkflowRun :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Id WorkflowRun -> Integer -> m ()
cancelWorkflowRun owner name runId runNumber =
  workflowRunCommand (cancelWorkflowRunR owner name runId) "cancel workflow run" ("Cancelled workflow run #" <> show runNumber)

rerunWorkflowRun :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Id WorkflowRun -> Integer -> m ()
rerunWorkflowRun owner name runId runNumber =
  workflowRunCommand (reRunWorkflowR owner name runId) "re-run workflow run" ("Re-running all jobs in workflow run #" <> show runNumber)

rerunFailedJobs :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Id WorkflowRun -> Integer -> m ()
rerunFailedJobs owner name runId runNumber =
  workflowRunCommand (reRunFailedJobsR owner name runId) "re-run failed jobs" ("Re-running failed jobs in workflow run #" <> show runNumber)

workflowRunCommand :: (
  MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => GenRequest 'MtUnit 'RW () -> Text -> Text -> m ()
workflowRunCommand req what successMsg =
  withGithubApiSemaphore (githubWithLoggingUnit req) >>= \case
    Left err -> logError $ "Failed to " <> what <> ": " <> show err
    Right _ -> do
      chan <- asks eventChan
      liftIO $ writeBChan chan (ToastFired ToastDefault successMsg)
