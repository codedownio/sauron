{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Sauron.Mutations.Workflow (
  cancelWorkflowRun
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
  withGithubApiSemaphore (githubWithLoggingUnit (cancelWorkflowRunR owner name runId)) >>= \case
    Left err -> logError $ "Failed to cancel workflow run: " <> show err
    Right _ -> do
      chan <- asks eventChan
      liftIO $ writeBChan chan (ToastFired ToastDefault msg)
  where msg = "Cancelled workflow run #" <> show runNumber
