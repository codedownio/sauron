{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Sauron.HealthCheck.Pull (
  startPullChecksHealthCheckIfNeeded
  , pullChecksHealthCheckPeriodUs
  ) where

import Control.Exception.Safe (handleAny)
import Control.Monad.Logger
import Data.String.Interpolate
import qualified Data.Vector as V
import GitHub
import Relude
import Sauron.Actions.Util (findRepoParent)
import Sauron.Fetch.Pull (fetchPullDetailsAndChecks)
import Sauron.Logging
import Sauron.Types
import UnliftIO.Async
import UnliftIO.Concurrent


pullChecksHealthCheckPeriodUs :: Int
pullChecksHealthCheckPeriodUs = 5_000_000

-- | Start a polling thread for a PR's CI checks if any are still pending and the node
-- doesn't already have one. The thread re-fetches the PR details and checks every
-- period and stops once every check has completed (or the fetch stops returning data).
startPullChecksHealthCheckIfNeeded ::
  BaseContext
  -> Node Variable 'SinglePullT
  -> NonEmpty (SomeNode Variable)
  -> IO ()
startPullChecksHealthCheckIfNeeded baseContext (SinglePullNode (EntityData {_static=pull, _state, _healthCheckThread})) parents =
  case findRepoParent parents of
    Just (RepoNode (EntityData {_static=(owner, name)})) -> do
      pending <- checksStillPending . pullNodeStateChecks <$> readTVarIO _state
      existing <- readTVarIO _healthCheckThread
      when (pending && isNothing existing) $ do
        log baseContext LevelInfo [i|Starting checks health check thread for PR \##{unIssueNumber (issueNumber pull)} (period: #{pullChecksHealthCheckPeriodUs}us)|] Nothing
        newThread <- async $ runLoop owner name
        atomically $ writeTVar _healthCheckThread (Just (newThread, pullChecksHealthCheckPeriodUs))
    Nothing -> return ()
  where
    runLoop owner name =
      flip runReaderT baseContext $
      handleAny (\e -> putStrLn [i|PR checks health check thread crashed: #{e}|]) $
      fix $ \loop -> do
        threadDelay pullChecksHealthCheckPeriodUs
        fetchPullDetailsAndChecks owner name (issueNumber pull) _state
        stillPending <- checksStillPending . pullNodeStateChecks <$> readTVarIO _state
        if stillPending
          then loop
          else atomically $ writeTVar _healthCheckThread Nothing

checksStillPending :: Fetchable (V.Vector CheckRun) -> Bool
checksStillPending fetchable = case fetchableCurrent fetchable of
  Nothing -> False
  Just runs -> not (V.null runs) && any (\r -> checkRunStatus r /= CheckRunCompleted) runs
