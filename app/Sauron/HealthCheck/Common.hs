module Sauron.HealthCheck.Common (
  stopHealthCheckThread
  ) where

import Relude
import UnliftIO.Async

stopHealthCheckThread :: TVar (Maybe (Async ())) -> IO ()
stopHealthCheckThread threadVar = do
  maybeThread <- readTVarIO threadVar
  case maybeThread of
    Nothing -> return ()
    Just thread -> do
      cancel thread
      atomically $ writeTVar threadVar Nothing