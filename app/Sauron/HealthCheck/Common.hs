module Sauron.HealthCheck.Common (
  clearOwnHealthCheckThread
  ) where

import Relude
import UnliftIO.Async
import UnliftIO.Concurrent (myThreadId)


-- | Clear a node's health check thread handle when the loop exits, so a dead thread can't
-- block a future check from starting. Only clears the handle if it still points at the
-- calling thread, so a thread that's being cancelled and replaced can't clear its
-- replacement's handle.
clearOwnHealthCheckThread :: MonadIO m => TVar (Maybe (Async (), Int)) -> m ()
clearOwnHealthCheckThread threadVar = do
  tid <- myThreadId
  atomically $ readTVar threadVar >>= \case
    Just (asy, _) | asyncThreadId asy == tid -> writeTVar threadVar Nothing
    _ -> return ()
