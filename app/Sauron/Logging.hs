module Sauron.Logging (
  -- * Logging functions
  logToModal,
  withLogToModal,
  
  -- * Re-exported LogLevel constructors
  LogLevel(..),
  
  -- * Re-exported from Control.Monad.Logger for convenience
  HasCallStack
) where

import Brick.BChan (writeBChan)
import Control.Monad.Logger (LogLevel(..))
import Data.Time (getCurrentTime, diffUTCTime, NominalDiffTime)
import GHC.Stack (HasCallStack)
import Relude hiding (HasCallStack)
import Sauron.Types (AppEvent(LogEntryAdded), BaseContext(..), LogEntry(..))

-- | Log a message to the modal with optional duration
logToModal :: (MonadIO m, HasCallStack) => BaseContext -> LogLevel -> Text -> Maybe NominalDiffTime -> m ()
logToModal bc level msg maybeDuration = do
  now <- liftIO getCurrentTime
  let logEntry = LogEntry now level msg maybeDuration (Just callStack)
  liftIO $ writeBChan (eventChan bc) (LogEntryAdded logEntry)

-- | Execute an action and log its completion with timing information
withLogToModal :: MonadIO m => BaseContext -> LogLevel -> Text -> m a -> m a
withLogToModal bc level msg action = do
  startTime <- liftIO getCurrentTime
  result <- action
  endTime <- liftIO getCurrentTime
  let duration = diffUTCTime endTime startTime
  logToModal bc level msg (Just duration)
  return result