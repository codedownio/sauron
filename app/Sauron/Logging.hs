module Sauron.Logging (
  -- * Logging functions
  log
  , withLogToModal

  , debug
  , info
  , warn
  , logError

  , debug'
  , info'
  , warn'
  , logError'

  -- * Re-exported LogLevel constructors
  , LogLevel(..)

  -- * Re-exported from Control.Monad.Logger for convenience
  , HasCallStack
) where

import Brick.BChan (writeBChan)
import Control.Monad.Logger (LogLevel(..))
import Data.Time (getCurrentTime, diffUTCTime, NominalDiffTime)
import GHC.Stack (HasCallStack)
import Relude hiding (HasCallStack)
import Sauron.Types (AppEvent(LogEntryAdded), BaseContext(..), LogEntry(..))

log :: (MonadIO m, HasCallStack) => BaseContext -> LogLevel -> Text -> Maybe NominalDiffTime -> m ()
log bc level msg maybeDuration = do
  now <- liftIO getCurrentTime
  let logEntry = LogEntry now level msg maybeDuration (Just callStack)
  liftIO $ writeBChan (eventChan bc) (LogEntryAdded logEntry)

debug :: (MonadIO m, MonadReader BaseContext m, HasCallStack) => Text -> m ()
debug msg = do
  bc <- ask
  debug' bc msg

info :: (MonadIO m, MonadReader BaseContext m, HasCallStack) => Text -> m ()
info msg = do
  bc <- ask
  info' bc msg

warn :: (MonadIO m, MonadReader BaseContext m, HasCallStack) => Text -> m ()
warn msg = do
  bc <- ask
  warn' bc msg

logError :: (MonadIO m, MonadReader BaseContext m, HasCallStack) => Text -> m ()
logError msg = do
  bc <- ask
  logError' bc msg

debug' :: (MonadIO m, HasCallStack) => BaseContext -> Text -> m ()
debug' = logWithLevel' LevelDebug

info' :: (MonadIO m, HasCallStack) => BaseContext -> Text -> m ()
info' = logWithLevel' LevelInfo

warn' :: (MonadIO m, HasCallStack) => BaseContext -> Text -> m ()
warn' = logWithLevel' LevelWarn

logError' :: (MonadIO m, HasCallStack) => BaseContext -> Text -> m ()
logError' = logWithLevel' LevelError

logWithLevel' :: (MonadIO m, HasCallStack) => LogLevel -> BaseContext -> Text -> m ()
logWithLevel' lvl bc msg = do
  now <- liftIO getCurrentTime
  let logEntry = LogEntry now lvl msg Nothing (Just callStack)
  liftIO $ writeBChan (eventChan bc) (LogEntryAdded logEntry)

-- | Execute an action and log its completion with timing information
withLogToModal :: MonadIO m => BaseContext -> LogLevel -> Text -> m a -> m a
withLogToModal bc level msg action = do
  startTime <- liftIO getCurrentTime
  result <- action
  endTime <- liftIO getCurrentTime
  let duration = diffUTCTime endTime startTime
  log bc level msg (Just duration)
  return result
