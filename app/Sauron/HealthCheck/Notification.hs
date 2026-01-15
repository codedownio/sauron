{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Sauron.HealthCheck.Notification (
  newNotificationHealthCheckThread
  ) where

import Control.Exception.Safe (handleAny)
import Control.Monad.IO.Class
import Data.List (lookup)
import Data.String.Interpolate
import qualified Data.Vector as V
import GitHub
import Network.HTTP.Client (responseHeaders, responseBody)
import Network.HTTP.Types.Header (Header, hLastModified)
import Relude
import Sauron.Actions.Util (withGithubApiSemaphore, githubWithLoggingResponse)
import Sauron.Logging
import Sauron.Types
import UnliftIO.Async
import UnliftIO.Concurrent

-- | Create a health check thread for notifications that respects GitHub's polling guidelines
-- Uses Last-Modified header for efficient polling and respects X-Poll-Interval header
newNotificationHealthCheckThread :: (
  HasCallStack
  )
  => BaseContext
  -> TVar (Search, PageInfo, Fetchable Int)  -- The notifications state
  -> IO (Async ())
newNotificationHealthCheckThread baseContext stateVar = async $ do
  log baseContext LevelInfo "Starting health check thread for notifications (using GitHub polling guidelines)" Nothing

  -- Initial poll interval (60 seconds as a default)
  pollIntervalVar <- newTVarIO (60 :: Int)
  lastModifiedVar <- newTVarIO Nothing

  handleAny (\e -> putStrLn [i|Notification health check thread crashed: #{e}|]) $ forever $ do
    -- Fetch current poll interval
    currentPollInterval <- readTVarIO pollIntervalVar

    -- Get the last modified value if we have one
    lastModified <- readTVarIO lastModifiedVar

    liftIO $ flip runReaderT baseContext $ do
      -- Make the API request with Last-Modified header if we have one
      let request = getNotificationsR optionsAll (FetchAtLeast 1)  -- TODO: Need to add If-Modified-Since header support

      withGithubApiSemaphore (githubWithLoggingResponse request) >>= \case
        Left err -> do
          -- Log error but don't update the state
          info' baseContext [i|Notification health check failed: #{err}|]

        Right response -> do
          -- Extract headers
          let headers = responseHeaders response
              newPollInterval = extractPollInterval headers
              newLastModified = extractLastModified headers
              notifications = responseBody response

          -- Update poll interval if provided
          whenJust newPollInterval $ \interval -> do
            debug' baseContext [i|GitHub requested notification poll interval: #{interval} seconds|]
            atomically $ writeTVar pollIntervalVar interval

          -- Update last modified
          atomically $ writeTVar lastModifiedVar newLastModified

          -- Update the notification count in state
          -- Only update if we got actual data (not 304 Not Modified)
          when (V.length notifications > 0 || isNothing lastModified) $ do
            atomically $ do
              (search, pageInfo, _) <- readTVar stateVar
              writeTVar stateVar (search, pageInfo, Fetched (V.length notifications))

          info' baseContext [i|Notification health check: #{V.length notifications} notifications|]

    -- Sleep for the poll interval
    threadDelay (currentPollInterval * 1000000)  -- Convert seconds to microseconds

extractPollInterval :: [Header] -> Maybe Int
extractPollInterval headers =
  case lookup "X-Poll-Interval" headers of
    Nothing -> Nothing
    Just val -> readMaybe (toString $ (decodeUtf8 val :: Text))

extractLastModified :: [Header] -> Maybe ByteString
extractLastModified headers = lookup hLastModified headers
