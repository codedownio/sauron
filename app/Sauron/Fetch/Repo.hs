{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Sauron.Fetch.Repo (
  fetchRepo
  , fetchRepos
  ) where

import Control.Exception.Safe (bracketOnError_)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Vector as V
import GitHub
import Relude
import Sauron.Actions.Util
import Sauron.Fetch.Core
import Sauron.HealthCheck.Repo (newRepoHealthCheckThread)
import Sauron.HealthCheck.Stop (cancelGatheredHealthCheckThreads, swapChildrenClearingRemoved)
import Sauron.Options (PeriodSpec(..), defaultHealthCheckPeriodUs)
import Sauron.Setup.Common (newRepoNode)
import Sauron.Types

fetchRepo :: (
  HasCallStack, MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> TVar (Fetchable Repo) -> m ()
fetchRepo owner name repoVar = do
  bracketOnError_ (atomically $ markFetching repoVar)
                  (atomically $ writeTVar repoVar (Errored "Repo fetch failed with exception.")) $
    withGithubApiSemaphore (githubWithLogging (repositoryR owner name)) >>= \case
      Left err -> atomically $ writeTVar repoVar (Errored (show err))
      Right x -> atomically $ writeTVar repoVar (Fetched x)

fetchRepos :: (
  HasCallStack, MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Node Variable PaginatedReposT -> m ()
fetchRepos (PaginatedReposNode (EntityData {..})) = do
  (search, _pageInfo, _fetchable) <- readTVarIO _state
  terms <- case search of
    SearchNone -> pure []
    SearchText t -> pure $ T.words t
  let fullQuery = T.intercalate "+" terms

  bc <- ask
  -- Use a TVar to pass results out of the STM callback
  resultsVar <- newTVarIO Nothing
  removedThreads <- fetchPaginatedWithState (searchReposR fullQuery) _state $ \case
    Left err -> do
      (s, p, _) <- readTVar _state
      writeTVar _state (s, p, Errored err)
      swapChildrenClearingRemoved _children []
    Right (SearchResult totalCount results, newPageInfo) -> do
      (s, _, _) <- readTVar _state
      writeTVar _state (s, newPageInfo, Fetched totalCount)
      writeTVar resultsVar (Just results)
      return []
  cancelGatheredHealthCheckThreads bc removedThreads

  -- Create proper repo nodes with children (in IO, outside STM)
  readTVarIO resultsVar >>= \case
    Nothing -> return ()
    Just results -> do
      repoNodes <- forM (V.toList results) $ \r -> do
        let nsName = (simpleOwnerLogin $ repoOwner r, repoName r)
        repoVar <- newTVarIO (Fetched r)
        healthCheckVar <- newTVarIO NotFetched
        let ps@(PeriodSpec period) = defaultHealthCheckPeriodUs
        hcThread <- liftIO $ newRepoHealthCheckThread bc nsName repoVar healthCheckVar ps
        newRepoNode nsName repoVar healthCheckVar (Just (hcThread, period)) (_depth + 1) (getIdentifier bc)
      -- Each fetch builds brand new repo nodes with brand new health check threads, so the
      -- nodes we're replacing must have their threads cancelled. Otherwise every refresh,
      -- search and page of this list would leave another set of them polling forever.
      atomically (swapChildrenClearingRemoved _children repoNodes) >>= cancelGatheredHealthCheckThreads bc
