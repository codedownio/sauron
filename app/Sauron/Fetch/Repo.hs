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
  fetchPaginatedWithState (searchReposR fullQuery) _state $ \case
    Left err -> do
      (s, p, _) <- readTVar _state
      writeTVar _state (s, p, Errored err)
      writeTVar _children []
    Right (SearchResult totalCount results, newPageInfo) -> do
      (s, _, _) <- readTVar _state
      writeTVar _state (s, newPageInfo, Fetched totalCount)
      (writeTVar _children =<<) $ forM (V.toList results) $ \r -> do
        let nsName = (simpleOwnerLogin $ repoOwner r, repoName r)
        entityData@(EntityData {_state=innerState}) <- makeEmptyElemWithState bc nsName NotFetched "" (_depth + 1)
        writeTVar innerState (Fetched r)
        return $ RepoNode entityData
