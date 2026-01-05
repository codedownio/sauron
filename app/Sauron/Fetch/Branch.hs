{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Sauron.Fetch.Branch (
  fetchBranches
  , fetchYourBranches
  , fetchActiveBranches
  , fetchStaleBranches
  , fetchBranchCommits
  , fetchBranchWithInfoCommits
  , fetchCommitDetails

  , getOverallBranches
  ) where

import Control.Exception.Safe (bracketOnError_)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import qualified Data.List as List
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import qualified Data.Vector as V
import GitHub
import Relude
import Sauron.Actions.Util
import Sauron.Fetch.Core
import qualified Sauron.GraphQL as GraphQL
import Sauron.Logging
import Sauron.Types
import Sauron.UI.BranchWithInfo (formatCommitTimeText, formatPRInfoText, formatCheckStatusWithWidth, formatAheadBehindWithWidth)


fetchBranches :: (
  HasCallStack, MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Node Variable PaginatedBranchesT -> m ()
fetchBranches owner name (PaginatedBranchesNode (EntityData {..})) = do
  bc <- ask
  fetchPaginatedWithState (branchesForR owner name) _state $ \case
    Left err -> do
      (s, p, _) <- readTVar _state
      writeTVar _state (s, p, Errored (show err))
      writeTVar _children []
    Right (branches, newPageInfo) -> do
      (s, _, _) <- readTVar _state
      writeTVar _state (s, newPageInfo, Fetched (V.length branches))
      (writeTVar _children =<<) $ forM (V.toList branches) $ \branch@(Branch {..}) ->
        SingleBranchNode <$> makeEmptyElemWithState bc branch NotFetched ("/tree/" <> branchName) (_depth + 1)

fetchBranchesWithFilter :: (
  HasCallStack, MonadReader BaseContext m, MonadIO m, MonadMask m
  )
  => Name Owner
  -> Name Repo
  -> Maybe Text
  -> TVar (Search, PageInfo, Fetchable TotalCount)
  -> TVar [Node Variable SingleBranchWithInfoT]
  -> Int
  -> Text
  -> ([BranchWithInfo] -> [BranchWithInfo])
  -> Text
  -> m ()
fetchBranchesWithFilter owner name repoDefaultBranch stateVar childrenVar depth' logPrefix filterFn _logSuffix = do
  bc <- ask

  bracketOnError_ (atomically $ do
                    (s, p, f) <- readTVar stateVar
                    writeTVar stateVar (s, p, Fetching (fetchableCurrent f)))
                  (atomically $ do
                    (s, p, _) <- readTVar stateVar
                    writeTVar stateVar (s, p, Errored $ logPrefix <> " fetch failed with exception.")) $ do
    case getAuthToken bc of
      Nothing -> liftIO $ do
        logToModal bc LevelError (logPrefix <> ": No auth token available") Nothing
        atomically $ do
          (s, p, _) <- readTVar stateVar
          writeTVar stateVar (s, p, Errored "No auth token available for GraphQL query")
      Just authToken -> do
        liftIO $ do
          -- Read current page info to determine pagination
          (_, currentPageInfo, _) <- readTVarIO stateVar
          let currentPage = pageInfoCurrentPage currentPageInfo

          -- Fetch a substantial number of branches to enable client-side filtering and pagination
          -- We fetch more than we need to allow for filtering (many branches may be filtered out)
          let branchesToFetch = max 100 (currentPage * pageSize * 3)  -- Fetch at least 100, or enough for 3 pages worth

          -- Fetch branches with commit info using GraphQL
          result <- withLogToModal bc LevelInfo (logPrefix <> ": Querying GraphQL for " <> toPathPart owner <> "/" <> toPathPart name) $
            GraphQL.queryBranchesWithInfos (\msg -> logToModal bc LevelDebug msg Nothing) authToken (toPathPart owner) (toPathPart name) repoDefaultBranch branchesToFetch
          case result of
            Left err -> atomically $ do
              (s, p, _) <- readTVar stateVar
              writeTVar stateVar (s, p, Errored $ toText err)
              writeTVar childrenVar []
            Right branchesWithCommits -> do
              -- Apply the provided filter function and sort by date
              let allFilteredBranches = GraphQL.sortBranchesByDate $ filterFn branchesWithCommits
              let totalBranches = length allFilteredBranches
              let totalPages = max 1 $ (totalBranches + pageSize - 1) `div` pageSize  -- Ceiling division, at least 1 page

              -- Calculate the slice for the current page
              let startIdx = (currentPage - 1) * pageSize
              let currentPageBranches = take pageSize $ drop startIdx allFilteredBranches

              let newPageInfo = PageInfo {
                    pageInfoCurrentPage = currentPage
                    , pageInfoFirstPage = if totalPages > 0 then Just 1 else Nothing
                    , pageInfoPrevPage = if currentPage > 1 then Just (currentPage - 1) else Nothing
                    , pageInfoNextPage = if currentPage < totalPages then Just (currentPage + 1) else Nothing
                    , pageInfoLastPage = if totalPages > 0 then Just totalPages else Nothing
                    }

              -- Calculate column widths based on all branches in current page
              currentTime <- getCurrentTime
              let columnWidths = calculateColumnWidths currentTime currentPageBranches

              -- Store only the current page's branches in the node state
              atomically $ do
                (s, _, _) <- readTVar stateVar
                writeTVar stateVar (s, newPageInfo, Fetched totalBranches)
                (writeTVar childrenVar =<<) $ forM currentPageBranches $ \branchInfo ->
                  SingleBranchWithInfoNode <$> makeEmptyElemWithState bc (branchInfo, columnWidths) NotFetched ("/tree/" <> branchWithInfoBranchName branchInfo) (depth' + 1)
  where
    getAuthToken :: BaseContext -> Maybe Text
    getAuthToken bc = case auth bc of
      OAuth token -> Just $ decodeUtf8 token
      _ -> Nothing  -- Only OAuth tokens supported for now

    calculateColumnWidths :: UTCTime -> [BranchWithInfo] -> ColumnWidths
    calculateColumnWidths _ [] = ColumnWidths 0 0 0 0  -- Default widths for empty list
    calculateColumnWidths currentTime branches = ColumnWidths {
      cwCommitTime = fromMaybe 0 $ viaNonEmpty List.maximum $ map (T.length . formatCommitTimeText currentTime) branches
      , cwCheckStatus = fromMaybe 0 $ viaNonEmpty List.maximum $ map (snd . formatCheckStatusWithWidth) branches
      , cwAheadBehind = fromMaybe 0 $ viaNonEmpty List.maximum $ map (snd . formatAheadBehindWithWidth) branches
      , cwPRInfo = fromMaybe 0 $ viaNonEmpty List.maximum $ map (T.length . formatPRInfoText) branches
    }

fetchYourBranches :: (
  HasCallStack, MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Maybe Text -> Node Variable PaginatedYourBranchesT -> m ()
fetchYourBranches owner name repoDefaultBranch (PaginatedYourBranchesNode (EntityData {..})) = do
  bc <- ask
  liftIO (getUserName bc) >>= \case
    Nothing -> liftIO $ do
      logToModal bc LevelError "fetchYourBranches: Could not get current user name" Nothing
      atomically $ do
        (s, p, _) <- readTVar _state
        writeTVar _state (s, p, Errored "Could not get current user name")
    Just userName ->
      fetchBranchesWithFilter owner name repoDefaultBranch _state _children _depth "fetchYourBranches"
        (GraphQL.filterBranchesByAuthor userName) "your branches"
  where
    getUserName :: BaseContext -> IO (Maybe Text)
    getUserName bc = do
      -- Get the current authenticated user's name from GitHub API
      result <- runReaderT (withGithubApiSemaphore (githubWithLogging userInfoCurrentR)) bc
      case result of
        Left _err -> return Nothing
        Right user -> return $ Just $ toPathPart $ userLogin user

fetchActiveBranches :: (
  HasCallStack, MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Maybe Text -> Node Variable PaginatedActiveBranchesT -> m ()
fetchActiveBranches owner name repoDefaultBranch (PaginatedActiveBranchesNode (EntityData {..})) =
  fetchBranchesWithFilter owner name repoDefaultBranch _state _children _depth "fetchActiveBranches"
    (GraphQL.filterBranchesByActivity 90) "active branches"

fetchStaleBranches :: (
  HasCallStack, MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Maybe Text -> Node Variable PaginatedStaleBranchesT -> m ()
fetchStaleBranches owner name repoDefaultBranch (PaginatedStaleBranchesNode (EntityData {..})) =
  fetchBranchesWithFilter owner name repoDefaultBranch _state _children _depth "fetchStaleBranches"
    (GraphQL.filterBranchesByInactivity 90) "stale branches"

getOverallBranches :: (
  MonadReader BaseContext m, MonadIO m
  ) => Name Owner -> Name Repo -> Node Variable OverallBranchesT -> m ()
getOverallBranches _owner _name (OverallBranchesNode (EntityData {..})) = do
  bc <- ask
  -- Create categorized branch sections similar to GitHub's interface
  categorizedChildren <- atomically $ do
    yourBranchesEd <- makeEmptyElemWithState bc () (SearchNone, emptyPageInfo, NotFetched) "your-branches" (_depth + 1)
    activeBranchesEd <- makeEmptyElemWithState bc () (SearchNone, emptyPageInfo, NotFetched) "active-branches" (_depth + 1)
    staleBranchesEd <- makeEmptyElemWithState bc () (SearchNone, emptyPageInfo, NotFetched) "stale-branches" (_depth + 1)
    return [
      SomeNode (PaginatedYourBranchesNode yourBranchesEd)
      , SomeNode (PaginatedActiveBranchesNode activeBranchesEd)
      , SomeNode (PaginatedStaleBranchesNode staleBranchesEd)
      ]

  atomically $ do
    writeTVar _state ()
    writeTVar _children categorizedChildren

fetchBranchCommits :: (
  HasCallStack, MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Node Variable SingleBranchT -> m ()
fetchBranchCommits owner name (SingleBranchNode (EntityData {_static=branch, ..})) = do
  bc <- ask
  let branchSha = branchCommitSha $ branchCommit branch
  bracketOnError_ (atomically $ markFetching _state)
                  (atomically $ writeTVar _state (Errored "Branch commits fetch failed with exception.")) $
    withGithubApiSemaphore (githubWithLogging (commitsWithOptionsForR owner name (FetchAtLeast 10) [CommitQuerySha branchSha])) >>= \case
      Left err -> atomically $ do
        writeTVar _state (Errored (show err))
        writeTVar _children []
      Right commits -> atomically $ do
        writeTVar _state (Fetched commits)
        (writeTVar _children =<<) $ forM (V.toList commits) $ \commit@(Commit {..}) ->
          SingleCommitNode <$> makeEmptyElemWithState bc commit NotFetched ("/commit/" <> T.pack (toString (untagName commitSha))) (_depth + 1)

fetchBranchWithInfoCommits :: (
  HasCallStack, MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Node Variable SingleBranchWithInfoT -> m ()
fetchBranchWithInfoCommits owner name (SingleBranchWithInfoNode (EntityData {_static=(branchInfo, _columnWidths), ..})) = do
  bc <- ask
  case branchWithInfoCommitOid branchInfo of
    Nothing -> atomically $ writeTVar _state (Errored "No commit OID available for branch with info")
    Just commitOid -> do
      bracketOnError_ (atomically $ markFetching _state)
                      (atomically $ writeTVar _state (Errored "Branch commits fetch failed with exception.")) $
        withGithubApiSemaphore (githubWithLogging (commitsWithOptionsForR owner name (FetchAtLeast 10) [CommitQuerySha commitOid])) >>= \case
          Left err -> atomically $ do
            writeTVar _state (Errored (show err))
            writeTVar _children []
          Right commits -> atomically $ do
            writeTVar _state (Fetched commits)
            (writeTVar _children =<<) $ forM (V.toList commits) $ \commit@(Commit {..}) ->
              SingleCommitNode <$> makeEmptyElemWithState bc commit NotFetched ("/commit/" <> T.pack (toString (untagName commitSha))) (_depth + 1)

fetchCommitDetails :: (
  HasCallStack, MonadReader BaseContext m, MonadIO m, MonadMask m
  ) => Name Owner -> Name Repo -> Name Commit -> TVar (Fetchable Commit) -> m ()
fetchCommitDetails owner name commitSha commitVar = do
  bracketOnError_ (atomically $ markFetching commitVar)
                  (atomically $ writeTVar commitVar (Errored "Commit details fetch failed with exception.")) $
    withGithubApiSemaphore (githubWithLogging (commitR owner name commitSha)) >>= \case
      Left err -> atomically $ writeTVar commitVar (Errored (show err))
      Right detailedCommit -> atomically $ writeTVar commitVar (Fetched detailedCommit)
