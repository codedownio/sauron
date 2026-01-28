{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Sauron.Actions (
  openBrowserToUrl

  , refreshVisibleLines
  , refreshLine
  , refreshSelected
  , refreshOnZoom
  , fetchOnOpen
  , fetchOnOpenIfNecessary
  ) where

import Control.Monad.IO.Class
import qualified Data.Vector as V
import GitHub
import Relude
import Sauron.Actions.Util (findRepoParent, openBrowserToUrl)
import Sauron.Fetch.Branch
import Sauron.Fetch.Issue
import Sauron.Fetch.Job
import Sauron.Fetch.Notification
import Sauron.Fetch.Pull
import Sauron.Fetch.Repo
import Sauron.Fetch.Workflow
import Sauron.HealthCheck.Job (startJobHealthCheckIfNeeded)
import Sauron.HealthCheck.Workflow (startWorkflowHealthCheckIfNeeded)
import Sauron.Types
import Sauron.UI.Util (isFetchingOrFetched)
import UnliftIO.Async

-- import Sauron.HealthCheck.Job (startJobHealthCheckIfNeeded)


refreshSelected :: (MonadIO m) => BaseContext -> Node Variable a -> NonEmpty (SomeNode Variable) -> m (Async ())
refreshSelected = fetchOnOpen

refreshOnZoom :: (MonadIO m) => BaseContext -> Node Variable a -> NonEmpty (SomeNode Variable) -> m ()
refreshOnZoom bc node parents = fetchOnOpenIfNecessary bc node parents >>= \case
  Nothing -> return ()
  Just asy -> wait asy

refreshLine :: (MonadIO m) => BaseContext -> Node Variable a -> NonEmpty (SomeNode Variable) -> m (Async ())
refreshLine bc (RepoNode (EntityData {_static=(owner, name), _state})) _parents =
  liftIO $ async $ flip runReaderT bc $ fetchRepo owner name _state
refreshLine bc item@(PaginatedIssuesNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ async $ liftIO $ runReaderT (fetchIssues owner name item) bc
refreshLine bc item@(PaginatedPullsNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ async $ liftIO $ runReaderT (fetchPulls owner name item) bc
refreshLine bc item@(PaginatedWorkflowsNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ async $ liftIO $ runReaderT (fetchWorkflows owner name item) bc
refreshLine bc item@(PaginatedBranchesNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ async $ liftIO $ runReaderT (fetchBranches owner name item) bc
refreshLine bc item@(PaginatedYourBranchesNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name), _state}))) = do
  liftIO $ async $ withRepoDefaultBranch _state $ \defaultBranch -> runReaderT (fetchYourBranches owner name defaultBranch item) bc
refreshLine bc item@(PaginatedActiveBranchesNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name), _state}))) =
  liftIO $ async $ withRepoDefaultBranch _state $ \defaultBranch -> runReaderT (fetchActiveBranches owner name defaultBranch item) bc
refreshLine bc item@(PaginatedStaleBranchesNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name), _state}))) =
  liftIO $ async $ withRepoDefaultBranch _state $ \defaultBranch -> runReaderT (fetchStaleBranches owner name defaultBranch item) bc
refreshLine bc item@(PaginatedNotificationsNode _) _ =
  liftIO $ async $ liftIO $ runReaderT (fetchNotifications item) bc
refreshLine bc item@(SingleJobNode _) parents = do
  liftIO $ void $ startJobHealthCheckIfNeeded bc item parents
  liftIO $ async $ return ()
refreshLine _ _ _ = liftIO $ async $ return ()

refreshVisibleLines :: (
  MonadReader BaseContext m, MonadIO m
  ) => V.Vector (SomeNode Variable) -> m ()
refreshVisibleLines elems = do
  baseContext <- ask
  refreshVisibleLines' baseContext [] (V.toList elems)

refreshVisibleLines' :: MonadIO m => BaseContext -> [SomeNode Variable] -> [SomeNode Variable] -> m ()
refreshVisibleLines' baseContext parents nodes = do
  -- Be careful not to log from the calling thread of this function, because
  -- this function is called in Main.hs, before the event loop starts. Logging
  -- writes to the Brick event chan, and it is bounded, so if we fill it up
  -- before starting the event loop, we can crash with an STM deadlock.

  forM_ nodes $ \someNode@(SomeNode node) -> do
    parentAsy <- refreshLine baseContext node (someNode :| parents)

    whenM (readTVarIO (_toggled (getEntityData node))) $ void $ liftIO $ async $ do
      wait parentAsy
      atomically (getExistentialChildrenWrapped node)
        >>= refreshVisibleLines' baseContext (someNode : parents)

fetchOnOpenIfNecessary :: (MonadIO m) => BaseContext -> Node Variable a -> NonEmpty (SomeNode Variable) -> m (Maybe (Async ()))
fetchOnOpenIfNecessary bc node parents = do
  shouldFetchOnExpand node >>= \case
    True -> (Just <$>) $ fetchOnOpen bc node parents
    False -> return Nothing

fetchOnOpen :: (MonadIO m) => BaseContext -> Node Variable a -> NonEmpty (SomeNode Variable) -> m (Async ())
-- Container nodes that just organize other nodes - do nothing
fetchOnOpen _bc (HeadingNode _) _parents = liftIO $ async (return ())
fetchOnOpen bc item@(RepoNode (EntityData {_children})) parents = do
  asyncs <- readTVarIO _children >>= mapM (\(SomeNode x) -> refreshLine bc x (SomeNode item :| toList parents))
  liftIO $ async (mapM_ wait asyncs)

-- Paginated nodes - fetch their lists when opened
fetchOnOpen bc item@(PaginatedIssuesNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ async $ liftIO $ runReaderT (fetchIssues owner name item) bc
fetchOnOpen bc item@(PaginatedPullsNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ async $ liftIO $ runReaderT (fetchPulls owner name item) bc
fetchOnOpen bc item@(PaginatedWorkflowsNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ async $ liftIO $ runReaderT (fetchWorkflows owner name item) bc
fetchOnOpen bc item@(PaginatedBranchesNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ async $ liftIO $ runReaderT (fetchBranches owner name item) bc
fetchOnOpen bc item@(PaginatedYourBranchesNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name), _state}))) =
  withRepoDefaultBranch' (liftIO $ async (return ())) _state $ \defaultBranch -> liftIO $ async $ runReaderT (fetchYourBranches owner name defaultBranch item) bc
fetchOnOpen bc item@(PaginatedActiveBranchesNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name), _state}))) =
  withRepoDefaultBranch' (liftIO $ async (return ())) _state $ \defaultBranch -> liftIO $ async $ runReaderT (fetchActiveBranches owner name defaultBranch item) bc
fetchOnOpen bc item@(PaginatedStaleBranchesNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name), _state}))) =
  withRepoDefaultBranch' (liftIO $ async (return ())) _state $ \defaultBranch -> liftIO $ async $ runReaderT (fetchStaleBranches owner name defaultBranch item) bc
fetchOnOpen bc item@(PaginatedNotificationsNode _) _parents =
  liftIO $ async $ liftIO $ runReaderT (fetchNotifications item) bc
fetchOnOpen bc item@(PaginatedReposNode _) _parents =
  liftIO $ async $ liftIO $ runReaderT (fetchRepos item) bc

-- Nodes that fetch their children when opened
fetchOnOpen bc item@(SingleWorkflowNode (EntityData {_static=workflowRun})) parents@(findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) = do
  liftIO $ async $ liftIO $ flip runReaderT bc $ do
    void $ fetchWorkflowJobs owner name (workflowRunWorkflowRunId workflowRun) item
    liftIO $ void $ startWorkflowHealthCheckIfNeeded bc item parents
fetchOnOpen bc item@(SingleBranchNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ async $ liftIO $ runReaderT (fetchBranchCommits owner name item) bc
fetchOnOpen bc item@(SingleBranchWithInfoNode _) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ async $ liftIO $ runReaderT (fetchBranchWithInfoCommits owner name item) bc

-- Content nodes that fetch their details when explicitly opened
fetchOnOpen bc (SingleIssueNode (EntityData {_static=issue, _state})) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ async $ liftIO $ runReaderT (fetchIssueComments owner name (issueNumber issue) _state) bc
fetchOnOpen bc (SinglePullNode (EntityData {_static=pull, _state})) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ async $ liftIO $ runReaderT (fetchPullComments owner name (issueNumber pull) _state) bc
fetchOnOpen bc (SingleCommitNode (EntityData {_static=commit, _state})) (findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) =
  liftIO $ async $ liftIO $ runReaderT (fetchCommitDetails owner name (commitSha commit) _state) bc

fetchOnOpen bc item@(SingleJobNode (EntityData {_state, _static=job@(Job {jobId})})) parents@(findRepoParent -> Just (RepoNode (EntityData {_static=(owner, name)}))) = do
  liftIO $ async $ liftIO $ flip runReaderT bc $ do
    void $ concurrently (fetchJob owner name jobId item)
                        (fetchJobLogs owner name job item)
    liftIO $ void $ startJobHealthCheckIfNeeded bc item parents

fetchOnOpen _ _ _ = liftIO $ async (return ())


withRepoDefaultBranch :: MonadIO m => TVar (Fetchable Repo) -> (Maybe Text -> m ()) -> m ()
withRepoDefaultBranch = withRepoDefaultBranch' (return ())

withRepoDefaultBranch' :: MonadIO m => m a -> TVar (Fetchable Repo) -> (Maybe Text -> m a) -> m a
withRepoDefaultBranch' defaultValue fetchableVar action = readTVarIO fetchableVar >>= \case
  Fetched (Repo {..}) -> action repoDefaultBranch
  Fetching (Just (Repo {..})) -> action repoDefaultBranch
  _ -> defaultValue


-- | This should be synced up with how fetchOnOpen works
shouldFetchOnExpand :: MonadIO m => Node Variable a -> m Bool
shouldFetchOnExpand (HeadingNode (EntityData {_state})) = return False
shouldFetchOnExpand (RepoNode (EntityData {_children})) =
  -- Just check if the paginated issues node is fetched or not
  readTVarIO _children >>= anyM
    (\case
        (SomeNode (PaginatedIssuesNode child)) -> (not . isFetchingOrFetched . thd) <$> readTVarIO (_state child)
        _ -> return False
    )
shouldFetchOnExpand (PaginatedIssuesNode (EntityData {_state})) = not . isFetchingOrFetched . thd <$> readTVarIO _state
shouldFetchOnExpand (PaginatedPullsNode (EntityData {_state})) = not . isFetchingOrFetched . thd <$> readTVarIO _state
shouldFetchOnExpand (PaginatedWorkflowsNode (EntityData {_state})) = not . isFetchingOrFetched . thd <$> readTVarIO _state
shouldFetchOnExpand (PaginatedReposNode (EntityData {_state})) = not . isFetchingOrFetched . thd <$> readTVarIO _state
shouldFetchOnExpand (PaginatedBranchesNode (EntityData {_state})) = not . isFetchingOrFetched . thd <$> readTVarIO _state
shouldFetchOnExpand (PaginatedYourBranchesNode (EntityData {_state})) = not . isFetchingOrFetched . thd <$> readTVarIO _state
shouldFetchOnExpand (PaginatedActiveBranchesNode (EntityData {_state})) = not . isFetchingOrFetched . thd <$> readTVarIO _state
shouldFetchOnExpand (PaginatedStaleBranchesNode (EntityData {_state})) = not . isFetchingOrFetched . thd <$> readTVarIO _state
shouldFetchOnExpand (PaginatedNotificationsNode (EntityData {_state})) = not . isFetchingOrFetched . thd <$> readTVarIO _state
shouldFetchOnExpand (SingleIssueNode (EntityData {_state})) = not . isFetchingOrFetched <$> readTVarIO _state
shouldFetchOnExpand (SinglePullNode (EntityData {_state})) = not . isFetchingOrFetched <$> readTVarIO _state
shouldFetchOnExpand (SingleWorkflowNode (EntityData {_state})) = not . isFetchingOrFetched <$> readTVarIO _state
shouldFetchOnExpand (SingleJobNode (EntityData {_state})) = do
  (jobFetchable, logsFetchable) <- readTVarIO _state
  return $ not (isFetchingOrFetched jobFetchable) || not (isFetchingOrFetched logsFetchable)
shouldFetchOnExpand (SingleBranchNode (EntityData {_state})) = not . isFetchingOrFetched <$> readTVarIO _state
shouldFetchOnExpand (SingleBranchWithInfoNode (EntityData {_state})) = not . isFetchingOrFetched <$> readTVarIO _state
shouldFetchOnExpand (SingleCommitNode (EntityData {_state})) = not . isFetchingOrFetched <$> readTVarIO _state
shouldFetchOnExpand (SingleNotificationNode (EntityData {_state})) = not . isFetchingOrFetched <$> readTVarIO _state
shouldFetchOnExpand (JobLogGroupNode (EntityData {})) = return True

thd :: (a, b, c) -> c
thd (_, _, z) = z
