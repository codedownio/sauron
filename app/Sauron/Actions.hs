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
import Sauron.Actions.Util
import Sauron.Fetch.Branch
import Sauron.Fetch.Issue
import Sauron.Fetch.Job
import Sauron.Fetch.Notification
import Sauron.Fetch.Pull
import Sauron.Fetch.Repo
import Sauron.Fetch.Workflow
import Sauron.HealthCheck.Job (startJobHealthCheckIfNeeded)
import Sauron.HealthCheck.Workflow (startWorkflowHealthCheckIfNeeded)
import Sauron.Logging
import Sauron.Types
import Sauron.UI.Util (isFetchingOrFetched)
import UnliftIO.Async


refreshSelected :: (MonadIO m) => BaseContext -> Node Variable a -> NonEmpty (SomeNode Variable) -> m (Async ())
refreshSelected = fetchOnOpen

refreshOnZoom :: (MonadIO m, SomeNodeConstraints Fixed a) => BaseContext -> Node Variable a -> NonEmpty (SomeNode Variable) -> m ()
refreshOnZoom bc node parents = fetchOnOpenIfNecessary bc node parents >>= wait

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
  let cb (SomeNode node) parents = liftIO $ refreshLine baseContext node parents
  forM_ (V.toList elems) $ forEachVisibleNode cb []

forEachVisibleNode :: (MonadIO m) => (SomeNode Variable -> NonEmpty (SomeNode Variable) -> IO (Async ())) -> [SomeNode Variable] -> SomeNode Variable -> m ()
forEachVisibleNode cb parents someNode@(SomeNode node) = do
  -- Be careful not to log from the calling thread of this function, because
  -- this function is called in Main.hs, before the event loop starts. Logging
  -- writes to the Brick event chan, and it is bounded, so if we fill it up
  -- before starting the event loop, we can crash with an STM deadlock.

  parentAsy <- liftIO $ cb someNode (someNode :| parents)

  whenM (readTVarIO (_toggled (getEntityData node))) $ void $ liftIO $ async $ do
    wait parentAsy
    atomically (getExistentialChildrenWrapped node)
      >>= mapM_ (forEachVisibleNode cb (someNode : parents))

fetchOnOpenIfNecessary :: (MonadIO m, SomeNodeConstraints Fixed a) => BaseContext -> Node Variable a -> NonEmpty (SomeNode Variable) -> m (Async ())
fetchOnOpenIfNecessary bc node parents = do
  maybeAsy <- shouldFetchOnExpand node >>= \case
    True -> (Just <$>) $ fetchOnOpen bc node parents
    False -> return Nothing

  liftIO $ async $ do
    whenJust maybeAsy wait

    let cb (SomeNode node') parents' = liftIO $ onBecameVisible bc node' parents'
    atomically (getExistentialChildrenWrapped node) >>= \children' ->
      liftIO $ forConcurrently_ children' (forEachVisibleNode cb (SomeNode node : toList parents))

onBecameVisible :: (MonadIO m) => BaseContext -> Node Variable a -> NonEmpty (SomeNode Variable) -> m (Async ())
onBecameVisible bc item@(SingleWorkflowNode (EntityData {_children})) parents = do
  -- After fetching, start health checks for any running workflows
  info' bc [i|Single workflow node became visible: #{item}|]
  liftIO (startWorkflowHealthCheckIfNeeded bc item (SomeNode item :| toList parents)) >>= \case
    Just x -> pure x
    Nothing -> liftIO $ async (return ())
onBecameVisible _ _ _ = liftIO $ async (return ())

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
