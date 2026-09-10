{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | Key handling for the tabbed pull request modal (the zoom modal on a PR node).
module Sauron.Event.PullModal (
  handlePullModalKey
  , openPullModalOnTab
  , switchPullModalTab
  , fileViewedState
  , zoomedPullNode
  ) where

import Brick as B
import Brick.BChan (writeBChan)
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Vector as V
import GitHub
import qualified Graphics.Vty as Vty
import Lens.Micro
import Relude hiding (Down)
import Sauron.Actions (refreshOnZoom)
import Sauron.Actions.Util (findRepoParent)
import Sauron.Event.Helpers (modifyPullModal, withFixedElemAndParents)
import Sauron.Fetch.Pull (fetchPullCommitDetail, fetchPullCommits, fetchPullFiles)
import Sauron.GraphQL.PullRequestFiles (setFileViewedState)
import Sauron.Types
import Sauron.UI.Toast (showToast)
import UnliftIO.Async


fileViewedState :: Map Text FileViewedState -> File -> FileViewedState
fileViewedState states file = fromMaybe FileUnviewed $ M.lookup (fileFilename file) states

-- | The pull request node the zoom modal is showing, if it's showing one. Returns
-- the variable node (for fetching) alongside its repo and fixed state.
zoomedPullNode :: AppState -> IO (Maybe (Node Variable 'SinglePullT, Name Owner, Name Repo))
zoomedPullNode s =
  readTVarIO (_appModalVariable s) >>= \case
    Just (PullRequestModalState {_pullModalNode=node, _pullModalParents=parents}) ->
      return $ case nonEmpty parents >>= findRepoParent of
        Just (RepoNode (EntityData {_static=(owner, name)})) -> Just (node, owner, name)
        Nothing -> Nothing
    _ -> return Nothing

-- | Zoom in on the selected pull request and show the given tab
openPullModalOnTab :: AppState -> PullModalTab -> EventM ClickableName AppState ()
openPullModalOnTab s tab = do
  withFixedElemAndParents s $ \_ (SomeNode variableEl) parents -> case variableEl of
    pullNode@(SinglePullNode {}) -> do
      refreshOnZoom (s ^. appBaseContext) variableEl parents
      liftIO $ atomically $ writeTVar (_appModalVariable s) $
        Just (newPullRequestModalState tab pullNode (toList parents))
    _ -> return ()
  startTabFetch s tab

-- | Show a tab, kicking off its fetch the first time it's opened
switchPullModalTab :: AppState -> PullModalTab -> EventM ClickableName AppState ()
switchPullModalTab s tab = do
  modifyPullModal s (\m -> m { _pullModalTab = tab })
  vScrollToBeginning (viewportScroll ZoomModalContent)
  startTabFetch s tab

-- | Kick off a tab's fetch if it hasn't been fetched yet
startTabFetch :: AppState -> PullModalTab -> EventM ClickableName AppState ()
startTabFetch s tab =
  liftIO (zoomedPullNode s) >>= \case
    Nothing -> return ()
    Just (SinglePullNode (EntityData {_static=issue, _state=stateVar}), owner, name) -> do
      nodeState <- liftIO $ readTVarIO stateVar
      let bc = s ^. appBaseContext
      let needsFetch fetchable = case fetchable of
            NotFetched -> True
            Errored _ -> True
            _ -> False
      liftIO $ void $ async $ flip runReaderT bc $ case tab of
        TabCommits | needsFetch (pullNodeStateCommits nodeState) ->
          fetchPullCommits owner name (issueNumber issue) stateVar
        TabReview | needsFetch (pullNodeStateFiles nodeState) ->
          fetchPullFiles owner name (issueNumber issue) stateVar
        _ -> return ()

-- | Handle a key in the pull request modal. Returns True if the key was consumed.
handlePullModalKey :: AppState -> ModalState Fixed -> Vty.Key -> EventM ClickableName AppState Bool
handlePullModalKey s modalState@(PullRequestModalState {_pullModalNode=SinglePullNode (EntityData {_state=nodeState})}) key
  -- Tab switching works from any tab
  | Just tab <- keyToTab key = True <$ switchPullModalTab s tab
  | key == Vty.KChar '\t' = True <$ switchPullModalTab s (cycleTab 1)
  | key == Vty.KBackTab = True <$ switchPullModalTab s (cycleTab (-1))
  | otherwise = case _pullModalTab modalState of
      TabReview -> handleReviewKey
      TabCommits -> handleCommitsKey
      _ -> return False
  where
    keyToTab (Vty.KChar '1') = Just TabConversation
    keyToTab (Vty.KChar '2') = Just TabCommits
    keyToTab (Vty.KChar '3') = Just TabChecks
    keyToTab (Vty.KChar '4') = Just TabReview
    keyToTab _ = Nothing

    cycleTab delta =
      let tabs = [minBound .. maxBound]
      in fromMaybe TabConversation $ tabs !!? ((fromEnum (_pullModalTab modalState) + delta) `mod` length tabs)

    -- * Review tab

    files = fromMaybe mempty $ fetchableCurrent (pullNodeStateFiles nodeState)
    viewedStates = pullNodeStateViewedStates nodeState
    currentFile = _pullModalCurrentFile modalState
    fileCount = V.length files

    handleReviewKey
      | key `elem` [Vty.KRight, Vty.KChar 'n'] = True <$ moveToFile (currentFile + 1)
      | key `elem` [Vty.KLeft, Vty.KChar 'p'] = True <$ moveToFile (currentFile - 1)
      | key == Vty.KChar 'v' = True <$ setCurrentViewed (currentViewedState /= FileViewed)
      | key == Vty.KChar 'V' = do
          when (currentViewedState /= FileViewed) $ setCurrentViewed True
          True <$ jumpToNextUnviewed
      | key == Vty.KChar 'u' = True <$ jumpToNextUnviewed
      | otherwise = return False

    currentViewedState = maybe FileUnviewed (fileViewedState viewedStates) (files V.!? currentFile)

    moveToFile ix'
      | ix' < 0 || ix' >= fileCount || ix' == currentFile = return ()
      | otherwise = do
          modifyPullModal s (\m -> m { _pullModalCurrentFile = ix' })
          vScrollToBeginning (viewportScroll ZoomModalContent)

    -- Update the local state immediately and fire the mutation in the background,
    -- reverting if GitHub rejects it
    setCurrentViewed viewed =
      whenJust (files V.!? currentFile) $ \file -> do
        let path = fileFilename file
        let newState = if viewed then FileViewed else FileUnviewed
        liftIO (zoomedPullNode s) >>= \case
          Nothing -> return ()
          Just (SinglePullNode (EntityData {_state=stateVar}), _, _) -> do
            liftIO $ atomically $ modifyTVar' stateVar $ \ps ->
              ps { pullNodeStateViewedStates = M.insert path newState (pullNodeStateViewedStates ps) }
            let bc = s ^. appBaseContext
            whenJust (pullNodeStatePullRequestId nodeState) $ \prId ->
              liftIO $ void $ async $
                setFileViewedState bc prId path viewed >>= \case
                  Right () -> return ()
                  Left err -> do
                    atomically $ modifyTVar' stateVar $ \ps ->
                      ps { pullNodeStateViewedStates = M.insert path currentViewedState (pullNodeStateViewedStates ps) }
                    warnToast bc ("Failed to update viewed state: " <> err)

    jumpToNextUnviewed = do
      -- Re-read the states, since marking the current file just changed them
      states <- liftIO (zoomedPullNode s) >>= \case
        Just (SinglePullNode (EntityData {_state=stateVar}), _, _) ->
          pullNodeStateViewedStates <$> liftIO (readTVarIO stateVar)
        _ -> return viewedStates
      let candidates = [(currentFile + offset) `mod` fileCount | fileCount > 0, offset <- [1 .. fileCount]]
      case find (\ix' -> maybe False (\f -> fileViewedState states f /= FileViewed) (files V.!? ix')) candidates of
        Just ix' -> moveToFile ix'
        Nothing -> showToast ToastSuccess "All files viewed"

    -- * Commits tab

    commits = fromMaybe mempty $ fetchableCurrent (pullNodeStateCommits nodeState)
    selectedCommit = _pullModalSelectedCommit modalState

    handleCommitsKey
      | key == Vty.KUp = True <$ moveCommit (-1)
      | key == Vty.KDown = True <$ moveCommit 1
      | key `elem` [Vty.KEnter, Vty.KChar '\t'] = True <$ toggleSelectedCommit
      | otherwise = return False

    moveCommit delta =
      modifyPullModal s (\m -> m { _pullModalSelectedCommit = max 0 (min (V.length commits - 1) (selectedCommit + delta)) })

    toggleSelectedCommit =
      whenJust (commits V.!? selectedCommit) $ \commit -> do
        let sha = untagName (commitSha commit)
        if Set.member sha (_pullModalExpandedCommits modalState)
          then modifyPullModal s (\m -> m { _pullModalExpandedCommits = Set.delete sha (_pullModalExpandedCommits m) })
          else do
            modifyPullModal s (\m -> m { _pullModalExpandedCommits = Set.insert sha (_pullModalExpandedCommits m) })
            -- Fetch the commit's patches the first time it's expanded
            liftIO (zoomedPullNode s) >>= \case
              Just (SinglePullNode (EntityData {_state=stateVar}), owner, name)
                | not (M.member sha (pullNodeStateCommitDetails nodeState)) ->
                    liftIO $ void $ async $ runReaderT (fetchPullCommitDetail owner name (commitSha commit) stateVar) (s ^. appBaseContext)
              _ -> return ()
handlePullModalKey _ _ _ = return False

warnToast :: BaseContext -> Text -> IO ()
warnToast bc msg = writeBChan (eventChan bc) (ToastFired ToastError msg)
