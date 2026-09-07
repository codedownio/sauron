{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Sauron.Event.PRReviewModal (
  handlePRReviewModalEvent
  , handlePRReviewModalKey
  , openPRReviewModal
  , fileViewedState
  ) where

import Brick as B
import Brick.BChan
import qualified Data.Map as M
import qualified Data.Vector as V
import GitHub
import qualified Graphics.Vty as Vty
import Lens.Micro
import Relude hiding (Down)
import Sauron.Actions.Util (withGithubApiSemaphore', githubWithLogging')
import Sauron.GraphQL.PullRequestFiles (queryPullRequestViewedStates, setFileViewedState)
import Sauron.Types
import Sauron.UI.Toast (showToast)
import UnliftIO.Async


fileViewedState :: Map Text FileViewedState -> File -> FileViewedState
fileViewedState states file = fromMaybe FileUnviewed $ M.lookup (fileFilename file) states

-- | Kick off the file list (REST) and viewed state (GraphQL) fetches; the modal opens
-- when the 'PRReviewModalReady' event comes back.
openPRReviewModal :: BaseContext -> Issue -> Name Owner -> Name Repo -> EventM ClickableName AppState ()
openPRReviewModal bc issue@(Issue {issueNumber=issueNumber@(IssueNumber number)}) owner name =
  liftIO $ void $ async $ do
    filesResult <- withGithubApiSemaphore' (requestSemaphore bc) $
      githubWithLogging' bc $ pullRequestFilesR owner name issueNumber FetchAll
    event <- case filesResult of
      Left err -> return $ PRReviewModalFetchFailed $ show err
      Right files -> queryPullRequestViewedStates bc owner name number >>= \case
        Left err -> return $ PRReviewModalFetchFailed err
        Right (prId, states) -> return $ PRReviewModalReady issue owner name prId files states
    writeBChan (eventChan bc) (PRReviewModalEvent event)

handlePRReviewModalEvent :: AppState -> PRReviewModalEvent -> EventM ClickableName AppState ()
handlePRReviewModalEvent _s (PRReviewModalReady issue owner name prId files states) = do
  -- Start on the first file that still needs review
  let startIndex = fromMaybe 0 $ V.findIndex (\f -> fileViewedState states f /= FileViewed) files
  modify (appModal ?~ PRReviewModalState issue owner name prId files states startIndex)
  vScrollToBeginning (viewportScroll PRReviewModalContent)

handlePRReviewModalEvent _s (PRReviewModalFetchFailed err) =
  showToast ToastError ("Failed to fetch PR files: " <> err)

handlePRReviewModalEvent _s (FileViewedMarkFailed err path previousState) = do
  modify (appModal . _Just . reviewViewedStates %~ M.insert path previousState)
  showToast ToastError ("Failed to update viewed state: " <> err)

-- | Handle a key press while the review modal is up. Returns True if the key was consumed.
handlePRReviewModalKey :: AppState -> ModalState Fixed -> Vty.Key -> EventM ClickableName AppState Bool
handlePRReviewModalKey s modalState@(PRReviewModalState {_reviewFiles, _reviewViewedStates, _reviewCurrentFile}) key
  | key `elem` [Vty.KRight, Vty.KChar 'n'] = True <$ moveToFile (_reviewCurrentFile + 1)
  | key `elem` [Vty.KLeft, Vty.KChar 'p'] = True <$ moveToFile (_reviewCurrentFile - 1)
  | key == Vty.KChar 'v' = True <$ setCurrentViewed (currentState /= FileViewed)
  | key == Vty.KChar 'V' = do
      when (currentState /= FileViewed) $ setCurrentViewed True
      True <$ jumpToNextUnviewed
  | key == Vty.KChar 'u' = True <$ jumpToNextUnviewed
  | key == Vty.KUp = True <$ vScrollBy (viewportScroll PRReviewModalContent) (-1)
  | key == Vty.KDown = True <$ vScrollBy (viewportScroll PRReviewModalContent) 1
  | key == Vty.KPageUp = True <$ vScrollPage (viewportScroll PRReviewModalContent) Up
  | key == Vty.KPageDown = True <$ vScrollPage (viewportScroll PRReviewModalContent) Down
  | otherwise = return False
  where
    fileCount = V.length _reviewFiles

    currentState = maybe FileUnviewed (fileViewedState _reviewViewedStates) (_reviewFiles V.!? _reviewCurrentFile)

    moveToFile ix'
      | ix' < 0 || ix' >= fileCount || ix' == _reviewCurrentFile = return ()
      | otherwise = do
          modify (appModal . _Just . reviewCurrentFile .~ ix')
          vScrollToBeginning (viewportScroll PRReviewModalContent)

    -- Update the local state immediately and fire the mutation in the background; a
    -- 'FileViewedMarkFailed' event reverts on failure.
    setCurrentViewed viewed =
      whenJust (_reviewFiles V.!? _reviewCurrentFile) $ \file -> do
        let path = fileFilename file
        modify (appModal . _Just . reviewViewedStates %~ M.insert path (if viewed then FileViewed else FileUnviewed))
        liftIO $ void $ async $
          setFileViewedState bc (_reviewPullRequestId modalState) path viewed >>= \case
            Right () -> return ()
            Left err -> writeBChan (eventChan bc) (PRReviewModalEvent (FileViewedMarkFailed err path currentState))

    jumpToNextUnviewed = do
      -- Re-read the modal since setCurrentViewed may have just changed the map
      states <- gets (^? appModal . _Just . reviewViewedStates)
      let statesNow = fromMaybe _reviewViewedStates states
      let candidates = [(_reviewCurrentFile + offset) `mod` fileCount | fileCount > 0, offset <- [1 .. fileCount]]
      case find (\ix' -> maybe False (\f -> fileViewedState statesNow f /= FileViewed) (_reviewFiles V.!? ix')) candidates of
        Just ix' -> moveToFile ix'
        Nothing -> showToast ToastSuccess "All files viewed"

    bc = s ^. appBaseContext
handlePRReviewModalKey _ _ _ = return False
