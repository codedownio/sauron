{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

-- | The comment editor lives inline at the bottom of the zoom modal ("comment mode")
-- rather than in a modal of its own, so switching into it doesn't swap modals.
module Sauron.Event.CommentModal (
  handleCommentModalEvent,
  handleCommentModeEvent,
  enterCommentMode,
  openZoomAndComment,
  exitCommentMode,
  submitComment,
  closeWithComment,
  fetchIssueCommentsAndEvents,
  openCommentForNotification
) where

import Brick as B
import Brick.BChan
import Control.Monad.IO.Unlift
import Data.Char (isDigit)
import qualified Data.Text as T
import Data.Time
import qualified Graphics.Vty as Vty
import GitHub
import Lens.Micro
import Network.URI (parseURI, uriPath)
import Relude
import Sauron.Actions (refreshOnZoom, refreshSelected)
import Sauron.Actions.Util (withGithubApiSemaphore, githubWithLogging)
import Sauron.Event.Helpers (modifyModalCommentMode, withFixedElemAndParents)
import Sauron.Fetch.Issue (fetchIssueCommentsAndEvents)
import qualified Sauron.Mutations.Issue as Issue
import Sauron.Types
import UnliftIO.Async
import WEditor.LineWrap (breakWords, noHyphen)
import WEditorBrick.WrappingEditor (dumpEditor, newEditor)
import qualified WEditorBrick.WrappingEditor as WEditorBrick


-- | Turn on the zoom modal's comment editor for the given issue/PR
enterCommentMode :: AppState -> Issue -> Bool -> Name Owner -> Name Repo -> EventM ClickableName AppState ()
enterCommentMode s issue isPR owner name = do
  setCommentMode s $ Just $ CommentMode {
    _commentModeEditor = newEditor (breakWords noHyphen) CommentEditor []
    , _commentModeIssue = issue
    , _commentModeIsPR = isPR
    , _commentModeOwner = owner
    , _commentModeName = name
    , _commentModeSubmission = NotSubmitting
    }
  vScrollToEnd (viewportScroll ZoomModalContent)

exitCommentMode :: AppState -> EventM ClickableName AppState ()
exitCommentMode s = setCommentMode s Nothing

-- | The comment editor is part of the modal's own state
setCommentMode :: AppState -> Maybe CommentMode -> EventM ClickableName AppState ()
setCommentMode s commentMode = modifyModalCommentMode s (const commentMode)

modifyCommentMode :: AppState -> (CommentMode -> CommentMode) -> EventM ClickableName AppState ()
modifyCommentMode s f = modifyModalCommentMode s (fmap f)

-- | Zoom in on the selected node and turn on comment mode, so commenting from the
-- main list lands in the same view as commenting from the zoom modal.
openZoomAndComment :: AppState -> Issue -> Bool -> Name Owner -> Name Repo -> EventM ClickableName AppState ()
openZoomAndComment s issue isPR owner name = do
  withFixedElemAndParents s $ \_ (SomeNode variableEl) parents -> do
    refreshOnZoom (s ^. appBaseContext) variableEl parents
    liftIO $ atomically $ writeTVar (_appModalVariable s) (Just (newZoomModalState (SomeNode variableEl) (toList parents)))
  enterCommentMode s issue isPR owner name

handleCommentModalEvent :: AppState -> CommentModalEvent -> EventM ClickableName AppState ()
handleCommentModalEvent s (CommentSubmitted result) = case result of
  Right _comment -> do
    -- Clear the editor and leave comment mode, then refresh the zoomed node so the
    -- new comment shows up in the timeline
    exitCommentMode s
    refreshZoomedNode s
    vScrollToEnd (viewportScroll ZoomModalContent)
  Left _err -> modifyCommentMode s (\cm -> cm { _commentModeSubmission = NotSubmitting })

handleCommentModalEvent s (IssueClosedWithComment result) = case result of
  Right _issue -> do
    exitCommentMode s
    refreshZoomedNode s
  Left _err -> modifyCommentMode s (\cm -> cm { _commentModeSubmission = NotSubmitting })

handleCommentModalEvent s (EnterCommentMode issue isPR owner name) = enterCommentMode s issue isPR owner name

-- | Re-fetch the node the zoom modal is showing, so mutations become visible
refreshZoomedNode :: AppState -> EventM ClickableName AppState ()
refreshZoomedNode s = do
  maybeVarModal <- liftIO $ readTVarIO (_appModalVariable s)
  case maybeVarModal of
    Just (ZoomModalState {_zoomModalSomeNode=SomeNode node, _zoomModalParents=parents}) ->
      whenJust (nonEmpty parents) $ \parents' ->
        void $ refreshSelected (s ^. appBaseContext) node parents'
    Just (PullRequestModalState {_pullModalNode=node, _pullModalParents=parents}) ->
      whenJust (nonEmpty parents) $ \parents' ->
        void $ refreshSelected (s ^. appBaseContext) node parents'
    _ -> return ()

-- | Handle a key while comment mode is on. Returns True if the key was consumed
-- (everything except the modal-closing keys handled by the caller).
handleCommentModeEvent :: AppState -> CommentMode -> Vty.Event -> EventM ClickableName AppState Bool
handleCommentModeEvent s commentMode ev
  | _commentModeSubmission commentMode /= NotSubmitting = return True
  | otherwise = case ev of
      -- Match Esc with any modifiers: some terminals tag it, and a modifier-tagged
      -- Esc falling through to the editor would type stray characters
      Vty.EvKey Vty.KEsc _ -> True <$ exitCommentMode s
      Vty.EvKey Vty.KEnter [Vty.MMeta] -> do
        modifyCommentMode s (\cm -> cm { _commentModeSubmission = SubmittingComment })
        True <$ liftIO (submitComment s commentMode)
      Vty.EvKey Vty.KEnter [Vty.MMeta, Vty.MShift] -> do
        modifyCommentMode s (\cm -> cm { _commentModeSubmission = SubmittingCloseWithComment })
        True <$ liftIO (closeWithComment s commentMode)
      -- Forward only recognizable editing input to the editor; anything else (Meta
      -- chords, stray escape-sequence fragments, mouse events) is dropped so it
      -- can't type garbage into the box.
      _ | isEditorInput ev -> do
            ed' <- WEditorBrick.handleEditor (_commentModeEditor commentMode) ev
            modifyCommentMode s (\cm -> cm { _commentModeEditor = ed' })
            return True
      _ -> return True
  where
    isEditorInput (Vty.EvPaste _) = True
    isEditorInput (Vty.EvKey (Vty.KChar _) mods) = mods `elem` [[], [Vty.MShift], [Vty.MCtrl]]
    isEditorInput (Vty.EvKey k []) = k `elem` [
      Vty.KBS, Vty.KDel, Vty.KEnter, Vty.KLeft, Vty.KRight, Vty.KUp, Vty.KDown
      , Vty.KHome, Vty.KEnd, Vty.KPageUp, Vty.KPageDown
      ]
    isEditorInput _ = False

submitComment :: AppState -> CommentMode -> IO ()
submitComment s (CommentMode {..}) = do
  let commentText = T.intercalate "\n" $ map toText $ dumpEditor _commentModeEditor
  unless (T.null $ T.strip commentText) $ do
    let baseContext = s ^. appBaseContext
    void $ async $ do
      result <- Issue.submitComment baseContext _commentModeOwner _commentModeName (issueNumber _commentModeIssue) commentText
      now <- getCurrentTime
      writeBChan (eventChan baseContext) (CommentModalEvent (CommentSubmitted result))
      writeBChan (eventChan baseContext) (TimeUpdated now)

closeWithComment :: AppState -> CommentMode -> IO ()
closeWithComment s (CommentMode {..}) = do
  let commentText = T.intercalate "\n" $ map toText $ dumpEditor _commentModeEditor
  let baseContext = s ^. appBaseContext
  void $ async $ do
    result <- Issue.closeIssueWithComment baseContext _commentModeOwner _commentModeName (issueNumber _commentModeIssue) commentText
    now <- getCurrentTime
    writeBChan (eventChan baseContext) (CommentModalEvent (IssueClosedWithComment result))
    writeBChan (eventChan baseContext) (TimeUpdated now)

-- | Turn on comment mode for a notification that contains an issue or PR.
-- Uses the fetched content if available, otherwise fetches the issue from scratch.
openCommentForNotification :: AppState -> Notification -> NotificationState -> EventM ClickableName AppState ()
openCommentForNotification s notification notifState =
  case notificationStateContent notifState of
    Fetched (NotificationIssue issue _comments) -> enterCommentMode s issue False owner name
    Fetched (NotificationPull issue _comments) -> enterCommentMode s issue True owner name
    _ | subjectType subject `elem` ["Issue", "PullRequest"] ->
      whenJust (subjectURL subject >>= extractIssueNumber) $ \issueNum ->
        liftIO $ fetchIssueAndEnterCommentMode (s ^. appBaseContext) owner name issueNum (subjectType subject == "PullRequest")
    _ -> return ()
  where
    subject = notificationSubject notification
    RepoRef {repoRefOwner=(SimpleOwner {simpleOwnerLogin=owner}), repoRefRepo=name} = notificationRepo notification

fetchIssueAndEnterCommentMode :: BaseContext -> Name Owner -> Name Repo -> Int -> Bool -> IO ()
fetchIssueAndEnterCommentMode baseContext owner name issueNum isPR =
  void $ async $ flip runReaderT baseContext $
    withGithubApiSemaphore (githubWithLogging (issueR owner name (IssueNumber issueNum))) >>= \case
      Left _err -> return ()
      Right issue -> liftIO $
        writeBChan (eventChan baseContext) (CommentModalEvent (EnterCommentMode issue isPR owner name))

extractIssueNumber :: URL -> Maybe Int
extractIssueNumber (URL url) = do
  uri <- parseURI (toString url)
  let segments = filter (not . T.null) $ T.splitOn "/" $ toText (uriPath uri)
  case reverse (toList segments) of
    (idStr:_) | T.all isDigit idStr -> readMaybe (toString idStr)
    _ -> Nothing
