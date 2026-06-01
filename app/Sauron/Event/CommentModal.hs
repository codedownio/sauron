{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Sauron.Event.CommentModal (
  handleCommentModalEvent,
  submitComment,
  closeWithComment,
  refreshIssueComments,
  fetchCommentsAndOpenModal,
  fetchIssueCommentsAndEvents,
  openCommentForNotification
) where

import Brick as B
import Brick.BChan
import Control.Monad.IO.Unlift
import Data.Char (isDigit)
import qualified Data.Text as T
import Data.Time
import qualified Data.Vector as V
import GitHub
import Lens.Micro
import Network.URI (parseURI, uriPath)
import Relude
import Sauron.Actions.Util (withGithubApiSemaphore, githubWithLogging)
import Sauron.Fetch.Issue (fetchIssueCommentsAndEvents)
import qualified Sauron.Mutations.Issue as Issue
import Sauron.Types
import UnliftIO.Async
import WEditor.LineWrap (breakWords, noHyphen)
import WEditorBrick.WrappingEditor (dumpEditor, newEditor)


handleCommentModalEvent :: AppState -> CommentModalEvent -> EventM ClickableName AppState ()
handleCommentModalEvent s (CommentSubmitted result) = case result of
  Right _comment -> do
    -- Reset submission state and clear editor
    modify (appModal . _Just . submissionState .~ NotSubmitting)
    modify (appModal . _Just . commentEditor .~ newEditor (breakWords noHyphen) CommentEditor [])
    -- Refresh issue comments and scroll to bottom
    case s ^. appModal of
      Just (CommentModalState _editor issue _comments _nodeState isPR owner name _submissionState) -> do
        let issueNum = case issueNumber issue of IssueNumber n -> n
        void $ refreshIssueComments (s ^. appBaseContext) owner name issueNum isPR
        vScrollToEnd (viewportScroll CommentModalContent)
      _ -> return ()
  Left _err -> do
    -- Reset submission state on error
    modify (appModal . _Just . submissionState .~ NotSubmitting)

handleCommentModalEvent _s (IssueClosedWithComment result) = case result of
  Right _issue -> do
    -- Close modal and refresh issue
    modify (appModal .~ Nothing)
    -- TODO: refresh the issue in the main list
  Left _err -> do
    -- Reset submission state on error
    modify (appModal . _Just . submissionState .~ NotSubmitting)

handleCommentModalEvent s (CommentsRefreshed comments) = do
  -- Update the modal with refreshed comments and scroll to bottom
  modify (appModal . _Just . commentIssueComments .~ comments)
  -- Also update the node's state so the main UI stays in sync
  case s ^. appModal of
    Just (CommentModalState {_commentNodeState=nodeState}) ->
      liftIO $ atomically $ writeTVar nodeState (Fetched comments)
    _ -> return ()
  invalidateCacheEntry CommentModalContent
  vScrollToEnd (viewportScroll CommentModalContent)

handleCommentModalEvent _s (OpenCommentModal issue comments nodeState isPR owner name) = do
  -- Update the node's state with the fresh comments
  liftIO $ atomically $ writeTVar nodeState (Fetched comments)
  -- Open the comment modal with fresh comments and scroll to bottom
  let editor = newEditor (breakWords noHyphen) CommentEditor []
  modify (appModal ?~ CommentModalState editor issue comments nodeState isPR owner name NotSubmitting)
  vScrollToEnd (viewportScroll CommentModalContent)

submitComment :: AppState -> ModalState Fixed -> IO ()
submitComment s (CommentModalState editor issue _comments _nodeState _isPR owner name _submissionState) = do
  let commentText = T.intercalate "\n" $ map toText $ dumpEditor editor
  unless (T.null $ T.strip commentText) $ do
    let baseContext = s ^. appBaseContext
    let issueNum = case issueNumber issue of IssueNumber n -> n
    void $ async $ do
      result <- Issue.submitComment baseContext owner name (IssueNumber issueNum) commentText
      now <- getCurrentTime
      writeBChan (eventChan baseContext) (CommentModalEvent (CommentSubmitted result))
      writeBChan (eventChan baseContext) (TimeUpdated now)
submitComment _ _ = return () -- ZoomModalState doesn't support comments

closeWithComment :: AppState -> ModalState Fixed -> IO ()
closeWithComment s (CommentModalState editor issue _comments _nodeState _isPR owner name _submissionState) = do
  let commentText = T.intercalate "\n" $ map toText $ dumpEditor editor
  let baseContext = s ^. appBaseContext
  let issueNum = case issueNumber issue of IssueNumber n -> n
  void $ async $ do
    result <- Issue.closeIssueWithComment baseContext owner name (IssueNumber issueNum) commentText
    now <- getCurrentTime
    writeBChan (eventChan baseContext) (CommentModalEvent (IssueClosedWithComment result))
    writeBChan (eventChan baseContext) (TimeUpdated now)
closeWithComment _ _ = return () -- ZoomModalState doesn't support comments


refreshIssueComments :: BaseContext -> Name Owner -> Name Repo -> Int -> Bool -> EventM ClickableName AppState (Async ())
refreshIssueComments baseContext owner name issueNumber _isPR = do
  liftIO $ async $ do
    fetchIssueCommentsAndEvents baseContext owner name issueNumber >>= \case
      Right merged -> do
        -- Send an event to update the modal with new comments and events
        now <- getCurrentTime
        writeBChan (eventChan baseContext) (CommentModalEvent (CommentsRefreshed merged))
        writeBChan (eventChan baseContext) (TimeUpdated now)
      Left _err -> return ()

fetchCommentsAndOpenModal :: BaseContext -> Issue -> TVar (Fetchable (V.Vector TimelineEvent)) -> Bool -> Name Owner -> Name Repo -> EventM ClickableName AppState ()
fetchCommentsAndOpenModal baseContext issue@(Issue {issueNumber=(IssueNumber issueNum)}) nodeState isPR owner name = do
  liftIO $ void $ async $ do
    fetchIssueCommentsAndEvents baseContext owner name issueNum >>= \case
      Right merged -> do
        -- Send event to open modal with fresh comments and events
        now <- getCurrentTime
        writeBChan (eventChan baseContext) (CommentModalEvent (OpenCommentModal issue merged nodeState isPR owner name))
        writeBChan (eventChan baseContext) (TimeUpdated now)
      Left _err -> do
        -- On error, open modal with empty comments and events
        writeBChan (eventChan baseContext) (CommentModalEvent (OpenCommentModal issue V.empty nodeState isPR owner name))

-- | Open comment modal for a notification that contains an issue or PR.
-- Uses the fetched content if available, otherwise fetches the issue from scratch.
openCommentForNotification :: BaseContext -> Notification -> NotificationState -> EventM ClickableName AppState ()
openCommentForNotification baseContext notification notifState =
  case notificationStateContent notifState of
    Fetched (NotificationIssue issue comments) -> do
      commentsVar <- liftIO $ newTVarIO (Fetched comments)
      fetchCommentsAndOpenModal baseContext issue commentsVar False owner name
    Fetched (NotificationPull issue comments) -> do
      commentsVar <- liftIO $ newTVarIO (Fetched comments)
      fetchCommentsAndOpenModal baseContext issue commentsVar True owner name
    _ | subjectType subject `elem` ["Issue", "PullRequest"] ->
      whenJust (subjectURL subject >>= extractIssueNumber) $ \issueNum ->
        liftIO $ fetchIssueAndOpenCommentModal baseContext owner name issueNum (subjectType subject == "PullRequest")
    _ -> return ()
  where
    subject = notificationSubject notification
    RepoRef {repoRefOwner=(SimpleOwner {simpleOwnerLogin=owner}), repoRefRepo=name} = notificationRepo notification

fetchIssueAndOpenCommentModal :: BaseContext -> Name Owner -> Name Repo -> Int -> Bool -> IO ()
fetchIssueAndOpenCommentModal baseContext owner name issueNum isPR =
  void $ async $ flip runReaderT baseContext $
    withGithubApiSemaphore (githubWithLogging (issueR owner name (IssueNumber issueNum))) >>= \case
      Left _err -> return ()
      Right issue -> liftIO $ do
        commentsResult <- fetchIssueCommentsAndEvents baseContext owner name issueNum
        commentsVar <- newTVarIO (Fetched (either (const V.empty) id commentsResult))
        now <- getCurrentTime
        writeBChan (eventChan baseContext) (CommentModalEvent (OpenCommentModal issue (either (const V.empty) id commentsResult) commentsVar isPR owner name))
        writeBChan (eventChan baseContext) (TimeUpdated now)

extractIssueNumber :: URL -> Maybe Int
extractIssueNumber (URL url) = do
  uri <- parseURI (toString url)
  let segments = filter (not . T.null) $ T.splitOn "/" $ toText (uriPath uri)
  case reverse (toList segments) of
    (idStr:_) | T.all isDigit idStr -> readMaybe (toString idStr)
    _ -> Nothing
