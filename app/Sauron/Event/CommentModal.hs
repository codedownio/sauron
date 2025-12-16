{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Sauron.Event.CommentModal (
  handleCommentModalEvent,
  submitComment,
  closeWithComment,
  refreshIssueComments,
  fetchCommentsAndOpenModal,
  fetchIssueCommentsAndEvents
) where

import Brick as B
import Brick.BChan
import Brick.Widgets.Edit (editorText, getEditContents)
import Control.Monad.IO.Unlift
import qualified Data.Text as T
import Data.Time
import qualified Data.Vector as V
import GitHub
import Lens.Micro
import Relude
import Sauron.Actions.Util
import Sauron.Fetch (fetchIssueCommentsAndEvents)
import Sauron.Types
import UnliftIO.Async


handleCommentModalEvent :: AppState -> CommentModalEvent -> EventM ClickableName AppState ()
handleCommentModalEvent s (CommentSubmitted result) = case result of
  Right _comment -> do
    -- Reset submission state and clear editor
    modify (appModal . _Just . submissionState .~ NotSubmitting)
    modify (appModal . _Just . commentEditor .~ editorText CommentEditor Nothing "")
    -- Refresh issue comments and scroll to bottom
    case s ^. appModal of
      Just (CommentModalState _editor issue _comments isPR owner name _submissionState) -> do
        let issueNum = case issueNumber issue of IssueNumber n -> n
        refreshIssueComments (s ^. appBaseContext) owner name issueNum isPR
        vScrollToEnd (viewportScroll CommentModalContent)
      Nothing -> return ()
      Just (ZoomModalState _) -> return () -- ZoomModal doesn't support comment operations
      Just (LogModalState _) -> return () -- LogModal doesn't support comment operations
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

handleCommentModalEvent _s (CommentsRefreshed comments) = do
  -- Update the modal with refreshed comments and scroll to bottom
  modify (appModal . _Just . commentIssueComments .~ comments)
  vScrollToEnd (viewportScroll CommentModalContent)

handleCommentModalEvent _s (OpenCommentModal issue comments isPR owner name) = do
  -- Open the comment modal with fresh comments and scroll to bottom
  let editor = editorText CommentEditor Nothing ""
  modify (appModal ?~ CommentModalState editor issue comments isPR owner name NotSubmitting)
  vScrollToEnd (viewportScroll CommentModalContent)

submitComment :: AppState -> ModalState Fixed -> IO ()
submitComment s (CommentModalState editor issue _comments _isPR owner name _submissionState) = do
  let commentText = T.unlines $ getEditContents editor
  unless (T.null $ T.strip commentText) $ do
    let baseContext = s ^. appBaseContext
    let issueNum = case issueNumber issue of IssueNumber n -> n
    void $ async $ do
      result <- submitCommentAsync baseContext owner name issueNum commentText
      now <- getCurrentTime
      writeBChan (eventChan baseContext) (CommentModalEvent (CommentSubmitted result))
      writeBChan (eventChan baseContext) (TimeUpdated now)
submitComment _ _ = return () -- ZoomModalState doesn't support comments

closeWithComment :: AppState -> ModalState Fixed -> IO ()
closeWithComment s (CommentModalState editor issue _comments isPR owner name _submissionState) = do
  let commentText = T.unlines $ getEditContents editor
  let baseContext = s ^. appBaseContext
  let issueNum = case issueNumber issue of IssueNumber n -> n
  void $ async $ do
    result <- closeWithCommentAsync baseContext owner name issueNum isPR commentText
    now <- getCurrentTime
    writeBChan (eventChan baseContext) (CommentModalEvent (IssueClosedWithComment result))
    writeBChan (eventChan baseContext) (TimeUpdated now)
closeWithComment _ _ = return () -- ZoomModalState doesn't support comments

submitCommentAsync :: BaseContext -> Name Owner -> Name Repo -> Int -> Text -> IO (Either Error Comment)
submitCommentAsync baseContext@(BaseContext {requestSemaphore}) owner name issueNumber commentBody = do
  withGithubApiSemaphore' requestSemaphore $ githubWithLogging' baseContext (createCommentR owner name (IssueNumber issueNumber) commentBody)

closeWithCommentAsync :: BaseContext -> Name Owner -> Name Repo -> Int -> Bool -> Text -> IO (Either Error Issue)
closeWithCommentAsync baseContext@(BaseContext {requestSemaphore}) owner name issueNumber _isPR commentBody = do
  -- First submit comment if there is one
  unless (T.null $ T.strip commentBody) $ do
    void $ withGithubApiSemaphore' requestSemaphore (githubWithLogging' baseContext (createCommentR owner name (IssueNumber issueNumber) commentBody))

  -- Then close the issue/PR
  let editIssue = EditIssue Nothing Nothing Nothing (Just StateClosed) Nothing Nothing
  withGithubApiSemaphore' requestSemaphore (githubWithLogging' baseContext (editIssueR owner name (IssueNumber issueNumber) editIssue))

refreshIssueComments :: BaseContext -> Name Owner -> Name Repo -> Int -> Bool -> EventM ClickableName AppState ()
refreshIssueComments baseContext owner name issueNumber _isPR = do
  liftIO $ void $ async $ do
    fetchIssueCommentsAndEvents baseContext owner name issueNumber >>= \case
      Right merged -> do
        -- Send an event to update the modal with new comments and events
        now <- getCurrentTime
        writeBChan (eventChan baseContext) (CommentModalEvent (CommentsRefreshed merged))
        writeBChan (eventChan baseContext) (TimeUpdated now)
      Left _err -> return ()

fetchCommentsAndOpenModal :: BaseContext -> Issue -> Bool -> Name Owner -> Name Repo -> EventM ClickableName AppState ()
fetchCommentsAndOpenModal baseContext issue@(Issue {issueNumber=(IssueNumber issueNum)}) isPR owner name = do
  liftIO $ void $ async $ do
    fetchIssueCommentsAndEvents baseContext owner name issueNum >>= \case
      Right merged -> do
        -- Send event to open modal with fresh comments and events
        now <- getCurrentTime
        writeBChan (eventChan baseContext) (CommentModalEvent (OpenCommentModal issue merged isPR owner name))
        writeBChan (eventChan baseContext) (TimeUpdated now)
      Left _err -> do
        -- On error, open modal with empty comments and events
        writeBChan (eventChan baseContext) (CommentModalEvent (OpenCommentModal issue V.empty isPR owner name))
