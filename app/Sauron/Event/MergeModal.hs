{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Sauron.Event.MergeModal (
  handleMergeModalEvent
  , handleMergeModalVtyEvent
  , openMergeModal
  ) where

import Brick as B
import Brick.BChan
import Brick.Widgets.Edit (Editor, editorText, getEditContents, handleEditorEvent)
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Graphics.Vty as V
import GitHub
import Lens.Micro
import Relude
import Sauron.Actions (refreshLine, refreshSelected)
import Sauron.Actions.Util (findPullsParent)
import Sauron.Event.Helpers (withFixedElemAndParents)
import qualified Sauron.Mutations.Pull as Pull
import Sauron.Types
import Sauron.UI.Toast (showToast)
import UnliftIO.Async


openMergeModal :: Issue -> Name Owner -> Name Repo -> EventM ClickableName AppState ()
openMergeModal issue owner name =
  modify (appModal ?~ MergeModalState issue owner name MergeMethodMerge
    (titleEditorFor issue MergeMethodMerge) emptyMessageEditor MergeFocusMethods NotSubmitting)

-- | The commit title the editor starts with, matching the web UI's squash default.
-- For a merge commit it's empty, meaning GitHub's own default title.
defaultCommitTitle :: Issue -> MergeMethod -> Text
defaultCommitTitle (Issue {issueNumber=(IssueNumber number), issueTitle}) MergeMethodSquash = [i|#{issueTitle} (\##{number})|]
defaultCommitTitle _ _ = ""

titleEditorFor :: Issue -> MergeMethod -> Editor Text ClickableName
titleEditorFor issue method = editorText MergeCommitTitleEditor (Just 1) (defaultCommitTitle issue method)

emptyMessageEditor :: Editor Text ClickableName
emptyMessageEditor = editorText MergeCommitMessageEditor Nothing ""

handleMergeModalEvent :: AppState -> MergeModalEvent -> EventM ClickableName AppState ()
handleMergeModalEvent s (MergeFinished result) = do
  modify (appModal .~ Nothing)
  liftIO $ atomically $ writeTVar (_appModalVariable s) Nothing
  case result of
    Right msg -> do
      showToast ToastSuccess msg
      -- Refresh the containing list rather than the pull request itself, since the merge
      -- changes the pull request's state (and may drop it from a list of open ones).
      withFixedElemAndParents s $ \_fixedEl (SomeNode el) parents -> case findPullsParent parents of
        Just pullsNode -> void $ refreshLine (s ^. appBaseContext) pullsNode parents
        Nothing -> void $ refreshSelected (s ^. appBaseContext) el parents
    Left err -> showToast ToastError ("Merge failed: " <> err)

-- | Handle an event while the merge modal is up (Esc and Ctrl+q close it upstream).
-- Tab cycles focus between the method list and (for merge/squash) the commit title
-- and message editors. Alt+Enter merges from anywhere; plain Enter only inserts a
-- newline in the message editor.
handleMergeModalVtyEvent :: AppState -> ModalState Fixed -> V.Event -> EventM ClickableName AppState ()
handleMergeModalVtyEvent s modalState@(MergeModalState {_mergeIssue, _mergeMethod, _mergeFocus, _mergeSubmissionState}) ev
  | _mergeSubmissionState == SubmittingMerge = return ()
  | otherwise = case ev of
      V.EvKey V.KEnter [V.MMeta] -> submit
      V.EvKey (V.KChar '\t') [] -> setFocus (cycleFocus _mergeFocus)
      V.EvKey V.KBackTab [] -> setFocus (cycleFocusBack _mergeFocus)
      V.EvKey V.KUp [] | _mergeFocus == MergeFocusMethods -> moveMethod (-1)
      V.EvKey V.KDown [] | _mergeFocus == MergeFocusMethods -> moveMethod 1
      -- Forward only recognizable editing input to the editors; anything else
      -- (Meta chords, stray escape-sequence fragments, mouse events) is dropped so
      -- it can't type garbage into them.
      _ | _mergeFocus == MergeFocusTitle -> when (isEditorInput False ev) $
            zoom (appModal . _Just . mergeCommitTitleEditor) $ handleEditorEvent (VtyEvent ev)
      _ | _mergeFocus == MergeFocusBody -> when (isEditorInput True ev) $
            zoom (appModal . _Just . mergeCommitMessageEditor) $ handleEditorEvent (VtyEvent ev)
      V.EvKey (V.KChar 'p') [] -> moveMethod (-1)
      V.EvKey (V.KChar 'n') [] -> moveMethod 1
      V.EvKey (V.KChar '1') [] -> setMethod MergeMethodMerge
      V.EvKey (V.KChar '2') [] -> setMethod MergeMethodSquash
      V.EvKey (V.KChar '3') [] -> setMethod MergeMethodRebase
      _ -> return ()
  where
    submit = do
      modify (appModal . _Just . mergeSubmissionState .~ SubmittingMerge)
      liftIO $ submitMerge s modalState

    -- The editors don't exist for rebase (it creates no commit), so focus stays on
    -- the method list there
    cycleFocus MergeFocusMethods | _mergeMethod /= MergeMethodRebase = MergeFocusTitle
    cycleFocus MergeFocusTitle = MergeFocusBody
    cycleFocus _ = MergeFocusMethods

    cycleFocusBack MergeFocusMethods | _mergeMethod /= MergeMethodRebase = MergeFocusBody
    cycleFocusBack MergeFocusBody = MergeFocusTitle
    cycleFocusBack _ = MergeFocusMethods

    setFocus focus = modify (appModal . _Just . mergeFocus .~ focus)

    isEditorInput _ (V.EvPaste _) = True
    isEditorInput _ (V.EvKey (V.KChar _) mods) = mods `elem` [[], [V.MShift], [V.MCtrl]]
    isEditorInput multiline (V.EvKey k []) =
      k `elem` ([V.KBS, V.KDel, V.KLeft, V.KRight, V.KHome, V.KEnd]
                <> if multiline then [V.KEnter, V.KUp, V.KDown] else [])
    isEditorInput _ _ = False

    -- Selection doesn't wrap: matches how GitHub's list behaves, and avoids
    -- surprising jumps from the top of the list to the bottom.
    moveMethod delta =
      whenJust ([minBound .. maxBound] !!? (fromEnum _mergeMethod + delta)) setMethod

    -- Selecting a method re-prefills the commit title/message with that method's
    -- defaults, like the web UI does when switching merge methods.
    setMethod method = when (method /= _mergeMethod) $ do
      modify (appModal . _Just . mergeMethod .~ method)
      modify (appModal . _Just . mergeCommitTitleEditor .~ titleEditorFor _mergeIssue method)
      modify (appModal . _Just . mergeCommitMessageEditor .~ emptyMessageEditor)
      when (method == MergeMethodRebase) $ setFocus MergeFocusMethods
handleMergeModalVtyEvent _ _ _ = return ()

submitMerge :: AppState -> ModalState Fixed -> IO ()
submitMerge s (MergeModalState {_mergeIssue=(Issue {issueNumber}), ..}) =
  void $ async $ do
    result <- Pull.mergePull baseContext _mergeRepoOwner _mergeRepoName issueNumber _mergeMethod commitTitle commitMessage
    writeBChan (eventChan baseContext) (MergeModalEvent (MergeFinished result))
  where
    baseContext = s ^. appBaseContext

    editorContents editor = T.strip $ T.unlines $ getEditContents editor
    forCommitCreatingMethod text = if _mergeMethod /= MergeMethodRebase && not (T.null text) then Just text else Nothing
    commitTitle = forCommitCreatingMethod $ editorContents _mergeCommitTitleEditor
    commitMessage = forCommitCreatingMethod $ editorContents _mergeCommitMessageEditor
submitMerge _ _ = return ()
