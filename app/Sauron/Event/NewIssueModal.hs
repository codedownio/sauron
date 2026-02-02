module Sauron.Event.NewIssueModal (
  handleNewIssueModalEvent,
  submitNewIssue,
  openNewIssueModal
) where

import Brick as B
import Brick.BChan
import Brick.Widgets.Edit (editorText, getEditContents)
import Control.Monad
import qualified Data.Text as T
import GitHub
import Lens.Micro
import Relude
import Sauron.Actions (refreshSelected)
import Sauron.Event.Helpers (withFixedElemAndParents)
import qualified Sauron.Mutations.Issue as Issue
import Sauron.Types
import UnliftIO.Async
import WEditor.LineWrap (breakWords, noHyphen)
import WEditorBrick.WrappingEditor (dumpEditor, newEditor)


handleNewIssueModalEvent :: AppState -> NewIssueModalEvent -> EventM ClickableName AppState ()
handleNewIssueModalEvent s (NewIssueCreated result) = case result of
  Right _issue -> do
    -- Close modal
    modify (appModal .~ Nothing)
    liftIO $ atomically $ writeTVar (_appModalVariable s) Nothing
    -- Refresh the selected paginated issues node to pick up the new issue
    withFixedElemAndParents s $ \_fixedEl (SomeNode el) parents ->
      void $ refreshSelected (s ^. appBaseContext) el parents
  Left _err -> do
    -- Reset submission state on error
    modify (appModal . _Just . newIssueSubmissionState .~ NotSubmitting)

submitNewIssue :: AppState -> ModalState Fixed -> IO ()
submitNewIssue s (NewIssueModalState {..}) = do
  let titleText = T.strip $ T.unlines $ getEditContents _newIssueTitleEditor
  unless (T.null titleText) $ do
    let bodyText = T.unlines $ map toText $ dumpEditor _newIssueBodyEditor
    let baseContext = s ^. appBaseContext
    void $ async $ do
      result <- Issue.createNewIssue baseContext _newIssueRepoOwner _newIssueRepoName titleText bodyText
      writeBChan (eventChan baseContext) (NewIssueModalEvent (NewIssueCreated result))
submitNewIssue _ _ = return ()

openNewIssueModal :: Name Owner -> Name Repo -> EventM ClickableName AppState ()
openNewIssueModal owner name =
  modify (appModal ?~ NewIssueModalState titleEditor bodyEditor owner name NotSubmitting True)
   where
     titleEditor = editorText NewIssueTitleEditor (Just 1) ""
     bodyEditor = newEditor (breakWords noHyphen) NewIssueBodyEditor []
