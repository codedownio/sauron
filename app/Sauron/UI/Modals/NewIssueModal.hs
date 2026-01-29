module Sauron.UI.Modals.NewIssueModal (
  renderNewIssueModal,
  ) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Edit (Editor, getEditContents, renderEditor)
import Data.String.Interpolate
import qualified Data.Text as T
import GitHub.Data.Name
import Relude
import Sauron.Types
import Sauron.UI.AttrMap
import Sauron.UI.Issue (maxCommentWidth)
import Sauron.UI.Markdown (markdownToWidgetsWithWidth)


renderNewIssueModal :: ModalState Fixed -> Widget ClickableName
renderNewIssueModal (NewIssueModalState {..}) =
  vBox [
    hCenter $ withAttr boldText $ str [i|New Issue on #{untagName _newIssueRepoOwner}/#{untagName _newIssueRepoName}|]

    , hBorder

    , hLimit maxCommentWidth $ vBox [
        -- Title
        withAttr (if _newIssueFocusTitle then boldText else italicText) $ str "Title:"
        , padAll 1 $
            vLimit 1 $
            hLimit (maxCommentWidth - 4) $
            withAttr normalAttr $
            renderEditor (str . toString . T.unlines) _newIssueFocusTitle _newIssueTitleEditor
        , str " "

        -- Body editor + preview
        , renderBodyEditor (not _newIssueFocusTitle) editorLines _newIssueBodyEditor
      ]
    , hBorder
    , newIssueButtonSection _newIssueTitleEditor _newIssueSubmissionState
  ]
  & border
  & withAttr normalAttr
  & hLimit (maxCommentWidth + 4)
  & vLimitPercent 80
  & centerLayer
  where
    bodyLineCount = length (getEditContents _newIssueBodyEditor)
    editorLines = max 10 (min bodyLineCount 30)
renderNewIssueModal _ = str "Invalid modal state for NewIssueModal"

renderBodyEditor :: Bool -> Int -> Editor Text ClickableName -> Widget ClickableName
renderBodyEditor focused editorHeight editor =
  hBox [
    -- Left: Editor
    vBox [
      withAttr (if focused then boldText else italicText) $ str "Body:"
      , padAll 1 $
          vLimit editorHeight $
          hLimit 60 $
          withAttr normalAttr $
          renderEditor (str . toString . T.unlines) focused editor
    ]
    , vBorder
    -- Right: Preview
    , vBox [
      withAttr italicText $ str "Preview:"
      , padAll 1 $
          hLimit 50 $
          vLimit editorHeight $
          if T.null text
            then withAttr italicText $ str "(preview will appear here)"
            else markdownToWidgetsWithWidth 48 text
    ]
  ]
  where
    text = T.unlines $ getEditContents editor

newIssueButtonSection :: Editor Text ClickableName -> SubmissionState -> Widget ClickableName
newIssueButtonSection titleEditor submissionState' =
  padLeft Max $ hBox [
    border $ hBox [
      str " "
      , withAttr (if hasTitle && submissionState' == NotSubmitting then hotkeyMessageAttr else disabledHotkeyMessageAttr) $ str "Create issue"
      , str " ["
      , withAttr (if hasTitle && submissionState' == NotSubmitting then hotkeyAttr else disabledHotkeyAttr) $ str "Alt+Enter"
      , str "] "
      , if submissionState' == SubmittingNewIssue then str " [submitting...]" else str ""
      ]
  ]
  where
    titleText = T.unlines $ getEditContents titleEditor
    hasTitle = not $ T.null $ T.strip titleText
