module Sauron.UI.Modals.NewIssueModal (
  renderNewIssueModal
  , renderBodyEditor
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
import WEditorBrick.WrappingEditor (WrappingEditor, dumpEditor)
import qualified WEditorBrick.WrappingEditor as WEditorBrick


renderNewIssueModal :: AppState -> ModalState Fixed -> Widget ClickableName
renderNewIssueModal app (NewIssueModalState {..}) =
  vBox [
    hCenter $ withAttr boldText $ str [i|New Issue on #{untagName _newIssueRepoOwner}/#{untagName _newIssueRepoName}|]

    , hBorder

    , vBox [
        -- Title
        withAttr (if _newIssueFocusTitle then boldText else italicText) $ str "Title"
        , border $
            vLimit 1 $
            withAttr normalAttr $
            renderEditor (str . toString . T.intercalate "\n") _newIssueFocusTitle _newIssueTitleEditor
        , str " "

        -- Body editor + preview
        , renderBodyEditor app (not _newIssueFocusTitle) modalWidth editorLines _newIssueBodyEditor
      ]
    , hBorder
    , newIssueButtonSection _newIssueTitleEditor _newIssueSubmissionState
  ]
  & border
  & withAttr normalAttr
  & hLimit modalWidth
  & vLimitPercent 80
  & centerLayer
  where
    modalWidth = case _appMainUiExtent app of
      Nothing -> maxCommentWidth + 4
      Just (Extent {extentSize=(w, _h)}) -> round ((0.8 :: Double) * fromIntegral w)

    bodyLineCount = length (dumpEditor _newIssueBodyEditor)
    editorLines = max 10 (min bodyLineCount 30)
renderNewIssueModal _ _ = str "Invalid modal state for NewIssueModal"

renderBodyEditor :: AppState -> Bool -> Int -> Int -> WrappingEditor Char ClickableName -> Widget ClickableName
renderBodyEditor (AppState {}) focused modalWidth editorHeight editor =
  vLimit (editorHeight + 3) $ hBox [
    -- Left: Editor
    vBox [
      withAttr (if focused then boldText else italicText) $ str "Write"
      , padAll 1 $
          vLimit editorHeight $
          hLimit sectionWidth $
          withAttr normalAttr $
          WEditorBrick.renderEditor focused editor
    ]
    , vBorder
    -- Right: Preview
    , vBox [
      withAttr (if focused then boldText else italicText) $ str "Preview"
      , border $ padRight Max $ padBottom Max $
          vLimit editorHeight $
          case text of
            "" -> withAttr italicText $ strWrap [i|(preview will appear here)|]
            t -> markdownToWidgetsWithWidth (sectionWidth - 4) t
    ]
  ]
  where
    sectionWidth = (modalWidth - 4) `div` 2

    text = T.intercalate "\n" $ map toText $ dumpEditor editor

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
