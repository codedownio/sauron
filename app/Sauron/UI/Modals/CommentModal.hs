module Sauron.UI.Modals.CommentModal (
  renderModal,
  ) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Edit (Editor, getEditContents, renderEditor)
import Data.String.Interpolate
import qualified Data.Text as T
import GitHub
import Lens.Micro
import Relude
import Sauron.Types
import Sauron.UI.AttrMap
import Sauron.UI.Issue (issueInner, maxCommentWidth)
import Sauron.UI.Markdown (markdownToWidgetsWithWidth)


renderModal :: AppState -> ModalState Fixed -> Widget ClickableName
renderModal appState (CommentModalState {_commentIssue=issue@(Issue {issueNumber=(IssueNumber num)}), ..}) =
  vBox [
    hCenter $ withAttr boldText $ str modalTitle

    , hBorder

    -- Scrollable content area with issue and comments
    , padBottom Max $ withVScrollBars OnRight $ withVScrollBarHandles $ viewport CommentModalContent Vertical $
      hLimit maxCommentWidth $ vBox [
        issueInner (appState ^. appNow) issue _commentIssueComments
        , hBorder
        , str " "
        , renderCommentEditor _commentEditor
      ]
    , hBorder
    , buttonSection _commentEditor issue _submissionState
  ]
  & border
  & withAttr normalAttr
  & hLimit (maxCommentWidth + 4)
  & vLimitPercent 90
  & centerLayer
  where
    typ :: Text
    typ = if _issueIsPR then "Pull Request" else "Issue"

    modalTitle = [i|Comment on #{typ} \##{num}|]
renderModal _ _ = str "Invalid modal state for CommentModal" -- This should never happen

renderCommentEditor :: Editor Text ClickableName -> Widget ClickableName
renderCommentEditor editor =
  vLimit 12 $ vBox [
    hBox [
      -- Left: Editor
      vBox [
        withAttr italicText $ str "Write your comment:"
        , padAll 1 $
            vLimit 8 $
            hLimit 60 $
            withAttr normalAttr $
            renderEditor (str . toString . T.unlines) True editor
      ]
      , vBorder
      -- Right: Preview
      , vBox [
        withAttr italicText $ str "Preview:"
        , padAll 1 $
            hLimit 50 $
            vLimit 8 $
            if T.null text
              then withAttr italicText $ str "(preview will appear here)"
              else markdownToWidgetsWithWidth 48 text
      ]
    ]
  ]
  where
    text = T.unlines $ getEditContents editor

buttonSection :: Editor Text ClickableName -> Issue -> SubmissionState -> Widget ClickableName
buttonSection editor _issue submissionState' =
  padLeft Max $ hBox [
      border $ hBox [
        str " "
        , withAttr hotkeyMessageAttr $ str buttonText
        , str " ["
        , withAttr hotkeyAttr $ str "Alt+Shift+Enter"
        , str "] "
        , if submissionState' == SubmittingCloseWithComment then str " [submitting...]" else str ""
        ]
      , str "  "
      , border $ hBox [
          str " "
          , withAttr (if hasText && submissionState' == NotSubmitting then hotkeyMessageAttr else disabledHotkeyMessageAttr) $ str "Comment"
          , str " ["
          , withAttr (if hasText && submissionState' == NotSubmitting then hotkeyAttr else disabledHotkeyAttr) $ str "Alt+Enter"
          , str "] "
          , if submissionState' == SubmittingComment then str " [submitting...]" else str ""
          ]
    ]
  where
    text = T.unlines $ getEditContents editor
    hasText = not $ T.null $ T.strip text
    buttonText = if hasText then "Close with comment" else "Close issue"
