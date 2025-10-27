module Sauron.UI.CommentModal (
  renderModal,
  ) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Edit (Editor, getEditContents, renderEditor)
import qualified Data.Text as T
import qualified Data.Vector as V
import GitHub
import Lens.Micro
import Relude
import Sauron.Types
import Sauron.UI.AttrMap
import Sauron.UI.Issue (issueInner, maxCommentWidth)
import Sauron.UI.Markdown (markdownToWidgetsWithWidth)

renderModal :: AppState -> ModalState -> Widget ClickableName
renderModal appState (CommentModalState editor issue comments isPR owner name submissionState') =
  renderCommentModal appState editor issue comments isPR owner name submissionState'

renderCommentModal :: AppState -> Editor Text ClickableName -> Issue -> V.Vector IssueComment -> Bool -> Name Owner -> Name Repo -> SubmissionState -> Widget ClickableName
renderCommentModal appState editor issue comments isPR _owner _name submissionState' =
  vBox [
    -- Scrollable content area with issue and comments
    padBottom Max $ withVScrollBars OnRight $ withVScrollBarHandles $ viewport CommentModalContent Vertical $
      hLimit maxCommentWidth $ vBox [
        issueInner (appState ^. appNow) issue comments
        , hBorder
        , str " "
        , renderCommentEditor editor isPR issue
      ]
    , hBorder
    , buttonSection editor issue submissionState'
  ]
  & border
  & withAttr normalAttr
  & hLimit 120
  & vLimitPercent 80
  & centerLayer

renderCommentEditor :: Editor Text ClickableName -> Bool -> Issue -> Widget ClickableName
renderCommentEditor editor isPR issue =
  vLimit 12 $ vBox [
    hCenter $ withAttr boldText $ str modalTitle
    , hBorder
    , hBox [
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
    modalTitle = if isPR
                 then "Comment on Pull Request #" <> show (issueNumber issue)
                 else "Comment on Issue #" <> show (issueNumber issue)
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
