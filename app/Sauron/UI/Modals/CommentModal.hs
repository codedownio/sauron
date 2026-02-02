module Sauron.UI.Modals.CommentModal (
  renderModal,
  ) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Data.String.Interpolate
import qualified Data.Text as T
import GitHub
import Lens.Micro
import Relude
import Sauron.Types
import Sauron.UI.AttrMap
import Sauron.UI.Issue (issueInner, maxCommentWidth)
import Sauron.UI.Modals.NewIssueModal (renderBodyEditor)
import WEditorBrick.WrappingEditor (WrappingEditor, dumpEditor)


renderModal :: AppState -> ModalState Fixed -> Widget ClickableName
renderModal appState (CommentModalState {_commentIssue=issue@(Issue {issueNumber=(IssueNumber num)}), ..}) =
  vBox [
    hCenter $ withAttr boldText $ str modalTitle

    , hBorder

    -- Scrollable content area with issue and comments
    , withVScrollBars OnRight $ withVScrollBarHandles $ viewport CommentModalContent Vertical $ vBox [
        hLimit maxCommentWidth $ issueInner (appState ^. appNow) issue _commentIssueComments
        , hBorder
        , str " "
        , renderBodyEditor appState True modalWidth editorLines _commentEditor
        ]
    , hBorder
    , buttonSection _commentEditor issue _submissionState
  ]
  & border
  & withAttr normalAttr
  & hLimit modalWidth
  & vLimitPercent 90
  & centerLayer
  where
    typ :: Text
    typ = if _issueIsPR then "Pull Request" else "Issue"

    modalTitle = [i|Comment on #{typ} \##{num}|]

    modalWidth = case _appMainUiExtent appState of
      Nothing -> maxCommentWidth + 4
      Just (Extent {extentSize=(w, _h)}) -> round ((0.8 :: Double) * fromIntegral w)

    bodyLineCount = length (dumpEditor _commentEditor)
    editorLines = max 10 (min bodyLineCount 30)
renderModal _ _ = str "Invalid modal state for CommentModal" -- This should never happen

buttonSection :: WrappingEditor Char ClickableName -> Issue -> SubmissionState -> Widget ClickableName
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
    text = T.intercalate "\n" $ map toText $ dumpEditor editor
    hasText = not $ T.null $ T.strip text
    buttonText = if hasText then "Close with comment" else "Close issue"
