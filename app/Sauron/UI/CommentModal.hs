module Sauron.UI.CommentModal (
  renderModal,
  ) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Edit (Editor, getEditContents, renderEditor)
import qualified Data.Text as T
import Relude
import Sauron.Types
import Sauron.UI.AttrMap
import Sauron.UI.Markdown (markdownToWidgetsWithWidth)

renderModal :: AppState -> ModalState -> Widget ClickableName
renderModal appState (CommentModalState editor issueNumber isPR) = renderCommentModal appState editor issueNumber isPR

renderCommentModal :: AppState -> Editor Text ClickableName -> Int -> Bool -> Widget ClickableName
renderCommentModal _appState editor issueNumber isPR =
  sideBySideEditor editor issueNumber isPR
  & border
  & withAttr normalAttr
  & hLimit 130
  & vLimit 25
  & centerLayer

sideBySideEditor :: Editor Text ClickableName -> Int -> Bool -> Widget ClickableName
sideBySideEditor editor issueNumber isPR = hBox [
  -- Left column: Editor
  vBox [
    hCenter $ withAttr boldText $ str modalTitle,
    hBorder,
    padAll 1 $
      hLimit 60 $
      vLimit 20 $
      withAttr normalAttr $
      renderEditor (str . toString . T.unlines) True editor,
    hBorder,
    hCenter $ withAttr italicText $ str "Ctrl+Enter to submit • Esc to cancel"
  ]
  , vBorder
  -- Right column: Preview
  , vBox [
    hCenter $ withAttr boldText $ str "Preview",
    hBorder,
    padAll 1 $
      hLimit 60 $
      vLimit 20 $
      viewport CommentModal Vertical $
      if T.null text
        then withAttr italicText $ str "(preview will appear here)"
        else markdownToWidgetsWithWidth 58 text
  ]]
  where
    modalTitle = if isPR
                 then "Comment on Pull Request #" <> show issueNumber
                 else "Comment on Issue #" <> show issueNumber

    text = T.unlines $ getEditContents editor
