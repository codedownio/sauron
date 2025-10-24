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
  vBox [
    padBottom Max $ sideBySideEditor editor issueNumber isPR
    , hBorder
    , buttonSection editor issueNumber isPR
  ]
  & border
  & withAttr normalAttr
  & hLimit 130
  & vLimit 30
  & centerLayer

sideBySideEditor :: Editor Text ClickableName -> Int -> Bool -> Widget ClickableName
sideBySideEditor editor issueNumber isPR = hBox [
  -- Left column: Editor
  vBox [
    hCenter $ withAttr boldText $ str modalTitle
    , hBorder
    , padAll 1 $
        vLimit 20 $
        withAttr normalAttr $
        renderEditor (str . toString . T.unlines) True editor
    , fill ' '
    ]
  , vBorder
  -- Right column: Preview
  , vBox [
    hCenter $ withAttr boldText $ str "Preview"
    , hBorder
    , padAll 1 $
        hLimit 60 $
        vLimit 20 $
        viewport CommentModal Vertical $
        if T.null text
          then withAttr italicText $ str "(preview will appear here)"
          else markdownToWidgetsWithWidth 58 text
    ]
  ]
  where
    modalTitle = if isPR
                 then "Comment on Pull Request #" <> show issueNumber
                 else "Comment on Issue #" <> show issueNumber

    text = T.unlines $ getEditContents editor

buttonSection :: Editor Text ClickableName -> Int -> Bool -> Widget ClickableName
buttonSection editor _issueNumber _isPR =
  padLeft Max $ hBox [
      border $ hBox [
        str " "
        , withAttr hotkeyMessageAttr $ str buttonText
        , str " ["
        , withAttr hotkeyAttr $ str "Ctrl+Shift+Enter"
        , str "] "
        ]
      , str "  "
      , border $ hBox [
          str " "
          , withAttr (if hasText then hotkeyMessageAttr else disabledHotkeyMessageAttr) $ str "Comment"
          , str " ["
          , withAttr (if hasText then hotkeyAttr else disabledHotkeyAttr) $ str "Ctrl+Enter"
          , str "] "
          ]
    ]
  where
    text = T.unlines $ getEditContents editor
    hasText = not $ T.null $ T.strip text
    buttonText = if hasText then "Close with comment" else "Close issue"
