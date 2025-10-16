module Sauron.UI.CommentModal (
  renderModal,
  commentEditor
  ) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Edit
import qualified Data.Text as T
import Relude
import Sauron.Types
import Sauron.UI.AttrMap
import Sauron.UI.Markdown (markdownToWidgetsWithWidth)

renderModal :: AppState -> ModalState -> Widget ClickableName
renderModal appState (CommentModalState text issueNumber isPR) = renderCommentModal appState text issueNumber isPR

renderCommentModal :: AppState -> Text -> Int -> Bool -> Widget ClickableName
renderCommentModal _appState text issueNumber isPR =
  let modalTitle = if isPR
                   then "Comment on Pull Request #" <> show issueNumber
                   else "Comment on Issue #" <> show issueNumber
  in commentCenterLayer $
     withAttr normalAttr $
     border $
     hBox [
       -- Left column: Editor
       vBox [
         hCenter $ withAttr boldText $ str modalTitle,
         hBorder,
         padAll 1 $
           hLimit 60 $
           vLimit 20 $
           withAttr normalAttr $
           renderEditor (str . toString . T.unlines) True (commentEditor text),
         hBorder,
         hCenter $ withAttr italicText $ str "Ctrl+Enter to submit • Esc to cancel"
       ],
       vBorder,
       -- Right column: Preview
       vBox [
         hCenter $ withAttr boldText $ str "Preview",
         hBorder,
         padAll 1 $
           hLimit 60 $
           vLimit 20 $
           viewport CommentModal Vertical $
           if T.null text
             then withAttr italicText $ str "(preview will appear here)"
             else markdownToWidgetsWithWidth 58 text
       ]
     ]

-- Create an editor from the comment text
commentEditor :: Text -> Editor Text ClickableName
commentEditor text =
  let textLines = T.lines text
  in editorText CommentEditor (Just 1) (T.unlines textLines)

-- Helper to create a centered layer that covers most of the screen
commentCenterLayer :: Widget ClickableName -> Widget ClickableName
commentCenterLayer widget =
  center $
  hLimit 130 $
  vLimit 25 $
  widget
