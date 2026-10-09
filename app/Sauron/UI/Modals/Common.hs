module Sauron.UI.Modals.Common (
  modalWidth
  , modalHeightPercent
  , modalTitle
  , renderBodyEditor
  ) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Data.String.Interpolate
import qualified Data.Text as T
import Relude
import Sauron.Types
import Sauron.UI.AttrMap
import Sauron.UI.Issue.Events (maxCommentWidth)
import Sauron.UI.Markdown (markdownToWidgetsWithWidth)
import Sauron.UI.Statuses (getQuarterCircleSpinner)
import WEditorBrick.WrappingEditor (WrappingEditor, dumpEditor)
import qualified WEditorBrick.WrappingEditor as WEditorBrick


-- | Calculate modal width: 80% of UI width, or maxCommentWidth + 4 as fallback
modalWidth :: AppState -> Int
modalWidth appState = case _appMainUiExtent appState of
  Nothing -> maxCommentWidth + 4
  Just (Extent {extentSize=(w, _h)}) -> round ((0.8 :: Double) * fromIntegral w)

-- | Modal height as percentage of screen height
modalHeightPercent :: Int
modalHeightPercent = 95

-- | A modal's centered title, with a spinner beside it while a refresh is in flight. The
-- spinner's space is held open either way, so the title doesn't shift as it comes and goes.
modalTitle :: AppState -> String -> Widget n
modalTitle appState title = hCenter $ hBox [
  withAttr boldText $ str title
  , padLeft (Pad 1) $
      if _appModalRefreshing appState
      then getQuarterCircleSpinner (_appAnimationCounter appState)
      else str " "
  ]

-- | A markdown body editor with a live preview beside it
renderBodyEditor :: AppState -> Bool -> Int -> Int -> WrappingEditor Char ClickableName -> Widget ClickableName
renderBodyEditor (AppState {_appDetailsExpanded}) focused totalWidth editorHeight editor =
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
            t -> markdownToWidgetsWithWidth _appDetailsExpanded (sectionWidth - 4) t
    ]
  ]
  where
    sectionWidth = (totalWidth - 4) `div` 2

    text = T.intercalate "\n" $ map toText $ dumpEditor editor
