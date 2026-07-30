{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Sauron.UI.Modals.SpeedScopePickerModal (
  renderSpeedScopePickerModal
  ) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import qualified Brick.Widgets.List as L
import GitHub
import qualified Graphics.Vty as V
import Lens.Micro
import Relude
import Sauron.Types
import Sauron.UI.AttrMap


renderSpeedScopePickerModal :: AppState -> ModalState Fixed -> Widget ClickableName
renderSpeedScopePickerModal _appState (SpeedScopePickerModalState {_speedScopePickerList=theList, _speedScopePickerTitle=title}) =
  vBox [
    hCenter $ withAttr boldText $ txt ("Open speedscope — " <> title)
    , hCenter $ withAttr hotkeyMessageAttr $ txt (show (length (L.listElements theList)) <> " artifacts containing speedscope.json")
    , hBorder
    , padLeftRight 1 $ vLimit 15 $ L.renderList drawRow True theList
    , hBorder
    , hCenter $ withAttr hotkeyMessageAttr $ str "[↑/↓] Select  [Enter] Open  [q] Close"
  ]
  & border
  & withDefAttr normalAttr
  & hLimit 74
  & centerLayer
  where
    -- The app's attrMap doesn't define brick's list-selected attrs, so highlight the
    -- selected row explicitly with reverse video (and keep it scrolled into view).
    drawRow selected (Artifact {artifactName, artifactSizeInBytes}) =
      style $ padRight Max $ hBox [
        txt (" " <> artifactName)
        , padLeft Max $ str (humanSize artifactSizeInBytes <> " ")
        ]
      where
        style
          | selected = visible . modifyDefAttr (`V.withStyle` V.reverseVideo)
          | otherwise = id

humanSize :: Int -> String
humanSize n
  | n < 1024 = show n <> " B"
  | n < 1024 * 1024 = show (n `div` 1024) <> " KB"
  | otherwise = show (n `div` (1024 * 1024)) <> "." <> show ((n * 10 `div` (1024 * 1024)) `mod` 10) <> " MB"
