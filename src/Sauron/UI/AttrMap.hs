{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Sauron.UI.AttrMap where

import Brick
import Brick.Widgets.ProgressBar
import qualified Graphics.Vty as V
import Relude


mkAttrName :: String -> AttrName
mkAttrName = attrName

mainAttrMap :: AttrMap
mainAttrMap = attrMap V.defAttr [
  -- (listAttr, V.white `on` V.blue)
  -- (listSelectedAttr, V.blue `on` V.white)
  -- (listSelectedAttr, bg (V.Color240 $ V.rgbColorToColor240 0 1 0))
  -- (selectedAttr, bg (V.Color240 $ V.rgbColorToColor240 0 1 0))

  -- Statuses
  (iconAttr, fg V.white)
  , (normalAttr, fg V.white)
  , (notFetchedAttr, fg midGray)
  , (fetchingAttr, fg V.blue)
  , (erroredAttr, fg V.red)

  -- Progress bar
  , (progressCompleteAttr, bg (V.Color240 235))
  , (progressIncompleteAttr, bg (V.Color240 225))

  -- Main list
  , (toggleMarkerAttr, fg midGray)
  , (openMarkerAttr, fg midGray)

  -- Hotkey stuff
  , (hotkeyAttr, fg V.blue)
  , (disabledHotkeyAttr, fg midGray)
  , (hotkeyMessageAttr, fg brightWhite)
  , (disabledHotkeyMessageAttr, fg brightGray)
  ]

iconAttr :: AttrName
iconAttr = mkAttrName "icon"

normalAttr :: AttrName
normalAttr = mkAttrName "normal"

notFetchedAttr :: AttrName
notFetchedAttr = mkAttrName "not_fetched"

fetchingAttr :: AttrName
fetchingAttr = mkAttrName "fetching"

erroredAttr :: AttrName
erroredAttr = mkAttrName "errored"

toggleMarkerAttr :: AttrName
toggleMarkerAttr = mkAttrName "toggleMarker"

openMarkerAttr :: AttrName
openMarkerAttr = mkAttrName "openMarker"

hotkeyAttr, disabledHotkeyAttr, hotkeyMessageAttr, disabledHotkeyMessageAttr :: AttrName
hotkeyAttr = mkAttrName "hotkey"
disabledHotkeyAttr = mkAttrName "disableHotkey"
hotkeyMessageAttr = mkAttrName "hotkeyMessage"
disabledHotkeyMessageAttr = mkAttrName "disabledHotkeyMessage"

-- * Colors

solarizedBase03 = V.rgbColor 0x00 0x2b 0x36
solarizedBase02 = V.rgbColor 0x07 0x36 0x42
solarizedBase01 = V.rgbColor 0x58 0x6e 0x75
solarizedbase00 = V.rgbColor 0x65 0x7b 0x83
solarizedBase0 = V.rgbColor 0x83 0x94 0x96
solarizedBase1 = V.rgbColor 0x93 0xa1 0xa1
solarizedBase2 = V.rgbColor 0xee 0xe8 0xd5
solarizedBase3 = V.rgbColor 0xfd 0xf6 0xe3
solarizedYellow = V.rgbColor 0xb5 0x89 0x00
solarizedOrange = V.rgbColor 0xcb 0x4b 0x16
solarizedRed = V.rgbColor 0xdc 0x32 0x2f
solarizedMagenta = V.rgbColor 0xd3 0x36 0x82
solarizedViolet = V.rgbColor 0x6c 0x71 0xc4
solarizedBlue = V.rgbColor 0x26 0x8b 0xd2
solarizedCyan = V.rgbColor 0x2a 0xa1 0x98
solarizedGreen = V.rgbColor 0x85 0x99 0x00

midGray = grayAt 50
brightGray = grayAt 80
midWhite = grayAt 140
brightWhite = grayAt 200

grayAt level = V.rgbColor level level level
-- grayAt level = V.Color240 $ V.rgbColorToColor240 level level level
