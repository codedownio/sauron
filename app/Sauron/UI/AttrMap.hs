{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Sauron.UI.AttrMap where

import Brick
import qualified Brick.Widgets.Edit as E
import Brick.Widgets.ProgressBar
import Brick.Widgets.Skylighting (attrMappingsForStyle)
import qualified Graphics.Vty as V
import Relude hiding (on)
import qualified Skylighting.Styles as Sky
import qualified Skylighting.Types as SkyTypes


mkAttrName :: String -> AttrName
mkAttrName = attrName

buildAdaptiveAttrMap :: V.ColorMode -> AttrMap
buildAdaptiveAttrMap colorMode = attrMap V.defAttr ([
  -- Statuses
  (iconAttr, fg V.white)
  , (normalAttr, fg V.white)
  , (notFetchedAttr, fg (select midGray))
  , (fetchingAttr, fg V.blue)
  , (erroredAttr, fg V.red)

  -- Pagination
  , (searchAttr, fg (select midGray))
  , (selectedPageAttr, fg (select solarizedGreen) & flip V.withStyle V.bold)
  , (notSelectedPageAttr, fg (select midGray) & flip V.withStyle V.dim)
  , (pageEllipsesAttr, fg (select midGray))

  -- Stats box
  , (starsAttr, fg V.yellow)

  -- Workflow icons
  , (cancelledAttr, fg (select midGray))
  , (greenCheckAttr, fg V.green)
  , (redXAttr, fg V.red)
  , (ellipsesAttr, fg (select midGray))
  , (neutralAttr, fg (select midGray))
  , (unknownAttr, fg V.white)
  , (queuedAttr, fg (select githubOrange))

  -- Notification styles
  , (unreadNotificationAttr, bg (select unreadNotificationBg))
  , (blueDotAttr, fg V.blue)

  -- Progress bar
  , (progressCompleteAttr, bg (select (V.Color240 235, V.Color240 235, V.brightBlack, V.black, V.black)))
  , (progressIncompleteAttr, bg (select (V.Color240 225, V.Color240 225, V.black, V.black, V.black)))

  -- Main list
  , (toggleMarkerAttr, fg (select midGray))
  , (openMarkerAttr, fg (select midGray))

  -- Hotkey stuff
  , (hotkeyAttr, fg V.blue)
  , (disabledHotkeyAttr, fg (select midGray))
  , (hotkeyMessageAttr, fg (select brightWhite))
  , (disabledHotkeyMessageAttr, fg (select brightGray))

  -- Spinner
  , (circleSpinnerAttr, fg (select brightGray))

  -- General UI
  , (branchAttr, fg (select solarizedBlue))
  , (hashAttr, fg (select midGray))
  , (hashNumberAttr, fg (select solarizedViolet))
  , (usernameAttr, fg (select solarizedBlue))

  -- Markdown
  , (italicText, style V.italic)
  , (underlineText, style V.underline)
  , (boldText, style V.bold)
  , (boldUnderlineText, V.defAttr `V.withStyle` V.bold `V.withStyle` V.underline)
  , (strikeoutText, style V.strikethrough)
  , (codeText, select brightWhite `on` select dimGray)
  , (codeBlockText, fg (select midGray))
  , (horizontalRuleAttr, fg (select midGray))

  -- Command lines inside job logs
  , (commandAttr, fg (select solarizedBlue) & flip V.withStyle V.bold)

  -- Diff line backgrounds
  , (diffAddedAttr, select brightWhite `on` select diffAddedBg)
  , (diffRemovedAttr, select brightWhite `on` select diffRemovedBg)
  , (diffContextAttr, fg (select brightWhite))

  -- Background-only diff attributes for syntax highlighting
  , (diffAddedBgAttr, bg (select diffAddedBg))
  , (diffRemovedBgAttr, bg (select diffRemovedBg))

  -- Forms
  , (E.editFocusedAttr, V.black `on` V.yellow)

  -- Event colors (GitHub-style)
  , (eventClosedColor, fg (select solarizedViolet))
  , (eventReopenedColor, fg (select solarizedGreen))
  , (eventAssignedColor, fg (select solarizedBlue))
  , (eventUnassignedColor, fg (select midGray))
  , (eventLabeledColor, fg (select solarizedBlue))
  , (eventUnlabeledColor, fg (select midGray))
  , (eventMilestoneColor, fg (select solarizedGreen))
  , (eventRenamedColor, fg (select solarizedOrange))
  , (eventLockedColor, fg (select solarizedYellow))
  , (eventUnlockedColor, fg (select midGray))
  , (eventReferencedColor, fg (select midGray))
  , (eventMergedColor, fg (select solarizedViolet))
  , (eventMentionedColor, fg (select solarizedBlue))
  , (eventSubscribedColor, fg (select solarizedGreen))
  , (eventUnsubscribedColor, fg (select midGray))
  , (eventReviewColor, fg (select solarizedYellow))
  , (eventDuplicateColor, fg (select midGray))
  , (eventProjectColor, fg (select solarizedGreen))
  , (eventConvertedColor, fg (select solarizedBlue))
  , (eventRefDeletedColor, fg V.red)
  , (eventRefRestoredColor, fg (select solarizedGreen))
  , (eventRefUnknownColor, fg V.red)

  -- Timeline colors
  , (timelineBorderAttr, fg (select solarizedBlue))

  -- Log levels
  , (errorLogAttr, fg V.red)
  , (warningLogAttr, fg V.yellow)
  , (infoLogAttr, fg V.white)
  , (debugLogAttr, fg (select midGray))
  ]
  <> attrMappingsForStyleNoBg Sky.breezeDark
  )
  where
    select = selectColor colorMode

iconAttr = mkAttrName "icon"
normalAttr = mkAttrName "normal"
notFetchedAttr = mkAttrName "not_fetched"
fetchingAttr = mkAttrName "fetching"
erroredAttr = mkAttrName "errored"

toggleMarkerAttr = mkAttrName "toggleMarker"
openMarkerAttr = mkAttrName "openMarker"

hotkeyAttr = mkAttrName "hotkey"
disabledHotkeyAttr = mkAttrName "disableHotkey"
hotkeyMessageAttr = mkAttrName "hotkeyMessage"
disabledHotkeyMessageAttr = mkAttrName "disabledHotkeyMessage"

circleSpinnerAttr = mkAttrName "circleSpinner"

-- * Search and pagination

searchAttr = mkAttrName "search"
selectedPageAttr = mkAttrName "selectedPage"
notSelectedPageAttr = mkAttrName "notSelectedPage"
pageEllipsesAttr = mkAttrName "pageEllipses"

-- * Stats box

starsAttr = mkAttrName "stars"

-- * Workflow icons

cancelledAttr = mkAttrName "cancelled"
greenCheckAttr = mkAttrName "greenCheck"
redXAttr = mkAttrName "redX"
ellipsesAttr = mkAttrName "ellipses"
neutralAttr = mkAttrName "neutral"
unknownAttr = mkAttrName "neutral"
queuedAttr = mkAttrName "queued"

-- * General UI

branchAttr = mkAttrName "branch"
hashAttr = mkAttrName "hash"
hashNumberAttr = mkAttrName "hashNumber"
usernameAttr = mkAttrName "username"

-- * Notification styles

unreadNotificationAttr = mkAttrName "unreadNotification"
blueDotAttr = mkAttrName "blueDot"

-- * Text

italicText = mkAttrName "italic-text"
underlineText = mkAttrName "underline-text"
boldText = mkAttrName "bold-text"
boldUnderlineText = mkAttrName "bold-underline-text"
strikeoutText = mkAttrName "strikeout-text"
codeText = mkAttrName "code-text"
codeBlockText = mkAttrName "code-block-text"
horizontalRuleAttr = mkAttrName "horizontal-rule"
commandAttr = mkAttrName "command"

-- * Diff line backgrounds

diffAddedAttr = mkAttrName "diff-added"
diffRemovedAttr = mkAttrName "diff-removed"
diffContextAttr = mkAttrName "diff-context"

-- Background-only diff attributes for syntax highlighting
diffAddedBgAttr = mkAttrName "diff-added-bg"
diffRemovedBgAttr = mkAttrName "diff-removed-bg"

-- * Event colors

eventClosedColor = mkAttrName "eventClosedColor"
eventReopenedColor = mkAttrName "eventReopenedColor"
eventAssignedColor = mkAttrName "eventAssignedColor"
eventUnassignedColor = mkAttrName "eventUnassignedColor"
eventLabeledColor = mkAttrName "eventLabeledColor"
eventUnlabeledColor = mkAttrName "eventUnlabeledColor"
eventMilestoneColor = mkAttrName "eventMilestoneColor"
eventRenamedColor = mkAttrName "eventRenamedColor"
eventLockedColor = mkAttrName "eventLockedColor"
eventUnlockedColor = mkAttrName "eventUnlockedColor"
eventReferencedColor = mkAttrName "eventReferencedColor"
eventMergedColor = mkAttrName "eventMergedColor"
eventMentionedColor = mkAttrName "eventMentionedColor"
eventSubscribedColor = mkAttrName "eventSubscribedColor"
eventUnsubscribedColor = mkAttrName "eventUnsubscribedColor"
eventReviewColor = mkAttrName "eventReviewColor"
eventDuplicateColor = mkAttrName "eventDuplicateColor"
eventProjectColor = mkAttrName "eventProjectColor"
eventConvertedColor = mkAttrName "eventConvertedColor"
eventRefDeletedColor = mkAttrName "eventRefDeletedColor"
eventRefRestoredColor = mkAttrName "eventRefRestoredColor"
eventRefUnknownColor = mkAttrName "eventRefUnknownColor"

-- * Timeline colors

timelineBorderAttr = mkAttrName "timelineBorder"

-- * Log levels

errorLogAttr = mkAttrName "error"
warningLogAttr = mkAttrName "warning"
infoLogAttr = mkAttrName "info"
debugLogAttr = mkAttrName "debug"

-- * Color Fallback System

-- Type alias for color fallbacks: (FullColor, ColorMode240, ColorMode16, ColorMode8, NoColor)
type ColorFallback = (V.Color, V.Color, V.Color, V.Color, V.Color)

-- Color selection function based on detected terminal capabilities
selectColor :: V.ColorMode -> ColorFallback -> V.Color
selectColor colorMode (fullColor, color240, color16, color8, noColor) = case colorMode of
  V.FullColor      -> fullColor
  V.ColorMode240 _ -> color240
  V.ColorMode16    -> color16
  V.ColorMode8     -> color8
  V.NoColor        -> noColor

-- * Color Definitions with Fallbacks

solarizedBase03 :: ColorFallback
solarizedBase03 = (V.rgbColor 0x00 0x2b 0x36, V.Color240 234, V.black, V.black, V.black)

solarizedBase02 :: ColorFallback
solarizedBase02 = (V.rgbColor 0x07 0x36 0x42, V.Color240 235, V.black, V.black, V.black)

solarizedBase01 :: ColorFallback
solarizedBase01 = (V.rgbColor 0x58 0x6e 0x75, V.Color240 240, V.brightBlack, V.black, V.black)

solarizedbase00 :: ColorFallback
solarizedbase00 = (V.rgbColor 0x65 0x7b 0x83, V.Color240 241, V.brightBlack, V.black, V.black)

solarizedBase0 :: ColorFallback
solarizedBase0 = (V.rgbColor 0x83 0x94 0x96, V.Color240 244, V.brightBlack, V.white, V.white)

solarizedBase1 :: ColorFallback
solarizedBase1 = (V.rgbColor 0x93 0xa1 0xa1, V.Color240 245, V.brightWhite, V.white, V.white)

solarizedBase2 :: ColorFallback
solarizedBase2 = (V.rgbColor 0xee 0xe8 0xd5, V.Color240 254, V.brightWhite, V.white, V.white)

solarizedBase3 :: ColorFallback
solarizedBase3 = (V.rgbColor 0xfd 0xf6 0xe3, V.Color240 230, V.brightWhite, V.white, V.white)

solarizedYellow :: ColorFallback
solarizedYellow = (V.rgbColor 0xb5 0x89 0x00, V.Color240 136, V.brightYellow, V.yellow, V.white)

solarizedOrange :: ColorFallback
solarizedOrange = (V.rgbColor 0xcb 0x4b 0x16, V.Color240 166, V.brightRed, V.red, V.white)

solarizedRed :: ColorFallback
solarizedRed = (V.rgbColor 0xdc 0x32 0x2f, V.Color240 160, V.brightRed, V.red, V.white)

solarizedMagenta :: ColorFallback
solarizedMagenta = (V.rgbColor 0xd3 0x36 0x82, V.Color240 125, V.brightMagenta, V.magenta, V.white)

solarizedViolet :: ColorFallback
solarizedViolet = (V.rgbColor 0x6c 0x71 0xc4, V.Color240 61, V.brightBlue, V.blue, V.white)

solarizedBlue :: ColorFallback
solarizedBlue = (V.rgbColor 0x26 0x8b 0xd2, V.Color240 33, V.brightBlue, V.blue, V.white)

solarizedCyan :: ColorFallback
solarizedCyan = (V.rgbColor 0x2a 0xa1 0x98, V.Color240 37, V.brightCyan, V.cyan, V.white)

solarizedGreen :: ColorFallback
solarizedGreen = (V.rgbColor 0x85 0x99 0x00, V.Color240 106, V.brightGreen, V.green, V.white)

githubOrange :: ColorFallback
githubOrange = (V.rgbColor 0xd2 0x99 0x22, V.Color240 178, V.brightYellow, V.yellow, V.white)

dimGray :: ColorFallback
dimGray = (grayAtRGB 25, V.Color240 236, V.brightWhite, V.white, V.white)

midGray :: ColorFallback
midGray = (grayAtRGB 50, V.Color240 238, V.brightWhite, V.white, V.white)

brightGray :: ColorFallback
brightGray = (grayAtRGB 80, V.Color240 244, V.brightWhite, V.white, V.white)

midWhite :: ColorFallback
midWhite = (grayAtRGB 140, V.Color240 249, V.brightWhite, V.white, V.white)

brightWhite :: ColorFallback
brightWhite = (grayAtRGB 200, V.Color240 253, V.brightWhite, V.white, V.white)

unreadNotificationBg :: ColorFallback
unreadNotificationBg = (V.rgbColor 0x1a 0x2f 0x3a, V.Color240 236, V.brightBlack, V.black, V.black)

-- Diff background colors (nuanced for capable terminals, ANSI fallbacks for compatibility)
diffAddedBg :: ColorFallback
diffAddedBg = (V.rgbColor 0x1a 0x3a 0x1a, V.Color240 28, V.green, V.green, V.black)

diffRemovedBg :: ColorFallback
diffRemovedBg = (V.rgbColor 0x3a 0x1a 0x1a, V.Color240 88, V.red, V.red, V.black)

grayAtRGB :: Word8 -> V.Color
grayAtRGB level = V.rgbColor level level level

-- | Create attribute mappings from a skylighting style, but strip all background colors
-- to avoid conflicts with our diff backgrounds
attrMappingsForStyleNoBg :: SkyTypes.Style -> [(AttrName, V.Attr)]
attrMappingsForStyleNoBg style =
  map stripBackground (attrMappingsForStyle style)
  where
    stripBackground :: (AttrName, V.Attr) -> (AttrName, V.Attr)
    stripBackground (name, attr) = (name, attr { V.attrBackColor = V.Default })
