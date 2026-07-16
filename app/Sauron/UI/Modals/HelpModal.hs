module Sauron.UI.Modals.HelpModal (
  renderHelpModal
  ) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Data.Version (showVersion)
import qualified Graphics.Vty as V
import Lens.Micro
import Paths_sauron (version)
import Relude
import Sauron.Types
import Sauron.UI.AttrMap


renderHelpModal :: AppState -> Widget ClickableName
renderHelpModal appState =
  vBox [
    hCenter $ withAttr boldText $ str ("Sauron v" <> showVersion version)
    , hBorder
    , padAll 1 $ vBox [
        sectionHeader "Additional hotkeys"
        , hotkeyRow "Ctrl+L" "Show/hide the logs"
        , hotkeyRow "Ctrl+↑/↓" "Scroll node content by line"
        , hotkeyRow "Ctrl+Home/End" "Scroll node content to top / bottom"

        , str " "
        , infoRow "Color mode (configured)" (showColorModeMaybe (appState ^. appCliColorMode))
        , infoRow "Color mode (detected)" (showColorMode (appState ^. appActualColorMode))
      ]
    , hBorder
    , hCenter $ withAttr hotkeyMessageAttr $ str "Press [?], [Esc], or [q] to close"
  ]
  & border
  & withDefAttr normalAttr
  & hLimit 60
  & centerLayer

sectionHeader :: String -> Widget ClickableName
sectionHeader t = withAttr headingTextAttr $ str t

hotkeyRow :: String -> String -> Widget ClickableName
hotkeyRow keys desc = hBox [
  hLimit 20 $ padRight Max $ hBox [str "[", withAttr hotkeyAttr $ str keys, str "]"]
  , withAttr hotkeyMessageAttr $ str desc
  ]

infoRow :: String -> String -> Widget ClickableName
infoRow label val = hBox [
  hLimit 28 $ padRight Max $ withAttr hotkeyMessageAttr $ str label
  , withAttr boldText $ str val
  ]

showColorModeMaybe :: Maybe V.ColorMode -> String
showColorModeMaybe Nothing = "auto-detected"
showColorModeMaybe (Just x) = showColorMode x

showColorMode :: V.ColorMode -> String
showColorMode V.FullColor = "Full (truecolor)"
showColorMode (V.ColorMode240 _) = "240"
showColorMode V.ColorMode16 = "16"
showColorMode V.ColorMode8 = "8"
showColorMode V.NoColor = "None"
