
module Sauron.UI.Draw (
  drawUI
  ) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import qualified Brick.Widgets.List as L
import Control.Monad
import qualified Data.List as L
import Data.Maybe
import Data.String.Interpolate
import Lens.Micro hiding (ix)
import Relude
import Sauron.Types
import Sauron.UI.AttrMap
import Sauron.UI.TopBox
import Sauron.UI.Util


drawUI :: AppState -> [Widget ClickableName]
drawUI app = [ui]
  where
    ui = vBox [
      topBox app
      , borderWithCounts app
      , mainList app
      , clickable InfoBar $ infoBar app
      ]

mainList :: AppState -> Widget ClickableName
mainList app = hCenter $ padAll 1 $ L.renderListWithIndex listDrawElement True (app ^. appMainList)
  where
    listDrawElement ix isSelected x@(MainListElem {..}) = clickable (ListRow ix) $ padLeft (Pad (4 * depth)) $ (if isSelected then border else id) $ vBox $ catMaybes [
      Just $ renderLine isSelected x
      , do
          guard toggled
          let infoWidgets = getInfoWidgets x
          guard (not $ L.null infoWidgets)
          return $ padLeft (Pad 4) $
            fixedHeightOrViewportPercent (InnerViewport [i|viewport_#{ident}|]) 33 $
              vBox infoWidgets
      ]

    renderLine _isSelected (MainListElem {..}) = hBox $ catMaybes [
      Just $ withAttr openMarkerAttr $ str (if open then "[-] " else "[+] ")
      , Just $ withAttr (chooseAttr status) (str label)
      -- , if not (app ^. appShowFileLocations) then Nothing else
      --     case runTreeLoc node of
      --       Nothing -> Nothing
      --       Just loc ->
      --         Just $ hBox [str " ["
      --                     , withAttr logFilenameAttr $ str $ srcLocFile loc
      --                     , str ":"
      --                     , withAttr logLineAttr $ str $ show $ srcLocStartLine loc
      --                     , str "]"]
      -- , if not (app ^. appShowVisibilityThresholds) then Nothing else
      --     Just $ hBox [str " ["
      --                 , withAttr visibilityThresholdIndicatorMutedAttr $ str "V="
      --                 , withAttr visibilityThresholdIndicatorAttr $ str $ show visibilityLevel
      --                 , str "]"]
      , Just $ padRight Max $ withAttr toggleMarkerAttr $ str (if toggled then " [-]" else " [+]")
      , Nothing -- if not (app ^. appShowRunTimes) then Nothing else case status of
          -- Running {..} -> Just $ str $ show statusStartTime
          -- Done {..} -> Just $ raw $ V.string attr $ formatNominalDiffTime (diffUTCTime statusEndTime statusStartTime)
          --   where totalElapsed = realToFrac (max (app ^. appTimeSinceStart) (diffUTCTime statusEndTime (app ^. appStartTime)))
          --         duration = realToFrac (diffUTCTime statusEndTime statusStartTime)
          --         intensity :: Double = logBase (totalElapsed + 1) (duration + 1)
          --         minGray :: Int = 50
          --         maxGray :: Int = 255
          --         level :: Int = min maxGray $ max minGray $ round (fromIntegral minGray + (intensity * (fromIntegral (maxGray - minGray))))
          --         attr = V.Attr {
          --           V.attrStyle = V.Default
          --           , V.attrForeColor = V.SetTo (grayAt level)
          --           , V.attrBackColor = V.Default
          --           , V.attrURL = V.Default
          --           }
          -- _ -> Nothing
      ]

    getInfoWidgets mle@(MainListElem {..}) = [] -- catMaybes [Just $ runReader (toBrickWidget status) (app ^. appCustomExceptionFormatters)]


borderWithCounts :: AppState -> Widget n
borderWithCounts _app = hBorderWithLabel $ padLeftRight 1 $ hBox [str "asdf"]

infoBar :: AppState -> Widget n
infoBar _app = Widget Greedy Fixed $ do
  c <- getContext
  render $ hBox [str "asdf"]
