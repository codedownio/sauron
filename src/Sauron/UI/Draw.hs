
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
import GitHub hiding (Status)
import GitHub.Data.Name
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

    renderLine _isSelected (MainListElem {repo, ..}) = hBox $ catMaybes [
      Just $ withAttr openMarkerAttr $ str (if open then "[-] " else "[+] ")
      , Just (renderName repo status)
      , Just (padLeft Max (statsBox repo))
      ]

    getInfoWidgets (MainListElem {}) = [] -- catMaybes [Just $ runReader (toBrickWidget status) (app ^. appCustomExceptionFormatters)]


borderWithCounts :: AppState -> Widget n
borderWithCounts (AppState {_appUser=(User {userLogin=(N name), ..})}) = hBorderWithLabel $ padLeftRight 1 $ hBox [str [i|#{name} (#{userPublicRepos} public repos, #{userFollowers} followers)|]]

infoBar :: AppState -> Widget n
infoBar _app = Widget Greedy Fixed $ do
  _c <- getContext
  render $ hBox [str "Info bar"]

renderName :: Repo -> Status -> Widget n
renderName (Repo {repoName=(N name), repoOwner}) status = hBox [
  case repoOwner of
    SimpleOwner {simpleOwnerLogin=(N ownerName)} -> withAttr (chooseAttr status) (str (toString ownerName))
  , withAttr toggleMarkerAttr $ str " / "
  , withAttr (chooseAttr status) (str (toString name))
  ]

statsBox :: Repo -> Widget n
statsBox (Repo {..}) = hBox $ catMaybes [
  guarding (repoWatchersCount > 0) $ padLeft (Pad 1) (
      padRight (Pad 2) (withAttr infoAttr (str "👁️"))
      <+> str (show repoWatchersCount)
      )

  , guarding (repoForksCount > 0) $ padLeft (Pad 1) (
      padRight (Pad 1) (withAttr infoAttr (str "⑂"))
      <+> str (show repoForksCount)
      )

  , guarding (repoStargazersCount > 0) $ padLeft (Pad 1) (
      padRight (Pad 1) (withAttr infoAttr (str "★"))
      <+> str (show repoStargazersCount)
      )
  ]

guarding :: (Monad m, Alternative m) => Bool -> b -> m b
guarding p widget = do
  guard p
  return widget
