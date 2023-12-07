
module Sauron.UI.Draw (
  drawUI
  ) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.List
import qualified Brick.Widgets.List as L
import Control.Monad
import qualified Data.List as L
import Data.Maybe
import Data.String.Interpolate
import qualified Data.Text as T
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
      -- , do
      --     guarding (isJust (app ^. (listSelectedElement (app ^. appMainList))))
      --       (clickable InfoBar $ infoBar app)
      ]

mainList :: AppState -> Widget ClickableName
mainList app = hCenter $ padAll 1 $ L.renderListWithIndex listDrawElement True (app ^. appMainList)
  where
    listDrawElement ix isSelected x@(MainListElemHeading {..}) = clickable (ListRow ix) $ padLeft (Pad (4 * _depth)) $ (if isSelected then border else id) $ vBox $ catMaybes [
      Just $ renderLine isSelected x
      ]
    listDrawElement ix isSelected x@(MainListElemRepo {..}) = clickable (ListRow ix) $ padLeft (Pad (4 * _depth)) $ (if isSelected then border else id) $ vBox $ catMaybes [
      Just $ renderLine isSelected x
      , do
          guard _toggled
          let infoWidgets = getInfoWidgets x
          guard (not $ L.null infoWidgets)
          return $ padLeft (Pad 4) $
            fixedHeightOrViewportPercent (InnerViewport [i|viewport_#{_ident}|]) 33 $
              vBox infoWidgets
      ]

    renderLine _isSelected (MainListElemHeading {..}) = hBox $ catMaybes [
      Just $ withAttr openMarkerAttr $ str (if _toggled then "[-] " else "[+] ")
      , Just (renderHeadingName _label _status)
      , Just (padLeft Max (str "⠀"))
      ]
    renderLine _isSelected (MainListElemRepo {_repo, ..}) = hBox $ catMaybes [
      Just $ withAttr openMarkerAttr $ str (if _toggled then "[-] " else "[+] ")
      , Just (renderName _repo _status)
      , Just (padLeft Max (statsBox _repo))
      ]

    getInfoWidgets (MainListElemHeading {}) = [
      str "HI I'M THE HEADING INFO"
      ]
    getInfoWidgets (MainListElemRepo {}) = [
      str "HI I'M THE WORKFLOW INFO"
      ]


borderWithCounts :: AppState -> Widget n
borderWithCounts (AppState {_appUser=(User {userLogin=(N name), ..})}) = hBorderWithLabel $ padLeftRight 1 $ hBox [str [i|#{name} (#{userPublicRepos} public repos, #{userFollowers} followers)|]]

infoBar :: AppState -> Widget n
infoBar s = Widget Greedy Fixed $ do
  _c <- getContext
  case listSelectedElement (s ^. appMainList) of
    Nothing -> render $ hBox [str ""]
    Just (_, MainListElemHeading {}) -> render $ hBox [str ""]
    Just (_, MainListElemRepo {_repo}) -> render $ hBox [str (T.unpack (T.intercalate ", " phrases))]
      where
        issuesPhrase = case repoOpenIssuesCount _repo of
          0 -> Nothing
          1 -> Just [i|1 issue|]
          n -> Just [i|#{n} issues|]

        phrases = catMaybes [issuesPhrase]

renderHeadingName :: Text -> Status -> Widget n
renderHeadingName l _stat = hBox [
  str (toString l)
  ]

renderName :: Repo -> Status -> Widget n
renderName (Repo {repoName=(N name), repoOwner}) stat = hBox [
  case repoOwner of
    SimpleOwner {simpleOwnerLogin=(N ownerName)} -> withAttr (chooseAttr stat) (str (toString ownerName))
  , withAttr toggleMarkerAttr $ str " / "
  , withAttr (chooseAttr stat) (str (toString name))
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
