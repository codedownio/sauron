
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
import Sauron.UI.Draw.WorkflowLine
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
    listDrawElement :: Int -> Bool -> MainListElem -> Widget ClickableName
    listDrawElement ix isSelected x@(MainListElemHeading {..}) = clickable (ListRow ix) $ padLeft (Pad (4 * _depth)) $ (if isSelected then border else id) $ vBox $ catMaybes [
      Just $ renderLine isSelected x
      ]
    listDrawElement ix isSelected x@(MainListElemRepo {..}) = clickable (ListRow ix) $ padLeft (Pad (4 * _depth)) $ (if isSelected then border else id) $ vBox $ catMaybes [
      Just $ renderLine isSelected x
      , do
          guard _toggled
          let unfoldWidgets = getUnfoldWigets x
          guard (not $ L.null unfoldWidgets)
          return $ padLeft (Pad 4) $
            fixedHeightOrViewportPercent (InnerViewport [i|viewport_#{_ident}|]) 33 $
              vBox unfoldWidgets
      ]

    renderLine :: Bool -> MainListElem -> Widget ClickableName
    renderLine _isSelected (MainListElemHeading {..}) = hBox $ catMaybes [
      Just $ withAttr openMarkerAttr $ str (if _toggled then "[-] " else "[+] ")
      , Just (renderHeadingName _label _status)
      , Just (padLeft Max (str " "))
      ]
    renderLine _isSelected (MainListElemRepo {_repo, ..}) =
      renderRepoLine _toggled _namespaceName (repoAttr _repo) (healthIndicator _healthCheck) (fetchableStatsBox _repo)

renderRepoLine :: Bool -> (Name Owner, Name Repo) -> AttrName -> Maybe (Widget n) -> Widget n -> Widget n
renderRepoLine isToggled (owner, name) attr maybeHealth rightSide = hBox $ catMaybes [
  Just $ withAttr openMarkerAttr $ str (if isToggled then "[-] " else "[+] ")
  , Just $ hBox [
      withAttr attr (str (toString (untagName owner)))
      , withAttr toggleMarkerAttr $ str " / "
      , withAttr attr (str (toString (untagName name)))
      ]
  , maybeHealth
  , Just (padLeft Max rightSide)
  ]

healthIndicator :: Fetchable HealthCheckResult -> Maybe (Widget n)
healthIndicator (Fetched (HealthCheckWorkflowResult ws)) = Just (padLeft (Pad 1) (workflowStatusToIcon ws))
healthIndicator _ = Nothing

repoAttr :: Fetchable Repo -> AttrName
repoAttr NotFetched = notFetchedAttr
repoAttr Fetching = fetchingAttr
repoAttr (Errored {}) = erroredAttr
repoAttr (Fetched {}) = normalAttr

fetchableStatsBox :: Fetchable Repo -> Widget n
fetchableStatsBox (Fetched r) = statsBox r
fetchableStatsBox _ = str " "

getUnfoldWigets :: MainListElem -> [Widget n]
getUnfoldWigets (MainListElemHeading {}) = [
  str "HI I'M THE HEADING INFO"
  ]
getUnfoldWigets (MainListElemRepo {_workflows=NotFetched}) = [
  hBox [str "Workflows not fetched."]
  ]
getUnfoldWigets (MainListElemRepo {_workflows=Fetching}) = [
  hBox [str "Fetching workflows..."]
  ]
getUnfoldWigets (MainListElemRepo {_workflows=(Errored msg)}) = [
  hBox [str [i|Failed to fetch workflows: #{msg}|]]
  ]

getUnfoldWigets (MainListElemRepo {_workflows=(Fetched (WithTotalCount items count))}) = [
  borderWithLabel (padLeftRight 1 $ str [i|Workflows (#{count})|])
                  (vBox $ toList $ fmap workflowWidget (toList items))
  ]

borderWithCounts :: AppState -> Widget n
borderWithCounts (AppState {_appUser=(User {userLogin=(N name), ..})}) = hBorderWithLabel $ padLeftRight 1 $ hBox [str [i|#{name} (#{userPublicRepos} public repos, #{userFollowers} followers)|]]

infoBar :: AppState -> Widget n
infoBar s = Widget Greedy Fixed $ do
  _c <- getContext
  case listSelectedElement (s ^. appMainList) of
    Nothing -> render $ hBox [str ""]
    Just (_, MainListElemHeading {}) -> render $ hBox [str ""]

    Just (_, MainListElemRepo {_repo=(Fetched r)}) -> render $ hBox [str (T.unpack (T.intercalate ", " phrases))]
      where
        issuesPhrase = case repoOpenIssuesCount r of
          0 -> Nothing
          1 -> Just [i|1 issue|]
          n -> Just [i|#{n} issues|]

        -- prsPhrase = case ? r of
        --   0 -> Nothing
        --   1 -> Just [i|1 PR|]
        --   n -> Just [i|#{n} PRs|]

        phrases = catMaybes [issuesPhrase]
    Just (_, MainListElemRepo {_repo=_}) -> render $ hBox [str ""]

renderHeadingName :: Text -> Fetchable () -> Widget n
renderHeadingName l _stat = hBox [
  str (toString l)
  ]

statsBox :: Repo -> Widget n
statsBox (Repo {..}) = hBox $ catMaybes [
  guarding (repoWatchersCount > 0) $ padLeft (Pad 1) (
      padRight (Pad 2) (withAttr iconAttr (str "👁️"))
      <+> str (show repoWatchersCount)
      )

  , guarding (repoForksCount > 0) $ padLeft (Pad 1) (
      padRight (Pad 1) (withAttr iconAttr (str "⑂"))
      <+> str (show repoForksCount)
      )

  , guarding (repoStargazersCount > 0) $ padLeft (Pad 1) (
      padRight (Pad 1) (withAttr iconAttr (str "★"))
      <+> str (show repoStargazersCount)
      )
  ]

guarding :: (Monad m, Alternative m) => Bool -> b -> m b
guarding p widget = do
  guard p
  return widget

progressThing :: String
progressThing = "⠀"
