{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sauron.UI.Repo (
  renderRepoLine
  ) where

import Brick
import qualified Data.List as L
import Data.Maybe
import GitHub hiding (Status)
import Relude
import Sauron.Types
import Sauron.UI.AttrMap
import Sauron.UI.Statuses
import Sauron.UI.Util


instance ListDrawable Fixed 'RepoT where
  drawLine appState (EntityData {_static=(owner, name), ..}) =
    renderRepoLine _toggled (owner, name) _state _healthCheck (_appAnimationCounter appState)

  drawInner _ _ = Nothing

renderRepoLine :: Bool -> (Name Owner, Name Repo) -> Fetchable Repo -> Fetchable HealthCheckResult -> Int -> Widget n
renderRepoLine isToggled (owner, name) fetchableRepo fetchableHealthCheck animationCounter = hBox $ catMaybes [
  Just $ withAttr openMarkerAttr $ str (if isToggled then "[-] " else "[+] ")
  , Just $ hBox [
      withAttr attr (str (toString (untagName owner)))
      , withAttr toggleMarkerAttr $ str " / "
      , withAttr attr (str (toString (untagName name)))
      ]
  , Just $ fetchableQuarterCircleSpinner animationCounter fetchableRepo
  , healthIndicator fetchableHealthCheck animationCounter
  , Just (padLeft Max (fetchableStatsBox fetchableRepo))
  ]
  where
    attr = repoAttr fetchableRepo

healthIndicator :: Fetchable HealthCheckResult -> Int -> Maybe (Widget n)
healthIndicator (Fetched (HealthCheckWorkflowResult ws)) animationCounter = Just (padLeft (Pad 1) (statusToIconAnimated animationCounter ws))
healthIndicator _ _ = Nothing

repoAttr :: Fetchable Repo -> AttrName
repoAttr NotFetched = notFetchedAttr
repoAttr (Fetching {}) = fetchingAttr
repoAttr (Errored {}) = erroredAttr
repoAttr (Fetched {}) = normalAttr

fetchableStatsBox :: Fetchable Repo -> Widget n
fetchableStatsBox (Fetched r) = statsBox r
fetchableStatsBox _ = str " "

statsBox :: Repo -> Widget n
statsBox (Repo {..}) = if L.null cells then str " " else hBox cells
  where
    cells = catMaybes [
      guardJust repoSubscribersCount $ \subscribersCount -> guarding (subscribersCount > 0) $ padLeft (Pad 1) (
          padRight (Pad 1) (withAttr iconAttr (str "◊"))
          <+> str (show subscribersCount)
          )

      , guarding (repoForksCount > 0) $ padLeft (Pad 1) (
          padRight (Pad 1) (withAttr iconAttr (str "⑂"))
          <+> str (show repoForksCount)
          )

      , guarding (repoStargazersCount > 0) $ padLeft (Pad 1) (
          padRight (Pad 1) (withAttr starsAttr (str "★"))
          <+> str (show repoStargazersCount)
          )
      ]
