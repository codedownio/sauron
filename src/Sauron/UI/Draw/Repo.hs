
module Sauron.UI.Draw.Repo (
  renderRepoLine
  ) where

import Brick
import qualified Data.List as L
import Data.Maybe
import GitHub hiding (Status)
import Relude
import Sauron.Types
import Sauron.UI.AttrMap
import Sauron.UI.Draw.Util
import Sauron.UI.Draw.WorkflowLine


renderRepoLine :: Bool -> (Name Owner, Name Repo) -> Fetchable Repo -> Fetchable HealthCheckResult -> Widget n
renderRepoLine isToggled (owner, name) fetchableRepo fetchableHealthCheck = hBox $ catMaybes [
  Just $ withAttr openMarkerAttr $ str (if isToggled then "[-] " else "[+] ")
  , Just $ hBox [
      withAttr attr (str (toString (untagName owner)))
      , withAttr toggleMarkerAttr $ str " / "
      , withAttr attr (str (toString (untagName name)))
      ]
  , healthIndicator fetchableHealthCheck
  , Just (padLeft Max (fetchableStatsBox fetchableRepo))
  ]
  where
    attr = repoAttr fetchableRepo

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

statsBox :: Repo -> Widget n
statsBox (Repo {..}) = if L.null items then str " " else hBox items
  where
    items = catMaybes [
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
