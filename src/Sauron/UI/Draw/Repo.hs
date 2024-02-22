
module Sauron.UI.Draw.Repo (
  renderRepoLine
  ) where

import Brick
import Data.Maybe
import GitHub hiding (Status)
import Relude
import Sauron.Types
import Sauron.UI.AttrMap
import Sauron.UI.Draw.WorkflowLine


renderRepoLine :: Bool -> (Name Owner, Name Repo) -> AttrName -> Fetchable HealthCheckResult -> Widget n -> Widget n
renderRepoLine isToggled (owner, name) attr fetchableHealthCheck rightSide = hBox $ catMaybes [
  Just $ withAttr openMarkerAttr $ str (if isToggled then "[-] " else "[+] ")
  , Just $ hBox [
      withAttr attr (str (toString (untagName owner)))
      , withAttr toggleMarkerAttr $ str " / "
      , withAttr attr (str (toString (untagName name)))
      ]
  , healthIndicator fetchableHealthCheck
  , Just (padLeft Max rightSide)
  ]

healthIndicator :: Fetchable HealthCheckResult -> Maybe (Widget n)
healthIndicator (Fetched (HealthCheckWorkflowResult ws)) = Just (padLeft (Pad 1) (workflowStatusToIcon ws))
healthIndicator _ = Nothing
