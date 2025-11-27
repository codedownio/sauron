{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sauron.UI.Branch (
  branchLine
  ) where

import Brick
import Data.String.Interpolate
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, diffUTCTime)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import qualified Data.Vector as V
import GitHub
import Relude
import qualified Sauron.GraphQL as GraphQL
import Sauron.Types
import Sauron.UI.AttrMap
import Sauron.UI.Statuses (fetchableQuarterCircleSpinner)
import Sauron.UI.Util.TimeDiff (timeFromNow)


instance ListDrawable Fixed 'SingleBranchT where
  drawLine appState (EntityData {_static=branch, _state, ..}) =
    branchLine _toggled branch Nothing appState _state

  drawInner _ _ = Nothing

instance ListDrawable Fixed 'SingleBranchWithInfoT where
  drawLine appState (EntityData {_static=branchInfo, _state, ..}) =
    let branchCommit = BranchCommit (fromMaybe "" (branchWithInfoCommitOid branchInfo)) 
                                   (URL "")  -- placeholder URL
        branch = Branch (branchWithInfoBranchName branchInfo) branchCommit
    in branchLine _toggled branch (Just branchInfo) appState _state

  drawInner _ _ = Nothing

branchLine :: Bool -> Branch -> Maybe BranchWithInfo -> AppState -> Fetchable (V.Vector Commit) -> Widget n
branchLine toggled' (Branch {branchName, branchCommit}) maybeBranchData appState fetchableState = vBox [line1, line2]
  where

    line1 = hBox [
      withAttr openMarkerAttr $ str (if toggled' then "[-] " else "[+] ")
      , hLimitPercent 30 $ withAttr branchAttr $
          padRight Max $ txt branchName
      , hLimitPercent 15 $
          padRight Max $ str $ formatCommitTime maybeBranchData
      , hLimitPercent 12 $
          padRight Max $ str $ formatCheckStatus maybeBranchData
      , hLimitPercent 10 $
          padRight Max $ str "↑0 ↓0"  -- Placeholder for ahead/behind
      , hLimitPercent 15 $
          padRight Max $ str $ formatPRInfo maybeBranchData
      , fetchableQuarterCircleSpinner (_appAnimationCounter appState) fetchableState
      , padLeft Max $ case fetchableState of
          Fetched commits -> str [i|(#{V.length commits} commits)|]
          _ -> withAttr normalAttr $ str "branch"
      ]

    line2 = padRight Max $ padLeft (Pad 4) $ hBox [
      str "Latest commit "
      , withAttr hashAttr $ str $ take 7 $ toString $ branchCommitSha branchCommit
      ]

    -- Helper function to format commit time from GraphQL data using timeFromNow
    formatCommitTime :: Maybe BranchWithInfo -> String
    formatCommitTime Nothing = "Unknown"
    formatCommitTime (Just branchData) =
      case branchWithInfoCommitDate branchData of
        Nothing -> "Unknown"
        Just dateStr ->
          case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (toString dateStr) :: Maybe UTCTime of
            Nothing -> toString $ T.take 10 dateStr  -- Fallback to raw date
            Just commitTime -> timeFromNow (diffUTCTime (_appNow appState) commitTime)

    -- Helper function to format check status
    formatCheckStatus :: Maybe BranchWithInfo -> String
    formatCheckStatus Nothing = "No checks"
    formatCheckStatus (Just branchData) =
      case branchWithInfoCheckStatus branchData of
        Nothing -> "No checks"
        Just "SUCCESS" -> "✓ Checks"
        Just "FAILURE" -> "✗ Failed"
        Just "PENDING" -> "⏳ Running"
        Just status -> toString status

    -- Helper function to format PR info
    formatPRInfo :: Maybe BranchWithInfo -> String
    formatPRInfo Nothing = "No PR"
    formatPRInfo (Just branchData) =
      case branchWithInfoAssociatedPR branchData of
        Nothing -> "No PR"
        Just pr -> case GraphQL.prNumber pr of
          Nothing -> "PR: Unknown"
          Just num -> "PR #" <> show num
