{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sauron.UI.BranchWithInfo () where

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


instance ListDrawable Fixed 'SingleBranchWithInfoT where
  drawLine appState (EntityData {_static=branchInfo, _state, ..}) =
    let branchCommit = BranchCommit (fromMaybe "" (branchWithInfoCommitOid branchInfo))
                                   (URL "")  -- placeholder URL
        branch = Branch (branchWithInfoBranchName branchInfo) branchCommit
    in branchLineWithInfo _toggled branch branchInfo appState _state

  drawInner _ _ = Nothing

branchLineWithInfo :: Bool -> Branch -> BranchWithInfo -> AppState -> Fetchable (V.Vector Commit) -> Widget n
branchLineWithInfo toggled' (Branch {branchName, branchCommit}) branchData appState fetchableState = vBox [line1, line2]
  where

    line1 = hBox [
      withAttr openMarkerAttr $ str (if toggled' then "[-] " else "[+] ")
      , hLimitPercent 30 $ withAttr branchAttr $
          padRight Max $ txt branchName
      , hLimitPercent 15 $
          padRight Max $ str $ formatCommitTime branchData
      , hLimitPercent 12 $
          padRight Max $ formatCheckStatus branchData
      , hLimitPercent 10 $
          padRight Max $ formatAheadBehind branchData
      , hLimitPercent 15 $
          padRight Max $ str $ formatPRInfo branchData
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
    formatCommitTime :: BranchWithInfo -> String
    formatCommitTime branchInfo =
      case branchWithInfoCommitDate branchInfo of
        Nothing -> "Unknown"
        Just dateStr ->
          case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (toString dateStr) :: Maybe UTCTime of
            Nothing -> toString $ T.take 10 dateStr  -- Fallback to raw date
            Just commitTime -> timeFromNow (diffUTCTime (_appNow appState) commitTime)

    -- Helper function to format check status with colors
    formatCheckStatus :: BranchWithInfo -> Widget n
    formatCheckStatus branchInfo =
      case branchWithInfoCheckStatus branchInfo of
        Nothing -> str "No checks"
        Just "SUCCESS" -> hBox [withAttr greenCheckAttr $ str "✓", str " Checks"]
        Just "FAILURE" -> hBox [withAttr redXAttr $ str "✗", str " Failed"]
        Just "PENDING" -> hBox [withAttr queuedAttr $ str "⏳", str " Running"]
        Just status -> str $ toString status

    -- Helper function to format PR info
    formatPRInfo :: BranchWithInfo -> String
    formatPRInfo branchInfo =
      case branchWithInfoAssociatedPR branchInfo of
        Nothing -> "No PR"
        Just pr -> case GraphQL.prNumber pr of
          Nothing -> "PR: Unknown"
          Just num -> "PR #" <> show num

    -- Helper function to format ahead/behind counts with colors
    formatAheadBehind :: BranchWithInfo -> Widget n
    formatAheadBehind branchInfo =
      let ahead = fromMaybe 0 (branchWithInfoAheadBy branchInfo)
          behind = fromMaybe 0 (branchWithInfoBehindBy branchInfo)
          aheadWidget = if ahead > 0
                       then hBox [withAttr greenCheckAttr $ str "↑", str $ " " <> show ahead]
                       else str "↑ 0"
          behindWidget = if behind > 0
                        then hBox [withAttr queuedAttr $ str "↓", str $ " " <> show behind]
                        else str "↓ 0"
      in hBox [aheadWidget, str " ", behindWidget]
