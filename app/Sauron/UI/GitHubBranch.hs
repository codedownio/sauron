{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sauron.UI.GitHubBranch () where

import Brick
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, diffUTCTime)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Relude
import Sauron.Types
import Sauron.Types.Branches
import Sauron.UI.AttrMap
import Sauron.UI.Util.TimeDiff (timeFromNow)


instance ListDrawable Fixed 'GitHubBranchT where
  drawLine appState (EntityData {_static=branchInfo, ..}) =
    gitHubBranchLine _toggled branchInfo appState

  drawInner _ _ = Nothing

gitHubBranchLine :: Bool -> GitHubBranchInfo -> AppState -> Widget n
gitHubBranchLine toggled' branchInfo appState = vBox [line1, line2]
  where
    line1 = hBox [
      withAttr openMarkerAttr $ str (if toggled' then "[-] " else "[+] ")
      , withAttr branchAttr $ txt (gitHubBranchInfoName branchInfo)
      , padLeft Max infoColumns
      ]

    infoColumns = hBox [
      txt $ formatCommitTimeText (_appNow appState) branchInfo
      , str "   "
      , if gitHubBranchInfoIsDefault branchInfo
        then withAttr greenCheckAttr $ str "DEFAULT"
        else str ""
      , str "   "
      , txt $ gitHubBranchAuthorLogin (gitHubBranchInfoAuthor branchInfo)
      ]

    line2 = padRight Max $ padLeft (Pad 4) $ hBox [
      str "Author: "
      , txt $ gitHubBranchAuthorName (gitHubBranchInfoAuthor branchInfo)
      ]

formatCommitTimeText :: UTCTime -> GitHubBranchInfo -> Text
formatCommitTimeText currentTime branchInfo =
  case parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" (toString $ gitHubBranchInfoAuthoredDate branchInfo) :: Maybe UTCTime of
    Nothing -> T.take 10 (gitHubBranchInfoAuthoredDate branchInfo)  -- Fallback to raw date
    Just commitTime -> toText $ timeFromNow (diffUTCTime currentTime commitTime)