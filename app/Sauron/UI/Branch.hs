{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sauron.UI.Branch () where

import Brick
import Data.String.Interpolate
import qualified Data.Vector as V
import GitHub
import Relude
import Sauron.Types
import Sauron.UI.AttrMap
import Sauron.UI.Statuses (fetchableQuarterCircleSpinner)


instance ListDrawable Fixed 'SingleBranchT where
  drawLine appState (EntityData {_static=branch, _state, ..}) =
    simpleBranchLine _toggled branch appState _state

  drawInner _ _ = Nothing

simpleBranchLine :: Bool -> Branch -> AppState -> Fetchable (V.Vector Commit) -> Widget n
simpleBranchLine toggled' (Branch {branchName, branchCommit}) appState fetchableState = vBox [line1, line2]
  where
    line1 = hBox [
      withAttr openMarkerAttr $ str (if toggled' then "[-] " else "[+] ")
      , hLimitPercent 40 $ withAttr branchAttr $
          padRight Max $ txt branchName
      , fetchableQuarterCircleSpinner (_appAnimationCounter appState) fetchableState
      , padLeft Max $ case fetchableState of
          Fetched commits -> str [i|(#{V.length commits} commits)|]
          _ -> withAttr normalAttr $ str "branch"
      ]

    line2 = padRight Max $ padLeft (Pad 4) $ hBox [
      str "Latest commit "
      , withAttr hashAttr $ str $ take 7 $ toString $ branchCommitSha branchCommit
      ]
