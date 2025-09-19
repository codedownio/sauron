{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Sauron.UI.Job (
  jobLine
  , jobInner
  ) where

import Brick
import Data.Time.Clock
import qualified Data.Vector as V
import GitHub
import Relude
import Sauron.UI.AttrMap
import Sauron.UI.Util.TimeDiff
import Sauron.UI.Workflow (statusToIconAnimated)

jobLine :: Int -> Bool -> Job -> Widget n
jobLine animationCounter toggled (Job {..}) = vBox [line1, line2]
  where
    line1 = hBox [
      withAttr openMarkerAttr $ str (if toggled then "[-] " else "[+] ")
      , withAttr normalAttr $ str $ toString $ untagName jobName
      , padLeft (Pad 1) $ statusToIconAnimated animationCounter $ fromMaybe jobStatus jobConclusion
      , padLeft Max $ str $ calculateDuration jobStartedAt jobCompletedAt
      ]

    line2 = padRight Max $ padLeft (Pad 4) $ hBox $ catMaybes [
      runnerNameWidget jobRunnerName
      ]

    calculateDuration :: UTCTime -> Maybe UTCTime -> String
    calculateDuration started (Just completed) = timeDiff $ diffUTCTime completed started
    calculateDuration _ Nothing = "running"

    runnerNameWidget :: Maybe Text -> Maybe (Widget n)
    runnerNameWidget (Just name) = Just $ hBox [
      str "Runner: "
      , withAttr normalAttr $ str (toString name)
      ]
    runnerNameWidget Nothing = Nothing

jobInner :: Int -> Job -> Widget n
jobInner animationCounter (Job {..}) = vBox [
  if V.null jobSteps
    then str "No steps available"
    else vBox $ map renderJobStep (V.toList jobSteps)
  ]
  where
    renderJobStep :: JobStep -> Widget n
    renderJobStep (JobStep {..}) = hBox [
      withAttr normalAttr $ str $ toString $ untagName jobStepName
      , padLeft (Pad 1) $ statusToIconAnimated animationCounter $ fromMaybe jobStepStatus jobStepConclusion
      ]
