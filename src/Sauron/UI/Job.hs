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
import Sauron.Types (JobLogGroup(..))
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
      do
        guard toggled
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

jobInner :: Int -> Job -> Maybe [JobLogGroup] -> Widget n
jobInner animationCounter (Job {..}) maybeJobLogs = vBox [
  vBox $ map renderJobStep (V.toList jobSteps)
  -- , case maybeJobLogs of
  --     Nothing -> str "No logs"
  --     Just xs -> vBox (map renderJobLogGroup xs)
  ]
  where
    renderJobStep :: JobStep -> Widget n
    renderJobStep (JobStep {..}) = hBox [
      withAttr normalAttr $ str $ toString $ untagName jobStepName
      , padLeft (Pad 1) $ statusToIconAnimated animationCounter $ fromMaybe jobStepStatus jobStepConclusion
      ]

    renderJobLogGroup :: JobLogGroup -> Widget n
    renderJobLogGroup (JobLogLine _timestamp content) = str $ toString content
    renderJobLogGroup (JobLogGroup _timestamp title children) = vBox [
      withAttr normalAttr $ str $ "Group: " <> toString title,
      padLeft (Pad 2) $ vBox (map renderJobLogGroup children)
      ]
