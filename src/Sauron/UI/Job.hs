{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Sauron.UI.Job (
  jobLine
  , jobInner
  ) where

import Brick
import GitHub
import Relude
import Sauron.Types hiding (toggled)
import Sauron.UI.AttrMap
import Sauron.UI.Workflow (statusToIcon)

jobLine :: Bool -> Job -> Widget n
jobLine toggled (Job {..}) = vBox [line1, line2]
  where
    line1 = hBox [
      withAttr openMarkerAttr $ str (if toggled then "[-] " else "[+] ")
      , withAttr normalAttr $ str $ toString $ untagName jobName
      , padLeft (Pad 1) $ statusToIcon $ fromMaybe jobStatus jobConclusion
      , padLeft Max $ str "2m 34s"  -- Placeholder duration
      ]

    line2 = padRight Max $ padLeft (Pad 4) $ hBox $ catMaybes [
      runnerNameWidget jobRunnerName
      ]

runnerNameWidget :: Maybe Text -> Maybe (Widget n)
runnerNameWidget (Just name) = Just $ hBox [
  str "Runner: "
  , withAttr normalAttr $ str (toString name)
  ]
runnerNameWidget Nothing = Nothing

jobInner :: Job -> Fetchable NodeState -> Widget n
jobInner job _jobInner = vBox [
  withAttr normalAttr $ str $ show job
  , padTop (Pad 1) $ str "Job details would go here"
  ]
