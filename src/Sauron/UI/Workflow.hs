{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE DataKinds #-}

module Sauron.UI.Workflow (
  workflowLine
  , workflowStatusToIcon
  , workflowInner
  ) where

import Brick
import Data.String.Interpolate
import Data.Time.Clock
import qualified Data.Vector as V
import GitHub
import Relude
import Sauron.Types
import Sauron.UI.AttrMap
import Sauron.UI.Statuses (statusToIconAnimated, fetchableQuarterCircleSpinner)
import Sauron.UI.Util.TimeDiff


-- WorkflowRun {workflowRunWorkflowRunId = Id 7403805672, workflowRunName = N "ci", workflowRunHeadBranch = migrate-debug, workflowRunHeadSha = "1367fa30fc409d198e18afa95bda04d26387925e", workflowRunPath = ".github/workflows/ci.yml", workflowRunDisplayTitle = More database stuff noci, workflowRunRunNumber = 2208, workflowRunEvent = "push", workflowRunStatus = "completed", workflowRunConclusion = Just skipped, workflowRunWorkflowId = 6848152, workflowRunUrl = URL https://api.github.com/repos/codedownio/codedown/actions/runs/7403805672, workflowRunHtmlUrl = URL https://github.com/codedownio/codedown/actions/runs/7403805672, workflowRunCreatedAt = 2024-01-04 00:10:06 UTC, workflowRunUpdatedAt = 2024-01-04 00:10:10 UTC, workflowRunActor = SimpleUser simpleUserId = Id 1634990, simpleUserLogin = N thomasjm, simpleUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/1634990?v=4", simpleUserUrl = URL "https://api.github.com/users/thomasjm", workflowRunAttempt = 1, workflowRunStartedAt = 2024-01-04 00:10:06 UTC}

workflowLine :: Int -> Bool -> WorkflowRun -> Fetchable a -> Widget n
workflowLine animationCounter toggled (WorkflowRun {..}) fetchableState = vBox [line1, line2]
  where
    runTime = diffUTCTime workflowRunUpdatedAt workflowRunStartedAt

    line1 = hBox [
      withAttr openMarkerAttr $ str (if toggled then "[-] " else "[+] ")
      , withAttr normalAttr $ str $ toString workflowRunDisplayTitle
      , padLeft (Pad 1) $ statusToIconAnimated animationCounter $ fromMaybe workflowRunStatus workflowRunConclusion
      , fetchableQuarterCircleSpinner animationCounter fetchableState
      , padLeft Max $ hBox [
          withAttr branchAttr $ str (toString workflowRunHeadBranch)
          , str " "
          , str [i|#{timeDiff runTime}|]
        ]
      ]

    line2 = padRight Max $ padLeft (Pad 4) $ hBox [
      withAttr boldText $ str $ toString $ untagName workflowRunName
      , str " "
      , withAttr hashAttr $ str "#"
      , withAttr hashNumberAttr $ str $ show workflowRunRunNumber
      , str ": Commit "
      , withAttr hashAttr $ str $ take 7 $ toString workflowRunHeadSha
      , str " pushed by "
      , withAttr usernameAttr $ str $ toString $ untagName $ simpleUserLogin workflowRunActor
      ]



workflowStatusToIcon :: WorkflowStatus -> Widget n
workflowStatusToIcon WorkflowSuccess = greenCheck
workflowStatusToIcon WorkflowPending = ellipses
workflowStatusToIcon WorkflowFailed = redX
workflowStatusToIcon WorkflowCancelled = cancelled
workflowStatusToIcon WorkflowNeutral = neutral
workflowStatusToIcon WorkflowUnknown = unknown

cancelled = withAttr cancelledAttr (str "⊘")
greenCheck = withAttr greenCheckAttr (str "✓")
redX = withAttr redXAttr (str "✗")
ellipses = withAttr ellipsesAttr (str "⋯")
neutral = withAttr neutralAttr (str "-")
unknown = withAttr unknownAttr (str "?")


workflowInner :: WorkflowRun -> Fetchable (NodeState SingleWorkflowT) -> Widget n
workflowInner (WorkflowRun {..}) jobsFetchable = vBox $ workflowDetails
  where
    runTime = diffUTCTime workflowRunUpdatedAt workflowRunStartedAt

    workflowDetails = [
      withAttr normalAttr $ strWrap $ toString workflowRunDisplayTitle

      , padTop (Pad 1) $ hBox [
          str "File: "
          , withAttr hashAttr $ str $ toString workflowRunPath
          , str " • Event: "
          , withAttr branchAttr $ str $ toString workflowRunEvent
        ]

      , hBox [
          str "Triggered by "
          , withAttr usernameAttr $ str $ toString $ untagName $ simpleUserLogin workflowRunActor
          , str [i| on #{workflowRunCreatedAt}|]
        ]

      , hBox [
          str "Run #"
          , withAttr hashNumberAttr $ str $ show workflowRunRunNumber
          , str " • Attempt "
          , str $ show workflowRunAttempt
          , str " • Duration: "
          , str [i|#{timeDiff runTime}|]
        ]

      , hBox [
          str "Commit: "
          , withAttr hashAttr $ str $ take 7 $ toString workflowRunHeadSha
          , str " on "
          , withAttr branchAttr $ str $ toString workflowRunHeadBranch
        ]
      ]
