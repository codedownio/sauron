{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Sauron.UI.Workflow (
  workflowLine
  , workflowStatusToIcon

  , workflowInner
  ) where

import Brick
import GitHub
import Relude
import Sauron.Types hiding (toggled)
import Sauron.UI.AttrMap


-- WorkflowRun {workflowRunWorkflowRunId = Id 7403805672, workflowRunName = N "ci", workflowRunHeadBranch = migrate-debug, workflowRunHeadSha = "1367fa30fc409d198e18afa95bda04d26387925e", workflowRunPath = ".github/workflows/ci.yml", workflowRunDisplayTitle = More database stuff noci, workflowRunRunNumber = 2208, workflowRunEvent = "push", workflowRunStatus = "completed", workflowRunConclusion = Just skipped, workflowRunWorkflowId = 6848152, workflowRunUrl = URL https://api.github.com/repos/codedownio/codedown/actions/runs/7403805672, workflowRunHtmlUrl = URL https://github.com/codedownio/codedown/actions/runs/7403805672, workflowRunCreatedAt = 2024-01-04 00:10:06 UTC, workflowRunUpdatedAt = 2024-01-04 00:10:10 UTC, workflowRunActor = SimpleUser simpleUserId = Id 1634990, simpleUserLogin = N thomasjm, simpleUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/1634990?v=4", simpleUserUrl = URL "https://api.github.com/users/thomasjm", workflowRunAttempt = 1, workflowRunStartedAt = 2024-01-04 00:10:06 UTC}

workflowLine :: Bool -> WorkflowRun -> Widget n
workflowLine toggled (WorkflowRun {..}) = hBox [
  withAttr openMarkerAttr $ str (if toggled then "[-] " else "[+] ")
  , withAttr hashAttr $ str "#"
  , withAttr hashNumberAttr $ str $ show workflowRunRunNumber
  , str " "
  , withAttr normalAttr $ str $ toString $ untagName workflowRunName
  , str ": "
  , str $ toString workflowRunDisplayTitle
  , padLeft (Pad 1) $ getIcon $ fromMaybe workflowRunStatus workflowRunConclusion
  , padLeft Max (str " ")
  ]

workflowStatusToIcon :: WorkflowStatus -> Widget n
workflowStatusToIcon WorkflowSuccess = greenCheck
workflowStatusToIcon WorkflowPending = ellipses
workflowStatusToIcon WorkflowFailed = redX
workflowStatusToIcon WorkflowCancelled = cancelled
workflowStatusToIcon WorkflowNeutral = neutral
workflowStatusToIcon WorkflowUnknown = unknown

getIcon :: Text -> Widget n
getIcon = workflowStatusToIcon . chooseWorkflowStatus

cancelled = withAttr cancelledAttr (str "⊘")
greenCheck = withAttr greenCheckAttr (str "✓")
redX = withAttr redXAttr (str "✗")
ellipses = withAttr ellipsesAttr (str "⋯")
neutral = withAttr neutralAttr (str "-")
unknown = withAttr unknownAttr (str "?")

workflowInner :: WorkflowRun -> Widget n
workflowInner (WorkflowRun {..}) = vBox [
  strWrap $ toString workflowRunDisplayTitle
  ]
