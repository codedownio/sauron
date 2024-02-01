{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Sauron.UI.Draw.WorkflowLine (
  workflowWidget
  ) where

import Brick
import GitHub hiding (Status)
import Relude
import Sauron.UI.AttrMap


-- WorkflowRun {workflowRunWorkflowRunId = Id 7403805672, workflowRunName = N "ci", workflowRunHeadBranch = migrate-debug, workflowRunHeadSha = "1367fa30fc409d198e18afa95bda04d26387925e", workflowRunPath = ".github/workflows/ci.yml", workflowRunDisplayTitle = More database stuff noci, workflowRunRunNumber = 2208, workflowRunEvent = "push", workflowRunStatus = "completed", workflowRunConclusion = Just skipped, workflowRunWorkflowId = 6848152, workflowRunUrl = URL https://api.github.com/repos/codedownio/codedown/actions/runs/7403805672, workflowRunHtmlUrl = URL https://github.com/codedownio/codedown/actions/runs/7403805672, workflowRunCreatedAt = 2024-01-04 00:10:06 UTC, workflowRunUpdatedAt = 2024-01-04 00:10:10 UTC, workflowRunActor = SimpleUser simpleUserId = Id 1634990, simpleUserLogin = N thomasjm, simpleUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/1634990?v=4", simpleUserUrl = URL "https://api.github.com/users/thomasjm", workflowRunAttempt = 1, workflowRunStartedAt = 2024-01-04 00:10:06 UTC}

workflowWidget :: WorkflowRun -> Widget n
workflowWidget (WorkflowRun {..}) = hBox [
  str ("#" <> show workflowRunRunNumber <> " ")
  , withAttr normalAttr $ str $ toString $ untagName workflowRunName
  , str ": "
  , str $ toString workflowRunDisplayTitle
  , padLeft (Pad 1) $ getWorkflowIcon workflowRunStatus workflowRunConclusion
  ]

getWorkflowIcon _ (Just conclusion) = getIcon conclusion
getWorkflowIcon status Nothing = getIcon status


getIcon :: Text -> Widget n
getIcon "completed" = greenCheck
getIcon "action_required" = ellipses
getIcon "cancelled" = cancelled
getIcon "failure" = redX
getIcon "neutral" = neutral
getIcon "skipped" = cancelled
getIcon "stale" = neutral
getIcon "success" = greenCheck
getIcon "timed_out" = redX
getIcon "in_progress" = ellipses
getIcon "queued" = ellipses
getIcon "requested" = ellipses
getIcon "waiting" = ellipses
getIcon "pending" = ellipses
getIcon _ = unknown

cancelled = withAttr cancelledAttr (str "⃠")
greenCheck = withAttr greenCheckAttr (str "✓")
redX = withAttr redXAttr (str "✗")
ellipses = withAttr ellipsesAttr (str "⋯")
neutral = withAttr neutralAttr (str "-")
unknown = withAttr unknownAttr (str "?")
