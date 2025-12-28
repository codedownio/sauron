{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sauron.UI.Workflow (
  workflowLine
  , workflowInner
  ) where

import Brick
import Control.Monad
import Data.String.Interpolate
import Data.Time.Clock
import GitHub
import Relude
import Sauron.HealthCheck.Stop (healthCheckIndicatorWidget)
import Sauron.Types
import Sauron.UI.AttrMap
import Sauron.UI.Statuses
import Sauron.UI.Util
import Sauron.UI.Util.TimeDiff
import UnliftIO.Async (Async)


instance ListDrawable Fixed 'SingleWorkflowT where
  drawLine appState (EntityData {_static=wf, ..}) =
    workflowLine (_appAnimationCounter appState) (_appNow appState) _toggled wf _state _healthCheckThread

  drawInner _appState (EntityData {_static=wf, _state, _ident, ..}) = do
    guard _toggled
    guardFetchedOrHasPrevious _state $ \_ ->
      return $ workflowInner wf _state

-- WorkflowRun {workflowRunWorkflowRunId = Id 7403805672, workflowRunName = N "ci", workflowRunHeadBranch = migrate-debug, workflowRunHeadSha = "1367fa30fc409d198e18afa95bda04d26387925e", workflowRunPath = ".github/workflows/ci.yml", workflowRunDisplayTitle = More database stuff noci, workflowRunRunNumber = 2208, workflowRunEvent = "push", workflowRunStatus = "completed", workflowRunConclusion = Just skipped, workflowRunWorkflowId = 6848152, workflowRunUrl = URL https://api.github.com/repos/codedownio/codedown/actions/runs/7403805672, workflowRunHtmlUrl = URL https://github.com/codedownio/codedown/actions/runs/7403805672, workflowRunCreatedAt = 2024-01-04 00:10:06 UTC, workflowRunUpdatedAt = 2024-01-04 00:10:10 UTC, workflowRunActor = SimpleUser simpleUserId = Id 1634990, simpleUserLogin = N thomasjm, simpleUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/1634990?v=4", simpleUserUrl = URL "https://api.github.com/users/thomasjm", workflowRunAttempt = 1, workflowRunStartedAt = 2024-01-04 00:10:06 UTC}

workflowLine :: Int -> UTCTime -> Bool -> WorkflowRun -> Fetchable a -> Maybe (Async (), Int) -> Widget n
workflowLine animationCounter currentTime toggled' (WorkflowRun {..}) fetchableState healthCheckThreadData = vBox [line1, line2]
  where
    runTime = diffUTCTime workflowRunUpdatedAt workflowRunStartedAt
    timeSinceStart = diffUTCTime currentTime workflowRunStartedAt

    line1 = hBox [
      withAttr openMarkerAttr $ str (if toggled' then "[-] " else "[+] ")
      , withAttr normalAttr $ str $ toString workflowRunDisplayTitle
      , padLeft (Pad 1) $ statusToIconAnimated animationCounter $ chooseWorkflowStatus $ fromMaybe workflowRunStatus workflowRunConclusion
      , fetchableQuarterCircleSpinner animationCounter fetchableState
      , healthCheckIndicatorWidget healthCheckThreadData
      , padLeft Max $ hBox [
          str [i|#{timeDiff runTime}|]
          , withAttr toggleMarkerAttr $ str " / "
          , str [i|#{timeFromNow timeSinceStart}|]
        ]
      ]

    line2 = padRight Max $ padLeft (Pad 4) $ hBox $ [
      withAttr boldText $ str $ toString $ untagName workflowRunName
      , str " "
      , withAttr hashAttr $ str "#"
      , withAttr hashNumberAttr $ str $ show workflowRunRunNumber
      , str ": Commit "
      , withAttr hashAttr $ str $ take 7 $ toString workflowRunHeadSha
      , str " on "
      , withAttr branchAttr $ str $ toString workflowRunHeadBranch
      , str " • Pushed by "
      , withAttr usernameAttr $ str $ toString $ untagName $ simpleUserLogin workflowRunActor
      ] <> (if workflowRunAttempt > 1 then [str [i| • Attempt #{workflowRunAttempt}|]] else [])

workflowInner :: WorkflowRun -> Fetchable (NodeState SingleWorkflowT) -> Widget n
workflowInner (WorkflowRun {..}) _jobsFetchable = vBox $ workflowDetails
  where
    workflowDetails = [
      hBox [
          str "File: "
          , withAttr hashAttr $ str $ toString workflowRunPath
        ]
      ]
