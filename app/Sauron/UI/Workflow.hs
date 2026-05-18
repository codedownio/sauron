{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
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
import Lens.Micro
import Relude
import Sauron.Actions (refreshLine)
import Sauron.Actions.Util (findRepoParent, findWorkflowsParent)
import Sauron.Event.Helpers (withFixedElemAndParents)
import Sauron.HealthCheck.Stop (healthCheckIndicatorWidget)
import Sauron.Mutations.Workflow (cancelWorkflowRun)
import Sauron.Types
import Sauron.UI.AttrMap
import Sauron.UI.Keys
import Sauron.UI.Statuses
import Sauron.UI.Util
import Sauron.UI.Util.TimeDiff
import UnliftIO.Async (Async, async)


instance ListDrawable Fixed 'SingleWorkflowT where
  drawLine appState (EntityData {_static=wf, ..}) =
    workflowLine (_appAnimationCounter appState) (_appNow appState) _toggled wf _state _healthCheckThread

  drawInner _appState (EntityData {_static=wf, _state, _children, ..}) = do
    guard _toggled
    guardFetchedOrHasPrevious _state $ \_ ->
      return $ workflowInner wf _children

  getExtraTopBoxWidgets _app (EntityData {_static=wf}) =
    if isNothing (workflowRunConclusion wf)
    then [hBox [str "["
              , withAttr hotkeyAttr $ str $ showKey cancelWorkflowKey
              , str "] "
              , withAttr hotkeyMessageAttr $ str "Cancel workflow"
              ]
         ]
    else []

  handleHotkey s key (EntityData {_static=wf})
    | key == cancelWorkflowKey && isNothing (workflowRunConclusion wf) = do
        liftIO $ void $ async $ do
          withFixedElemAndParents s $ \_ _ parents -> do
            case findRepoParent parents of
              Just (RepoNode (EntityData {_static=(owner, name)})) -> do
                runReaderT (cancelWorkflowRun owner name (workflowRunWorkflowRunId wf)) (s ^. appBaseContext)
                whenJust (findWorkflowsParent parents) $ \workflowsNode ->
                  liftIO $ void $ refreshLine (s ^. appBaseContext) workflowsNode parents
              _ -> return ()
        return True
  handleHotkey _ _ _ = return False

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

workflowInner :: WorkflowRun -> [Node Fixed SingleJobT] -> Widget n
workflowInner (WorkflowRun {..}) jobs = vBox $ workflowDetails
  where
    jobStatuses = [chooseWorkflowStatus (fromMaybe (jobStatus j) (jobConclusion j))
                  | SingleJobNode (EntityData {_state=JobNodeState {jnsJob=Fetched j}}) <- jobs]
    succeeded = length [() | WorkflowSuccess <- jobStatuses]
    failed = length [() | WorkflowFailed <- jobStatuses]
    running = length [() | WorkflowRunning <- jobStatuses]
    notStarted = length [() | s <- jobStatuses, s `elem` [WorkflowPending, WorkflowNeutral, WorkflowUnknown]]
    cancelled = length [() | WorkflowCancelled <- jobStatuses]

    parts = catMaybes [
      ifPositive succeeded $ withAttr greenCheckAttr (str [i|#{succeeded}|]) <+> str " succeeded"
      , ifPositive failed $ withAttr redXAttr (str [i|#{failed}|]) <+> str " failed"
      , ifPositive running $ withAttr queuedAttr (str [i|#{running}|]) <+> str " running"
      , ifPositive cancelled $ withAttr cancelledAttr (str [i|#{cancelled}|]) <+> str " cancelled"
      , ifPositive notStarted $ withAttr queuedAttr (str [i|#{notStarted}|]) <+> str " not started"
      ]
    jobSummary = hBox $ intercalate [str " / "] [[w] | w <- parts]

    ifPositive n w
      | n > 0 = Just w
      | otherwise = Nothing

    workflowDetails = [
      hBox [
          str "File: "
          , withAttr hashAttr $ str $ toString workflowRunPath
        ]
      ] ++ [jobSummary | not (null jobStatuses)]
