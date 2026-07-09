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
import Brick.Widgets.Center (hCenter)
import Control.Monad
import Data.String.Interpolate
import Data.Time.Clock
import GitHub
import qualified Graphics.Vty as V
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
import Sauron.UI.Pagination (paginationInfo)
import Sauron.UI.Statuses
import Sauron.UI.Util
import Sauron.UI.Util.TimeDiff
import Sauron.Workflow.Sorting (computeJobPageInfo)
import UnliftIO.Async (Async, async)


instance ListDrawable Fixed 'SingleWorkflowT where
  drawLine appState (EntityData {_static=wf, ..}) =
    workflowLine (_appAnimationCounter appState) (_appNow appState) _toggled wf (workflowNodeStateFetchable _state) _healthCheckThread _state (length _children)

  drawInner _appState (EntityData {_static=wf, _state, _children, _toggled}) =
    guardFetchedOrHasPrevious (workflowNodeStateFetchable _state) $ \_ ->
      if _toggled
      then Just (workflowInner wf _children _state)
      -- When collapsed, still show the job summary for in-progress workflows. Their jobs are
      -- kept fresh by the health-check thread, so this needs no extra fetch.
      else if workflowIsInProgress wf then workflowJobSummary _children else Nothing

  getExtraTopBoxWidgets _app (EntityData {_static=wf, _state}) = concat [
    if isNothing (workflowRunConclusion wf)
    then [hBox [str "["
              , withAttr hotkeyAttr $ str $ showKey cancelWorkflowKey
              , str "] "
              , withAttr hotkeyMessageAttr $ str "Cancel workflow"
              ]
         ]
    else []
    , [hBox [str "["
            , withAttr hotkeyAttr $ str $ showKeys [sortJobsByFailuresKey, sortJobsByNameKey, sortJobsByRuntimeKey]
            , str "] "
            , withAttr hotkeyMessageAttr $ str "Sort by "
            , sortLabel SortJobsByFailures "failures"
            , withAttr hotkeyMessageAttr $ str "/"
            , sortLabel SortJobsByName "name"
            , withAttr hotkeyMessageAttr $ str "/"
            , sortLabel SortJobsByRuntime "runtime"
            ]
      ]
    ]
    where
      currentSort = workflowNodeStateJobSortBy _state
      sortLabel mode label
        | currentSort == mode = withAttr hotkeyAttr $ str label
        | otherwise = withAttr hotkeyMessageAttr $ str label

  handleHotkey s key (EntityData {_static=wf, _state, _children})
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
    | key == sortJobsByNameKey = toggleWorkflowSort s SortJobsByName
    | key == sortJobsByRuntimeKey = toggleWorkflowSort s SortJobsByRuntime
    | key == sortJobsByFailuresKey = toggleWorkflowSort s SortJobsByFailures
    | key `elem` [nextPageKey, prevPageKey, firstPageKey, lastPageKey] && length _children > workflowJobPageSize =
        modifyWorkflowState s (navigateJobPage key (length _children))
  handleHotkey _ _ _ = return False

-- WorkflowRun {workflowRunWorkflowRunId = Id 7403805672, workflowRunName = N "ci", workflowRunHeadBranch = migrate-debug, workflowRunHeadSha = "1367fa30fc409d198e18afa95bda04d26387925e", workflowRunPath = ".github/workflows/ci.yml", workflowRunDisplayTitle = More database stuff noci, workflowRunRunNumber = 2208, workflowRunEvent = "push", workflowRunStatus = "completed", workflowRunConclusion = Just skipped, workflowRunWorkflowId = 6848152, workflowRunUrl = URL https://api.github.com/repos/codedownio/codedown/actions/runs/7403805672, workflowRunHtmlUrl = URL https://github.com/codedownio/codedown/actions/runs/7403805672, workflowRunCreatedAt = 2024-01-04 00:10:06 UTC, workflowRunUpdatedAt = 2024-01-04 00:10:10 UTC, workflowRunActor = SimpleUser simpleUserId = Id 1634990, simpleUserLogin = N thomasjm, simpleUserAvatarUrl = URL "https://avatars.githubusercontent.com/u/1634990?v=4", simpleUserUrl = URL "https://api.github.com/users/thomasjm", workflowRunAttempt = 1, workflowRunStartedAt = 2024-01-04 00:10:06 UTC}

workflowLine :: Int -> UTCTime -> Bool -> WorkflowRun -> Fetchable a -> Maybe (Async (), Int) -> WorkflowNodeState -> Int -> Widget n
workflowLine animationCounter currentTime toggled' (WorkflowRun {..}) fetchableState healthCheckThreadData wfState totalJobs = vBox [line1, line2]
  where
    runTime = diffUTCTime workflowRunUpdatedAt workflowRunStartedAt
    timeSinceStart = diffUTCTime currentTime workflowRunStartedAt

    hasPagination = totalJobs > workflowJobPageSize
    pageInfo = computeJobPageInfo (workflowNodeStateJobPage wfState) totalJobs

    timingWidget = hBox [
        str [i|#{timeDiff runTime}|]
        , withAttr toggleMarkerAttr $ str " / "
        , str [i|#{timeFromNow timeSinceStart}|]
      ]

    leftContent = hBox [
      withAttr openMarkerAttr $ str (if toggled' then "[-] " else "[+] ")
      , withAttr normalAttr $ str $ toString workflowRunDisplayTitle
      , padLeft (Pad 1) $ statusToIconAnimated animationCounter $ chooseWorkflowStatus $ fromMaybe workflowRunStatus workflowRunConclusion
      , fetchableQuarterCircleSpinner animationCounter fetchableState
      , healthCheckIndicatorWidget healthCheckThreadData
      ]

    line1
      | hasPagination = hBox [leftContent, hCenter (paginationInfo pageInfo), timingWidget]
      | otherwise = hBox [leftContent, padLeft Max timingWidget]

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

toggleWorkflowSort :: AppState -> WorkflowJobSortBy -> EventM ClickableName AppState Bool
toggleWorkflowSort s sortMode = modifyWorkflowState s $ \wns ->
  wns { workflowNodeStateJobSortBy = if workflowNodeStateJobSortBy wns == sortMode then SortJobsByFailures else sortMode
      , workflowNodeStateJobPage = 1 }

modifyWorkflowState :: AppState -> (WorkflowNodeState -> WorkflowNodeState) -> EventM ClickableName AppState Bool
modifyWorkflowState s f = do
  withFixedElemAndParents s $ \_ (SomeNode variableEl) _ ->
    case variableEl of
      SingleWorkflowNode (EntityData {_state=stateVar}) ->
        liftIO $ atomically $ modifyTVar' stateVar f
      _ -> return ()
  return True

navigateJobPage :: V.Key -> Int -> WorkflowNodeState -> WorkflowNodeState
navigateJobPage key totalJobs wns =
  let totalPages = (totalJobs + workflowJobPageSize - 1) `div` workflowJobPageSize
      currentPage = workflowNodeStateJobPage wns
      newPage
        | key == nextPageKey = min totalPages (currentPage + 1)
        | key == prevPageKey = max 1 (currentPage - 1)
        | key == firstPageKey = 1
        | key == lastPageKey = totalPages
        | otherwise = currentPage
  in wns { workflowNodeStateJobPage = newPage }

workflowInner :: WorkflowRun -> [Node Fixed SingleJobT] -> WorkflowNodeState -> Widget n
workflowInner (WorkflowRun {..}) jobs _wfState = vBox $ [
  hBox [
      str "File: "
      , withAttr hashAttr $ str $ toString workflowRunPath
    ]
  ] ++ maybeToList (workflowJobSummary jobs)

-- | True for any not-yet-completed workflow (running, queued, or otherwise in progress). This
-- is exactly the set whose jobs are kept fresh by the health-check thread, so showing the
-- summary for these needs no extra fetch.
workflowIsInProgress :: WorkflowRun -> Bool
workflowIsInProgress wf =
  chooseWorkflowStatus (fromMaybe (workflowRunStatus wf) (workflowRunConclusion wf))
    `notElem` [WorkflowSuccess, WorkflowFailed, WorkflowCancelled, WorkflowNeutral]

-- | The "1 running / 54 not started" summary row, or Nothing if no job statuses are known yet.
workflowJobSummary :: [Node Fixed SingleJobT] -> Maybe (Widget n)
workflowJobSummary jobs
  | null jobStatuses = Nothing
  | otherwise = Just $ hBox $ intercalate [str " / "] [[w] | w <- parts]
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

    ifPositive n w
      | n > 0 = Just w
      | otherwise = Nothing
