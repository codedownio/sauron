{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sauron.UI.Pull (
  pullLine
  , pullInner
  , checksWidget
  ) where

import Brick
import Brick.Forms
import Brick.Widgets.Border (border, hBorder)
import Control.Monad
import Data.String.Interpolate
import qualified Data.Text as T
import Data.Time
import qualified Data.Vector as V
import GitHub
import GitHub.Data.Name
import Lens.Micro
import Relude
import Sauron.Actions (refreshOnZoom)
import Sauron.Actions.Util (findRepoParent, findPullsParent)
import Sauron.Event.CommentModal (openZoomAndComment)
import Sauron.Event.Helpers (withFixedElemAndParents)
import Sauron.Event.MergeModal (openMergeModal)
import Sauron.Event.PullModal (openPullModalOnTab)
import Sauron.Event.Search (ensureNonEmptySearch)
import Sauron.Fetch.Pull (fetchPullComments)
import Sauron.HealthCheck.Stop (healthCheckIndicatorWidget)
import Sauron.Types
import Sauron.UI.AttrMap
import Sauron.UI.Issue (issueInner, renderTimelineItem, closeReopenAndRefresh, consolidateEvents, detailsToggleWidget, nodeButtonsWidget, actionButtonWidget)
import Sauron.UI.Issue.Events (adaptiveWidth)
import Sauron.UI.Keys
import Sauron.UI.Statuses (fetchableQuarterCircleSpinner, statusToIconAnimated)
import Sauron.UI.Util
import Sauron.UI.Util.TimeDiff
import UnliftIO.Async (Async, async)


instance ListDrawable Fixed 'SinglePullT where
  drawLine appState (EntityData {_static=issue, ..}) =
    pullLine (_appNow appState) _toggled issue (_appAnimationCounter appState) (pullNodeStateTimeline _state)

  drawInner appState (EntityData {_static=issue, _state, _ident, ..}) = do
    guard _toggled
    guardFetchedOrHasPrevious (pullNodeStateTimeline _state) $ \comments -> do
      let extraSections = catMaybes [
            padTop (Pad 1) <$> checksWidget (_appAnimationCounter appState) _healthCheckThread (pullNodeStateChecks _state)
            , mergeabilityWidget issue (pullNodeStateDetails _state)
            , Just (nodeButtonsWidget appState _ident True issue)
            ]
      -- No explicit spacing between sections: the rendering pipeline already
      -- separates them by one blank line
      return $ vBox $
        issueInner (_appDetailsExpanded appState) (_appNow appState) issue comments
        : extraSections

  getExtraTopBoxWidgets app (EntityData {_static=issue}) =
    [hBox [str "["
          , withAttr hotkeyAttr $ str $ showKey editSearchKey
          , str "] "
          , withAttr hotkeyMessageAttr $ str "Search"
          ]
    , hBox [str "["
          , withAttr hotkeyAttr $ str $ showKey zoomModalKey
          , str "/"
          , withAttr hotkeyAttr $ str $ showKey reviewKey
          , str "] "
          , withAttr hotkeyMessageAttr $ str "Zoom"
          , str "/"
          , withAttr hotkeyMessageAttr $ str "Review"
          ]
    , hBox $ [str "["]
             <> intersperse (str "/") [withAttr hotkeyAttr $ str $ showKey key | key <- keys]
             <> [str "] "]
             <> intersperse (str "/") [withAttr hotkeyMessageAttr $ str label | label <- labels]
    , detailsToggleWidget app
    ]
    where
      isOpen = issueState issue == StateOpen
      keys = [closeReopenKey] <> [mergeKey | isOpen] <> [commentKey]
      labels = [if isOpen then "Close" else "Reopen"] <> ["Merge" | isOpen] <> ["Comment"]

  handleHotkey s key (EntityData {_static=issue})
    | key == editSearchKey = do
        withFixedElemAndParents s $ \_ _ parents -> do
          case findPullsParent parents of
            Just pullsNode@(PaginatedPullsNode ed) -> do
              searchText <- liftIO $ atomically $ ensureNonEmptySearch pullsNode
              modify (appForm ?~ (newForm [editTextField id TextForm (Just 1)] searchText, _ident ed))
            _ -> return ()
        return True
    | key == zoomModalKey = True <$ openPullModalOnTab s TabConversation
    | key == commentKey = do
        withFixedElemAndParents s $ \_ _ parents ->
          whenJust (findRepoParent parents) $ \(RepoNode (EntityData {_static=(owner, name)})) ->
            openZoomAndComment s issue True owner name
        return True
    | key == reviewKey = True <$ openPullModalOnTab s TabReview
    | key == mergeKey, issueState issue == StateOpen = do
        withFixedElemAndParents s $ \_ _ parents ->
          whenJust (findRepoParent parents) $ \(RepoNode (EntityData {_static=(owner, name)})) ->
            openMergeModal issue owner name
        return True
    | key == closeReopenKey = do
        liftIO $ void $ async $ do
          withFixedElemAndParents s $ \_ _ parents ->
            whenJust (findRepoParent parents) $ \(RepoNode (EntityData {_static=(owner, name)})) ->
              whenJust (findPullsParent parents) $ \(PaginatedPullsNode ed) ->
                closeReopenAndRefresh (s ^. appBaseContext) owner name issue (_children ed)
                  (\(SinglePullNode e) -> (_static e, _state e))
                  (\(SinglePullNode e) iss -> SinglePullNode (e { _static = iss }))
                  fetchPullComments
        return True

  handleHotkey _ _ _ = return False

-- * CI checks

-- | The CI checks box at the bottom of an open PR, like the web UI's checks section
checksWidget :: Int -> Maybe (Async (), Int) -> Fetchable (V.Vector CheckRun) -> Maybe (Widget ClickableName)
checksWidget animationCounter healthCheckThreadData = \case
  NotFetched -> Nothing
  Fetching Nothing -> Just $ withAttr italicText $ str "Fetching checks..."
  Fetching (Just runs) -> renderChecks animationCounter healthCheckThreadData runs
  Fetched runs -> renderChecks animationCounter healthCheckThreadData runs
  Errored err -> Just $ withAttr erroredAttr $ strWrap [i|Failed to fetch checks: #{err}|]

renderChecks :: Int -> Maybe (Async (), Int) -> V.Vector CheckRun -> Maybe (Widget ClickableName)
renderChecks animationCounter healthCheckThreadData runs
  | V.null runs = Nothing
  | otherwise = Just $ adaptiveWidth $ \w -> border $ vBox $
      [padLeftRight 1 $ hBox [summaryWidget, padLeft Max (healthCheckIndicatorWidget healthCheckThreadData)]
      , padLeftRight 1 (statusBar (max 10 (w - 4)))
      , hBorder]
      <> map (padLeftRight 1 . checkRow) sortedRuns
  where
    -- Like the web UI: failed checks first, then queued, in progress, and successful
    -- last; alphabetical within each status
    sortedRuns = sortOn (\r -> (statusSortKey (checkRunWorkflowStatus r), T.toCaseFold (untagName (checkRunName r)))) (V.toList runs)
    statusSortKey = \case
      WorkflowFailed -> 0 :: Int
      WorkflowPending -> 1
      WorkflowRunning -> 2
      WorkflowCancelled -> 3
      WorkflowNeutral -> 4
      WorkflowUnknown -> 5
      WorkflowSuccess -> 6

    -- A proportional status bar (the terminal version of the web UI's little pie chart):
    -- red failed, yellow pending/running, gray cancelled/skipped, green successful
    statusBar barWidth = hBox [withAttr attr (str (replicate n '━')) | (n, attr) <- barAllocations, n > 0]
      where
        segments = filter ((> 0) . fst) [
          (failedCount, redXAttr)
          , (inProgressCount + queuedCount, queuedAttr)
          , (grayCount, cancelledAttr)
          , (successfulCount, greenCheckAttr)
          ]
        grayCount = countStatus WorkflowCancelled + countStatus WorkflowNeutral + countStatus WorkflowUnknown
        segmentTotal = sum (map fst segments)
        -- Every nonempty segment gets at least one cell; the rest is distributed
        -- proportionally via cumulative rounding so the widths sum exactly
        extraSpace = max 0 (barWidth - length segments)
        cumulative = scanl1 (+) (map fst segments)
        offsets = [round (fromIntegral extraSpace * fromIntegral c / fromIntegral segmentTotal :: Double) | c <- cumulative]
        extras = zipWith (-) offsets (0 : offsets)
        barAllocations = zipWith (\(_, attr) e -> (1 + e, attr)) segments extras

    checkRow run = hBox [
      statusToIconAnimated animationCounter (checkRunWorkflowStatus run)
      , str "  "
      , withAttr normalAttr $ str $ toString $ untagName $ checkRunName run
      , padLeft Max $ withAttr hashAttr $ str (durationText run)
      ]

    durationText (CheckRun {checkRunStartedAt=(Just started), checkRunCompletedAt=(Just completed)}) =
      timeDiff (diffUTCTime completed started)
    durationText _ = ""

    failedCount = countStatus WorkflowFailed
    successfulCount = countStatus WorkflowSuccess
    inProgressCount = countStatus WorkflowRunning
    queuedCount = countStatus WorkflowPending
    countStatus status = length [() | r <- V.toList runs, checkRunWorkflowStatus r == status]

    countsLine = withAttr hotkeyMessageAttr $ str $ intercalate ", " countParts <> " checks"
    countParts = catMaybes [
      countPart failedCount "failing"
      , countPart inProgressCount "in progress"
      , countPart queuedCount "queued"
      , countPart successfulCount "successful"
      , countPart (countStatus WorkflowCancelled) "cancelled"
      , countPart (countStatus WorkflowNeutral) "skipped"
      ]
    countPart n label = if n > 0 then Just (show n <> " " <> label) else Nothing

    summaryWidget
      | failedCount > 0 = vBox [
          withAttr redXAttr $ str "Some checks were not successful"
          , countsLine
          ]
      | inProgressCount + queuedCount > 0 = vBox [
          withAttr queuedAttr $ str "Some checks haven't completed yet"
          , countsLine
          ]
      | successfulCount == V.length runs = withAttr greenCheckAttr $ str "All checks have passed"
      | otherwise = vBox [withAttr normalAttr $ str "Checks completed", countsLine]

-- | The mergeability box shown below the checks on an open PR, like the web UI's
mergeabilityWidget :: Issue -> Fetchable PullRequest -> Maybe (Widget ClickableName)
mergeabilityWidget issue details
  | issueState issue /= StateOpen = Nothing
  | otherwise = fetchableCurrent details <&> \pr ->
      adaptiveWidth $ \_ -> border $ padRight Max $ padLeftRight 1 $ case pullRequestMergeable pr of
        Just True -> vBox [
          hBox [withAttr greenCheckAttr $ str "\10003", str " ", withAttr boldText $ str "No conflicts with base branch"]
          , padLeft (Pad 2) $ withAttr hotkeyMessageAttr $ str "Merging can be performed automatically."
          , actionButtonWidget buttonGreenAttr [str [i|Merge pull request [#{showKey mergeKey}]|]]
          ]
        Just False -> vBox [
          hBox [withAttr redXAttr $ str "\10007", str " ", withAttr boldText $ str "This branch has conflicts with the base branch"]
          , padLeft (Pad 2) $ withAttr hotkeyMessageAttr $ str "Resolve the conflicts before merging."
          ]
        Nothing -> withAttr italicText $ str "Checking for ability to merge automatically..."

checkRunWorkflowStatus :: CheckRun -> WorkflowStatus
checkRunWorkflowStatus (CheckRun {checkRunStatus, checkRunConclusion}) = case checkRunStatus of
  CheckRunQueued -> WorkflowPending
  CheckRunInProgress -> WorkflowRunning
  CheckRunCompleted -> case checkRunConclusion of
    Just CheckRunSuccess -> WorkflowSuccess
    Just CheckRunFailure -> WorkflowFailed
    Just CheckRunTimedOut -> WorkflowFailed
    Just CheckRunStartupFailure -> WorkflowFailed
    Just CheckRunActionRequired -> WorkflowFailed
    Just CheckRunCancelled -> WorkflowCancelled
    Just CheckRunSkipped -> WorkflowNeutral
    Just CheckRunNeutral -> WorkflowNeutral
    Just CheckRunStale -> WorkflowNeutral
    Nothing -> WorkflowUnknown

pullLine :: UTCTime -> Bool -> Issue -> Int -> Fetchable a -> Widget n
pullLine now toggled' (Issue {issueNumber=(IssueNumber number), ..}) animationCounter fetchableState = vBox [line1, line2]
  where
    pullSubjectState
      | issueState == StateOpen && issueDraft == Just True = PullDraft
      | issueState == StateOpen = PullOpen
      | isJust (issuePullRequest >>= pullRequestReferenceMergedAt) = PullMerged
      | otherwise = PullClosed
    (icon, markerAttr) = subjectStateIcon pullSubjectState
    line1 = hBox [
      withAttr openMarkerAttr $ str (if toggled' then "[-] " else "[+] ")
      , withAttr markerAttr $ str (icon <> "  ")
      , withAttr normalAttr $ str $ toString issueTitle
      , fetchableQuarterCircleSpinner animationCounter fetchableState
      , padLeft Max $ str "" -- (if pullComments > 0 then [i|🗨  #{pullComments}|] else "")
      ]

    line2 = padRight Max $ padLeft (Pad 4) $ hBox [
      withAttr hashAttr $ str "#"
      , withAttr hashNumberAttr $ str $ show number
      , str [i| opened #{timeFromNow (diffUTCTime now issueCreatedAt)} by |]
      , withAttr usernameAttr $ str $ [i|#{untagName $ simpleUserLogin issueUser}|]
      ]

pullInner :: DetailsExpanded -> UTCTime -> Issue -> Text -> Fetchable (V.Vector TimelineEvent) -> Widget n
pullInner detailsExpanded now (Issue {..}) body inner =
  allItems
  & zip [0..]
  & fmap (uncurry (renderTimelineItem detailsExpanded now (length allItems)))
  & (++ statusMessages)
  & vBox
  where
    SimpleUser {simpleUserLogin=(N openerUsername)} = issueUser

    (commentsAndEvents, statusMessages) = case inner of
      Fetched cs -> (V.toList cs, [])
      Fetching maybeCs -> case maybeCs of
        Just cs -> (V.toList cs, [strWrap [i|Refreshing comments...|]])
        Nothing -> ([], [strWrap [i|Fetching comments...|]])
      Errored err -> ([], [strWrap [i|Failed to fetch comments: #{err}|]])
      NotFetched -> ([], [strWrap [i|Comments not fetched.|]])

    -- All timeline items including the PR description
    allItems = (Left (openerUsername, body, issueCreatedAt), "")
             : fmap (\item -> (Right item, "")) (consolidateEvents commentsAndEvents)
