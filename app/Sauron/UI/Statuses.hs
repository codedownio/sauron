{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Sauron.UI.Statuses (
  statusToIconAnimated
  , getQuarterCircleSpinner
  , getIdleQuarterCircle
  , activitySpinnerWidget
  , fetchableQuarterCircleSpinner
  , chooseWorkflowStatus
  ) where

import Brick
import Data.Time (NominalDiffTime)
import Relude
import Sauron.Types
import Sauron.UI.AttrMap
import UnliftIO.Async (Async)

quarterCircleSpinners :: [String]
quarterCircleSpinners = ["◐", "◓", "◑", "◒"]

spinningIcons :: [String]
spinningIcons = ["⣾", "⣽", "⣻", "⢿", "⡿", "⣟", "⣯", "⣷"]

getQuarterCircleSpinner :: Int -> Widget n
getQuarterCircleSpinner counter =
  let iconIndex = counter `mod` length quarterCircleSpinners
      icon = case drop iconIndex quarterCircleSpinners of
        (x:_) -> x
        [] -> "◐"  -- fallback
  in withAttr circleSpinnerAttr (str icon)

-- | The spinner glyph held still and dimmed, for something that's being watched but isn't
-- fetching right now.
getIdleQuarterCircle :: Widget n
getIdleQuarterCircle = withAttr idleCircleSpinnerAttr (str (fromMaybe "◐" (listToMaybe quarterCircleSpinners)))

-- | The one spinner a node line gets. It sits still and dimmed while the node is just being
-- watched, spins while a fetch is actually in flight, and goes back to sitting still afterwards;
-- a health check also puts its polling period next to it. A node with neither gets nothing.
activitySpinnerWidget :: Int -> Bool -> Maybe (Async (), Int) -> Widget n
activitySpinnerWidget animationCounter fetching healthCheckThreadData =
  case (fetching, healthCheckThreadData) of
    (False, Nothing) -> emptyWidget
    (_, Nothing) -> spinner
    (_, Just (_, periodMicroseconds)) ->
      let period = fromIntegral periodMicroseconds / 1_000_000 :: NominalDiffTime
      in hBox [spinner, padLeft (Pad 1) $ withAttr idleCircleSpinnerAttr $ str ("[" <> show period <> "]")]
  where
    spinner = padLeft (Pad 1) $
      if fetching then getQuarterCircleSpinner animationCounter else getIdleQuarterCircle

fetchableQuarterCircleSpinner :: Int -> Fetchable a -> Widget n
fetchableQuarterCircleSpinner animationCounter fetchableState =
  case fetchableState of
    Fetching _ -> padLeft (Pad 1) $ getQuarterCircleSpinner animationCounter
    _ -> str ""

getSpinningIcon :: Int -> Widget n
getSpinningIcon counter =
  let iconIndex = counter `mod` length spinningIcons
      icon = case drop iconIndex spinningIcons of
        (x:_) -> x
        [] -> "⣾"  -- fallback
  in withAttr queuedAttr (str icon)

statusToIconAnimated :: Int -> WorkflowStatus -> Widget n
statusToIconAnimated _ WorkflowSuccess = greenCheck
statusToIconAnimated _ WorkflowPending = queuedIcon
statusToIconAnimated counter WorkflowRunning = getSpinningIcon counter
statusToIconAnimated _ WorkflowFailed = redX
statusToIconAnimated _ WorkflowCancelled = cancelled
statusToIconAnimated _ WorkflowNeutral = neutral
statusToIconAnimated _ WorkflowUnknown = unknown

queuedIcon :: Widget n
queuedIcon = withAttr queuedAttr (str "●")

cancelled = withAttr cancelledAttr (str "⊘")
greenCheck = withAttr greenCheckAttr (str "✓")
redX = withAttr redXAttr (str "✗")
neutral = withAttr neutralAttr (str "○")
unknown = withAttr unknownAttr (str "?")

chooseWorkflowStatus :: Text -> WorkflowStatus
chooseWorkflowStatus "completed" = WorkflowSuccess
chooseWorkflowStatus "action_required" = WorkflowPending
chooseWorkflowStatus "cancelled" = WorkflowCancelled
chooseWorkflowStatus "failure" = WorkflowFailed
chooseWorkflowStatus "neutral" = WorkflowNeutral
chooseWorkflowStatus "skipped" = WorkflowCancelled
chooseWorkflowStatus "stale" = WorkflowNeutral
chooseWorkflowStatus "success" = WorkflowSuccess
chooseWorkflowStatus "timed_out" = WorkflowFailed
chooseWorkflowStatus "in_progress" = WorkflowRunning
chooseWorkflowStatus "running" = WorkflowRunning
chooseWorkflowStatus "queued" = WorkflowPending
chooseWorkflowStatus "requested" = WorkflowPending
chooseWorkflowStatus "waiting" = WorkflowPending
chooseWorkflowStatus "pending" = WorkflowPending
chooseWorkflowStatus _ = WorkflowUnknown
