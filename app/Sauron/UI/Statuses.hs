{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Sauron.UI.Statuses (
  statusToIconAnimated
  , getQuarterCircleSpinner
  , fetchableQuarterCircleSpinner
  , chooseWorkflowStatus
  ) where

import Brick
import Relude
import Sauron.Types
import Sauron.UI.AttrMap

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
neutral = withAttr neutralAttr (str "-")
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
