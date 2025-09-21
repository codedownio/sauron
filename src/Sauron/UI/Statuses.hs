{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Sauron.UI.Statuses (
  statusToIcon
  , statusToIconAnimated
  , getQuarterCircleSpinner
  , fetchableQuarterCircleSpinner
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
    Fetching -> padLeft (Pad 1) $ getQuarterCircleSpinner animationCounter
    _ -> str ""

getSpinningIcon :: Int -> Widget n
getSpinningIcon counter =
  let iconIndex = counter `mod` length spinningIcons
      icon = case drop iconIndex spinningIcons of
        (x:_) -> x
        [] -> "⣾"  -- fallback
  in withAttr queuedAttr (str icon)

statusToIcon :: Text -> Widget n
statusToIcon = workflowStatusToIcon . chooseWorkflowStatus

statusToIconAnimated :: Int -> Text -> Widget n
statusToIconAnimated counter status'
  | status' == "queued" = queuedIcon
  | status' == "in_progress" = getSpinningIcon counter
  | status' == "running" = getSpinningIcon counter
  | otherwise = case chooseWorkflowStatus status' of
      WorkflowSuccess -> greenCheck
      WorkflowPending -> getSpinningIcon counter
      WorkflowFailed -> redX
      WorkflowCancelled -> cancelled
      WorkflowNeutral -> neutral
      WorkflowUnknown -> queuedIcon

workflowStatusToIcon :: WorkflowStatus -> Widget n
workflowStatusToIcon WorkflowSuccess = greenCheck
workflowStatusToIcon WorkflowPending = ellipses
workflowStatusToIcon WorkflowFailed = redX
workflowStatusToIcon WorkflowCancelled = cancelled
workflowStatusToIcon WorkflowNeutral = neutral
workflowStatusToIcon WorkflowUnknown = unknown

queuedIcon :: Widget n
queuedIcon = withAttr queuedAttr (str "●")

cancelled = withAttr cancelledAttr (str "⊘")
greenCheck = withAttr greenCheckAttr (str "✓")
redX = withAttr redXAttr (str "✗")
ellipses = withAttr ellipsesAttr (str "⋯")
neutral = withAttr neutralAttr (str "-")
unknown = withAttr unknownAttr (str "?")