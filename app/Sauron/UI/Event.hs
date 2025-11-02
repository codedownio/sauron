module Sauron.UI.Event (
  getEventIcon,
  getEventDescription,
  getEventIconWithColor
) where

import Brick (AttrName, withAttr, str, Widget)
import Data.String.Interpolate
import GitHub (EventType(..))
import Relude
import Sauron.UI.AttrMap


getEventIcon :: EventType -> String
getEventIcon eventType = case eventType of
  Closed -> "×"
  Reopened -> "↪"
  Assigned -> "@"
  ActorUnassigned -> "-"
  Labeled -> "#"
  Unlabeled -> "#"
  Milestoned -> "▸"
  Demilestoned -> "▸"
  Renamed -> "~"
  Locked -> "!"
  Unlocked -> "!"
  Referenced -> "&"
  Merged -> "→"
  Mentioned -> "»"
  Subscribed -> "⊕"
  Unsubscribed -> "-"
  ReviewRequested -> "?"
  ReviewDismissed -> "×"
  ReviewRequestRemoved -> "?"
  MarkedAsDuplicate -> "="
  UnmarkedAsDuplicate -> "="
  AddedToProject -> "⊕"
  AddedToProjectV2 -> "⊕"
  MovedColumnsInProject -> "↑"
  ProjectItemStatusChanged -> "↔"
  RemovedFromProject -> "-"
  ConvertedNoteToIssue -> "*"
  HeadRefDeleted -> "∅"
  HeadRefRestored -> "↪"
  Unknown _ -> "?"

-- | Get a colored icon widget for an event type using GitHub's color scheme
getEventIconWithColor :: EventType -> Widget n
getEventIconWithColor eventType =
  withAttr (getEventColorAttr eventType) (str (getEventIcon eventType))

-- | Get the color attribute for each event type based on GitHub's colors
getEventColorAttr :: EventType -> AttrName
getEventColorAttr eventType = case eventType of
  Closed -> eventClosedColor
  Reopened -> eventReopenedColor
  Assigned -> eventAssignedColor
  ActorUnassigned -> eventUnassignedColor
  Labeled -> eventLabeledColor
  Unlabeled -> eventUnlabeledColor
  Milestoned -> eventMilestoneColor
  Demilestoned -> eventMilestoneColor
  Renamed -> eventRenamedColor
  Locked -> eventLockedColor
  Unlocked -> eventUnlockedColor
  Referenced -> eventReferencedColor
  Merged -> eventMergedColor
  Mentioned -> eventMentionedColor
  Subscribed -> eventSubscribedColor
  Unsubscribed -> eventUnsubscribedColor
  ReviewRequested -> eventReviewColor
  ReviewDismissed -> eventReviewColor
  ReviewRequestRemoved -> eventReviewColor
  MarkedAsDuplicate -> eventDuplicateColor
  UnmarkedAsDuplicate -> eventDuplicateColor
  AddedToProject -> eventProjectColor
  AddedToProjectV2 -> eventProjectColor
  MovedColumnsInProject -> eventProjectColor
  ProjectItemStatusChanged -> eventProjectColor
  RemovedFromProject -> eventProjectColor
  ConvertedNoteToIssue -> eventConvertedColor
  HeadRefDeleted -> eventRefDeletedColor
  HeadRefRestored -> eventRefRestoredColor
  Unknown _ -> eventRefRestoredColor

getEventDescription :: EventType -> String
getEventDescription eventType = case eventType of
  Closed -> "closed this as completed"
  Reopened -> "reopened this"
  Assigned -> "was assigned"
  ActorUnassigned -> "was unassigned"
  Labeled -> "added a label"
  Unlabeled -> "removed a label"
  Milestoned -> "added this to a milestone"
  Demilestoned -> "removed this from a milestone"
  Renamed -> "changed the title"
  Locked -> "locked this conversation"
  Unlocked -> "unlocked this conversation"
  Referenced -> "referenced this"
  Merged -> "merged this"
  Mentioned -> "was mentioned"
  Subscribed -> "subscribed to this"
  Unsubscribed -> "unsubscribed from this"
  ReviewRequested -> "requested review"
  ReviewDismissed -> "dismissed a review"
  ReviewRequestRemoved -> "removed review request"
  MarkedAsDuplicate -> "marked this as duplicate"
  UnmarkedAsDuplicate -> "unmarked this as duplicate"
  AddedToProject -> "added this to a project"
  AddedToProjectV2 -> "added this to a project"
  MovedColumnsInProject -> "moved this in a project"
  ProjectItemStatusChanged -> "changed the status in a project"
  RemovedFromProject -> "removed this from a project"
  ConvertedNoteToIssue -> "converted note to issue"
  HeadRefDeleted -> "deleted the head branch"
  HeadRefRestored -> "restored the head branch"
  Unknown t -> [i|#{t} (unknown event type)|]
