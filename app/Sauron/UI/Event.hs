module Sauron.UI.Event (
  getEventIcon,
  getEventDescription
) where

import GitHub (EventType(..))
import Relude


getEventIcon :: EventType -> String
getEventIcon eventType = case eventType of
  Closed -> "🚫"
  Reopened -> "↩️"
  Assigned -> "👤"
  ActorUnassigned -> "👤"
  Labeled -> "🏷️"
  Unlabeled -> "🏷️"
  Milestoned -> "🏁"
  Demilestoned -> "🏁"
  Renamed -> "✏️"
  Locked -> "🔒"
  Unlocked -> "🔓"
  Referenced -> "📎"
  Merged -> "🔀"
  Mentioned -> "💬"
  Subscribed -> "🔔"
  Unsubscribed -> "🔕"
  ReviewRequested -> "👀"
  ReviewDismissed -> "❌"
  ReviewRequestRemoved -> "👀"
  MarkedAsDuplicate -> "📋"
  UnmarkedAsDuplicate -> "📋"
  AddedToProject -> "📋"
  MovedColumnsInProject -> "📋"
  RemovedFromProject -> "📋"
  ConvertedNoteToIssue -> "📝"
  HeadRefDeleted -> "🗑️"
  HeadRefRestored -> "↩️"

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
  MovedColumnsInProject -> "moved this in a project"
  RemovedFromProject -> "removed this from a project"
  ConvertedNoteToIssue -> "converted note to issue"
  HeadRefDeleted -> "deleted the head branch"
  HeadRefRestored -> "restored the head branch"
