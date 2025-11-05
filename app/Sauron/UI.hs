{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Sauron.UI (
  listDrawElement
  , listDrawElement'
  ) where

import Brick
import Brick.Widgets.Border
import Data.Maybe
import Data.String.Interpolate
import Relude
import Sauron.Types
import Sauron.UI.Branch ()
import Sauron.UI.Commit ()
import Sauron.UI.Issue ()
import Sauron.UI.Job ()
import Sauron.UI.Notification ()
import Sauron.UI.Pagination ()
import Sauron.UI.Pull ()
import Sauron.UI.Repo ()
import Sauron.UI.Util
import Sauron.UI.Workflow ()


listDrawElement :: AppState -> Int -> Bool -> SomeNode Fixed -> Widget ClickableName
listDrawElement appState ix isSelected node@(SomeNode inner) = wrapper ix isSelected (getEntityData inner) $ listDrawElement' appState node

listDrawElement' :: AppState -> SomeNode Fixed -> Widget ClickableName
listDrawElement' appState (SomeNode inner) =
  let ed = getEntityData inner
  in vBox $ catMaybes [
    Just $ drawNodeLine appState inner
    , fmap (padLeft (Pad 4) . fixedHeightOrViewportPercent (InnerViewport [i|viewport_#{_ident ed}|]) 50) (drawNodeInner appState inner)
  ]

-- Pattern match on specific node types to call their instances
drawNodeLine :: AppState -> Node Fixed a -> Widget ClickableName
drawNodeLine appState node = case node of
  HeadingNode ed -> drawLine appState ed
  RepoNode ed -> drawLine appState ed
  PaginatedIssuesNode ed -> drawLine appState ed
  PaginatedPullsNode ed -> drawLine appState ed
  PaginatedWorkflowsNode ed -> drawLine appState ed
  PaginatedReposNode ed -> drawLine appState ed
  PaginatedBranchesNode ed -> drawLine appState ed
  PaginatedNotificationsNode ed -> drawLine appState ed
  SingleIssueNode ed -> drawLine appState ed
  SinglePullNode ed -> drawLine appState ed
  SingleWorkflowNode ed -> drawLine appState ed
  SingleJobNode ed -> drawLine appState ed
  SingleBranchNode ed -> drawLine appState ed
  SingleCommitNode ed -> drawLine appState ed
  SingleNotificationNode ed -> drawLine appState ed
  JobLogGroupNode ed -> drawLine appState ed

drawNodeInner :: AppState -> Node Fixed a -> Maybe (Widget ClickableName)
drawNodeInner appState node = case node of
  HeadingNode ed -> drawInner appState ed
  RepoNode ed -> drawInner appState ed
  PaginatedIssuesNode ed -> drawInner appState ed
  PaginatedPullsNode ed -> drawInner appState ed
  PaginatedWorkflowsNode ed -> drawInner appState ed
  PaginatedReposNode ed -> drawInner appState ed
  PaginatedBranchesNode ed -> drawInner appState ed
  PaginatedNotificationsNode ed -> drawInner appState ed
  SingleIssueNode ed -> drawInner appState ed
  SinglePullNode ed -> drawInner appState ed
  SingleWorkflowNode ed -> drawInner appState ed
  SingleJobNode ed -> drawInner appState ed
  SingleBranchNode ed -> drawInner appState ed
  SingleCommitNode ed -> drawInner appState ed
  SingleNotificationNode ed -> drawInner appState ed
  JobLogGroupNode ed -> drawInner appState ed

wrapper :: Int -> Bool -> EntityData a f -> Widget ClickableName -> Widget ClickableName
wrapper ix isSelected x = clickable (ListRow ix) . padLeft (Pad (4 * (_depth x))) . (if isSelected then border else id)
