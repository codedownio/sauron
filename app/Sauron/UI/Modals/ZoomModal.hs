{-# LANGUAGE GADTs #-}

module Sauron.UI.Modals.ZoomModal (
  renderZoomModal,
  generateModalTitle,
  ) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import qualified Data.Text as T
import GitHub
import Lens.Micro
import Relude
import Sauron.Types
import Sauron.UI
import Sauron.UI.AttrMap
import Sauron.UI.Issue (maxCommentWidth)


renderZoomModal :: AppState -> ModalState Fixed -> Widget ClickableName
renderZoomModal appState (ZoomModalState {_zoomModalSomeNode=someNode}) =
  vBox [
    hCenter $ withAttr boldText $ str modalTitle
    , hBorder
    -- Scrollable content area with node content
    , padBottom Max $ withVScrollBars OnRight $ withVScrollBarHandles $ viewport ZoomModalContent Vertical $
      hLimit maxCommentWidth $ vBox [
        renderNodeContent appState someNode
      ]
    , hBorder
    , hCenter $ withAttr hotkeyMessageAttr $ str "Press [Esc] to close"
  ]
  & border
  & withAttr normalAttr
  & hLimit (maxCommentWidth + 4)
  & vLimitPercent 90
  & centerLayer
  where
    modalTitle = generateModalTitle someNode
renderZoomModal _ _ = str "Invalid modal state" -- This should never happen

renderNodeContent :: AppState -> SomeNode Fixed -> Widget ClickableName
renderNodeContent appState (SomeNode inner) = vBox $ catMaybes [
  Just $ drawNodeLine appState inner'
  , fmap (padLeft (Pad 1)) (drawNodeInner appState inner')
  ]
  where
    inner' = over entityDataL transformEntityData inner

    transformEntityData :: EntityData Fixed a -> EntityData Fixed a
    transformEntityData = set toggled True
                        . over ident (\x -> -x) -- Flip the sign so the viewport doesn't collide with one in the main UI

-- | Generate a nice title for the zoom modal based on node type
generateModalTitle :: SomeNode Fixed -> String
generateModalTitle (SomeNode inner) =
  case inner of
    HeadingNode (EntityData {_static = label}) ->
      "Heading: " <> toString label
    RepoNode (EntityData {_static = (owner, name)}) ->
      "Repository: " <> show owner <> "/" <> show name
    PaginatedReposNode _ ->
      "Repositories"
    PaginatedIssuesNode _ ->
      "Issues"
    PaginatedPullsNode _ ->
      "Pull Requests"
    PaginatedWorkflowsNode _ ->
      "Workflow Runs"
    PaginatedBranchesNode _ ->
      "Branches"
    OverallBranchesNode _ ->
      "Branches"
    PaginatedYourBranchesNode _ ->
      "Your Branches"
    PaginatedActiveBranchesNode _ ->
      "Active Branches"
    PaginatedStaleBranchesNode _ ->
      "Stale Branches"
    PaginatedNotificationsNode _ ->
      "Notifications"
    SingleIssueNode (EntityData {_static = Issue {issueNumber = IssueNumber num, issueTitle}}) ->
      "Issue #" <> show num <> ": " <> T.unpack issueTitle
    SinglePullNode (EntityData {_static = Issue {issueNumber = IssueNumber num, issueTitle}}) ->
      "Pull Request #" <> show num <> ": " <> T.unpack issueTitle
    SingleWorkflowNode (EntityData {_static = WorkflowRun {workflowRunName}}) ->
      "Workflow: " <> T.unpack (untagName workflowRunName)
    SingleJobNode (EntityData {_state}) ->
      case _state of
        Fetched job -> "Job: " <> T.unpack (untagName (jobName job))
        Fetching (Just job) -> "Job: " <> T.unpack (untagName (jobName job))
        _ -> "Job"
    SingleBranchNode (EntityData {_static = Branch {branchName}}) ->
      "Branch: " <> T.unpack branchName
    SingleBranchWithInfoNode (EntityData {_static = (branchInfo, _columnWidths)}) ->
      "Branch: " <> T.unpack (branchWithInfoBranchName branchInfo)
    SingleCommitNode (EntityData {_static = Commit {commitGitCommit = GitCommit {gitCommitMessage}}}) ->
      "Commit: " <> T.unpack (T.take 50 gitCommitMessage) <> if T.length gitCommitMessage > 50 then "..." else ""
    SingleNotificationNode (EntityData {_static = Notification {notificationSubject = Subject {subjectTitle}}}) ->
      "Notification: " <> T.unpack subjectTitle
    JobLogGroupNode (EntityData {_state}) ->
      case _state of
        Fetched (JobLogGroup _ title _ _) -> "Log Group: " <> T.unpack title
        Fetched (JobLogLines _ _) -> "Job log lines"
        NotFetched -> "Job logs not fetched"
        _ -> "Log Group"
