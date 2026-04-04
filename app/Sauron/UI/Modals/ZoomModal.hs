{-# LANGUAGE GADTs #-}

module Sauron.UI.Modals.ZoomModal (
  renderZoomModal
  , generateModalTitle
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
import Sauron.UI.Keys (commentKey, showKey)
import Sauron.UI.Modals.CommentModal (modalHeightPercent, modalWidth)


renderZoomModal :: AppState -> ModalState Fixed -> Widget ClickableName
renderZoomModal appState (ZoomModalState {_zoomModalSomeNode=someNode, _zoomModalParents=_parents}) =
  vBox [
    hCenter $ withAttr boldText $ str modalTitle
    , hBorder
    -- Scrollable content area with node content
    , padBottom Max $ withVScrollBars OnRight $ withVScrollBarHandles $ viewport ZoomModalContent Vertical $
      hCenter $ hLimit maxCommentWidth $ vBox [
        renderNodeContent appState someNode
      ]
    , hBorder
    , hCenter $ hBox $ intersperse (str "  ") $ getZoomModalHotkeys someNode
  ]
  & border
  & withDefAttr normalAttr
  & hLimit (modalWidth appState)
  & vLimitPercent modalHeightPercent
  & centerLayer
  where
    modalTitle = generateModalTitle someNode
renderZoomModal _ _ = str "Invalid modal state" -- This should never happen

-- | Generate hotkey widgets for the zoom modal footer based on the node type
getZoomModalHotkeys :: SomeNode Fixed -> [Widget ClickableName]
getZoomModalHotkeys (SomeNode node) = nodeSpecificHotkeys ++ commonHotkeys
  where
    commonHotkeys = [hotkeyWidget "Esc" "Close"]

    nodeSpecificHotkeys = case node of
      SingleIssueNode {} -> [hotkeyWidget (showKey commentKey) "Comment"]
      SinglePullNode {} -> [hotkeyWidget (showKey commentKey) "Comment"]
      _ -> []

    hotkeyWidget :: String -> String -> Widget ClickableName
    hotkeyWidget key msg = hBox [
      str "["
      , withAttr hotkeyAttr $ str key
      , str "] "
      , withAttr hotkeyMessageAttr $ str msg
      ]

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
        JobNodeState {jnsJob = Fetched job} -> "Job: " <> T.unpack (untagName (jobName job))
        JobNodeState {jnsJob = Fetching (Just job)} -> "Job: " <> T.unpack (untagName (jobName job))
        _ -> "Job"
    SingleBranchNode (EntityData {_static = Branch {branchName}}) ->
      "Branch: " <> T.unpack branchName
    SingleBranchWithInfoNode (EntityData {_static = (branchInfo, _columnWidths)}) ->
      "Branch: " <> T.unpack (branchWithInfoBranchName branchInfo)
    SingleCommitNode (EntityData {_static = Commit {commitGitCommit = GitCommit {gitCommitMessage}}}) ->
      "Commit: " <> T.unpack (T.take 50 gitCommitMessage) <> if T.length gitCommitMessage > 50 then "..." else ""
    SingleNotificationNode (EntityData {_static = Notification {notificationSubject = Subject {subjectTitle}}}) ->
      "Notification: " <> T.unpack subjectTitle
    JobLogGroupNode (EntityData {_static=jobLogGroup}) ->
      case jobLogGroup of
        JobLogGroup {jobLogGroupTitle = title} -> "Log Group: " <> T.unpack title
        JobLogLines {} -> "Job log lines"
