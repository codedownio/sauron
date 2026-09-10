{-# LANGUAGE GADTs #-}

module Sauron.UI.Modals.ZoomModal (
  renderZoomModal
  , generateModalTitle
  , commentSection
  , commentModeHotkeys
  , hotkeyWidget
  ) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import qualified Data.Text as T
import GitHub
import Lens.Micro
import Relude
import Sauron.Types
import Sauron.UI.AttrMap
import Sauron.UI.Issue (maxCommentWidth)
import Sauron.UI.Keys (commentKey, openSelectedKey, showKey)
import Sauron.UI.Modals.Common (modalHeightPercent, modalWidth, renderBodyEditor)
import Sauron.UI.Modals.NodeContent (renderNodeContent)
import WEditorBrick.WrappingEditor (dumpEditor)


renderZoomModal :: AppState -> ModalState Fixed -> Widget ClickableName
renderZoomModal appState (ZoomModalState {_zoomModalSomeNode=someNode, _zoomModalCommentMode=commentMode}) =
  vBox ([
    hCenter $ withAttr boldText $ str (generateModalTitle someNode)
    , hBorder
    -- Scrollable content area with node content
    , padBottom Max $ withVScrollBars OnRight $ withVScrollBarHandles $ viewport ZoomModalContent Vertical $
      hCenter $ hLimit maxCommentWidth $ vBox [
        renderNodeContent appState someNode
      ]
    ]
    <> commentSection appState commentMode
    <> [
    hBorder
    , hCenter $ hBox $ intersperse (str "  ") footerHotkeys
  ])
  & border
  & withDefAttr normalAttr
  & hLimit (modalWidth appState)
  & vLimitPercent modalHeightPercent
  & centerLayer
  where
    footerHotkeys = case commentMode of
      Just cm -> commentModeHotkeys cm
      Nothing -> getZoomModalHotkeys someNode
renderZoomModal _ _ = str "Invalid modal state" -- This should never happen

-- | The comment editor, when comment mode is on. Issue and pull request content
-- renders its own comment box above the action buttons, so this is only for nodes
-- (like notifications) whose content has nowhere to put one.
commentSection :: AppState -> Maybe CommentMode -> [Widget ClickableName]
commentSection appState = \case
  Just (CommentMode {_commentModeEditor, _commentModeIssue}) | not (contentHasBox _commentModeIssue) -> [
    hBorder
    , renderBodyEditor appState True (modalWidth appState) (editorLines _commentModeEditor) _commentModeEditor
    ]
  _ -> []
  where
    editorLines editor = max 5 (min (length (dumpEditor editor)) 20)

    contentHasBox commentIssue = case _appModal appState of
      Just (ZoomModalState {_zoomModalSomeNode=SomeNode (SingleIssueNode (EntityData {_static=issue}))}) ->
        issueId issue == issueId commentIssue
      Just (PullRequestModalState {}) -> True
      _ -> False

-- | Footer hotkeys while the comment editor is focused
commentModeHotkeys :: CommentMode -> [Widget ClickableName]
commentModeHotkeys (CommentMode {_commentModeSubmission, _commentModeIssue}) = [
  hotkeyWidget "Alt+Enter" (if _commentModeSubmission == SubmittingComment then "Submitting..." else "Submit comment")
  , hotkeyWidget "Alt+Shift+Enter" closeWithCommentLabel
  , hotkeyWidget "Esc" "Cancel comment"
  ]
  where
    closeWithCommentLabel
      | _commentModeSubmission == SubmittingCloseWithComment = "Closing..."
      | issueState _commentModeIssue == StateOpen = "Close with comment"
      | otherwise = "Reopen with comment"

-- | Generate hotkey widgets for the zoom modal footer based on the node type
getZoomModalHotkeys :: SomeNode Fixed -> [Widget ClickableName]
getZoomModalHotkeys (SomeNode node) = nodeSpecificHotkeys ++ commonHotkeys
  where
    commonHotkeys = [hotkeyWidget (showKey openSelectedKey) "Open", hotkeyWidget "q" "Close modal"]

    nodeSpecificHotkeys = case node of
      -- Issues have a comment button of their own at the bottom of the conversation
      SingleIssueNode {} -> []
      SingleNotificationNode (EntityData {_static=notification}) ->
        if subjectType (notificationSubject notification) `elem` ["Issue", "PullRequest"]
        then [hotkeyWidget (showKey commentKey) "Comment"]
        else []
      _ -> []

hotkeyWidget :: String -> String -> Widget ClickableName
hotkeyWidget key msg = hBox [
  str "["
  , withAttr hotkeyAttr $ str key
  , str "] "
  , withAttr hotkeyMessageAttr $ str msg
  ]

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
    PaginatedIssuesNode (EntityData {_static = label}) ->
      T.unpack label
    PaginatedPullsNode (EntityData {_static = label}) ->
      T.unpack label
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
