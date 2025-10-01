{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Sauron.UI (
  drawUI
  ) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import qualified Brick.Widgets.List as L
import Control.Monad
import Data.Maybe
import Data.String.Interpolate
import GitHub hiding (Status)
import Lens.Micro hiding (ix)
import Relude
import Sauron.Types
import Sauron.UI.AnsiUtil
import Sauron.UI.AttrMap
import Sauron.UI.Border
import Sauron.UI.BottomBar
import Sauron.UI.Branch
import Sauron.UI.Commit
import Sauron.UI.Issue
import Sauron.UI.Job
import Sauron.UI.Notification
import Sauron.UI.Pagination
import Sauron.UI.Pull
import Sauron.UI.Repo
import Sauron.UI.Search
import Sauron.UI.Statuses (getQuarterCircleSpinner)
import Sauron.UI.TopBox
import Sauron.UI.Util
import Sauron.UI.Workflow


drawUI :: AppState -> [Widget ClickableName]
drawUI app = [vBox [
                 topBox app
                 , borderWithCounts app
                 -- , fixedHeightOrViewportPercent (InnerViewport [i|viewport_debugging|]) 50 $
                 --     vBox [border $ strWrap (show item <> "\n") | item <- toList (app ^. appMainList)]
                 , hCenter $ padAll 1 $ L.renderListWithIndex (listDrawElement app) True (app ^. appMainList)
                 , clickable InfoBar $ bottomBar app
                 ]
             ]

listDrawElement :: AppState -> Int -> Bool -> SomeNode Fixed -> Widget ClickableName

-- * Repos

listDrawElement appState ix isSelected (SomeNode (PaginatedReposNode ed@(EntityData {..}))) = wrapper ix isSelected ed [
  Just $ case _state of
    Fetched repos -> paginatedHeading ed appState "Repositories" (str [i|(#{length repos})|])
    Fetching maybeRepos -> case maybeRepos of
      Just repos -> paginatedHeading ed appState "Repositories" (str [i|(#{length repos}) |] <+> getQuarterCircleSpinner (_appAnimationCounter appState))
      Nothing -> paginatedHeading ed appState "Repositories" (str "(" <+> getQuarterCircleSpinner (_appAnimationCounter appState) <+> str ")")
    NotFetched -> paginatedHeading ed appState "Repositories" (str [i|(not fetched)|])
    Errored err -> paginatedHeading ed appState "Repositories" (str [i|(error fetching: #{err})|])
  ]
listDrawElement appState ix isSelected (SomeNode (RepoNode x@(EntityData {_static=(owner, name), ..}))) = wrapper ix isSelected x [
  Just $ renderRepoLine _toggled (owner, name) _state _healthCheck (_appAnimationCounter appState)
  ]

-- * Issues

listDrawElement appState ix isSelected (SomeNode (PaginatedIssuesNode x@(EntityData {_state}))) = wrapper ix isSelected x [
  Just $ case _state of
    Fetched (SearchResult totalCount _xs) -> paginatedHeading x appState "Issues" (str [i|(#{totalCount})|])
    Fetching maybeIssues -> case maybeIssues of
      Just (SearchResult totalCount _xs) -> paginatedHeading x appState "Issues" (str [i|(#{totalCount}) |] <+> getQuarterCircleSpinner (_appAnimationCounter appState))
      Nothing -> paginatedHeading x appState "Issues" (str "(" <+> getQuarterCircleSpinner (_appAnimationCounter appState) <+> str ")")
    NotFetched -> paginatedHeading x appState "Issues" (str [i|(not fetched)|])
    Errored err -> paginatedHeading x appState "Issues" (str [i|(error fetching: #{err})|])
  ]
listDrawElement appState ix isSelected (SomeNode (SingleIssueNode ed@(EntityData {_static=issue, ..}))) = wrapper ix isSelected ed [
  Just $ issueLine (_appNow appState) _toggled issue (_appAnimationCounter appState) _state
  , do
      guard _toggled
      guardFetchedOrHasPrevious _state $ \comments ->
        guardJust (issueBody issue) $ \body ->
          return $ padLeft (Pad 4) $
            fixedHeightOrViewportPercent (InnerViewport [i|viewport_#{_ident}|]) 50 $
              issueInner (_appNow appState) issue body (Fetched comments)
  ]

-- * Pulls

listDrawElement appState ix isSelected (SomeNode (PaginatedPullsNode ed@(EntityData {_state}))) = wrapper ix isSelected ed [
  Just $ case _state of
    Fetched (SearchResult totalCount _xs) -> paginatedHeading ed appState "Pulls" (str [i|(#{totalCount})|])
    Fetching maybePulls -> case maybePulls of
      Just (SearchResult totalCount _xs) -> paginatedHeading ed appState "Pulls" (str [i|(#{totalCount}) |] <+> getQuarterCircleSpinner (_appAnimationCounter appState))
      Nothing -> paginatedHeading ed appState "Pulls" (str "(" <+> getQuarterCircleSpinner (_appAnimationCounter appState) <+> str ")")
    NotFetched -> paginatedHeading ed appState "Pulls" (str [i|(not fetched)|])
    Errored err -> paginatedHeading ed appState "Pulls" (str [i|(error fetching: #{err})|])
  ]
listDrawElement appState ix isSelected (SomeNode (SinglePullNode ed@(EntityData {_static=issue, ..}))) = wrapper ix isSelected ed [
  Just $ pullLine (_appNow appState) _toggled issue (_appAnimationCounter appState) _state
  , do
      guard _toggled
      guardFetchedOrHasPrevious _state $ \comments ->
        guardJust (issueBody issue) $ \body ->
          return $ padLeft (Pad 4) $
            fixedHeightOrViewportPercent (InnerViewport [i|viewport_#{_ident}|]) 50 $
              issueInner (_appNow appState) issue body (Fetched comments)
  ]

-- * Workflows

listDrawElement appState ix isSelected (SomeNode (PaginatedWorkflowsNode ed@(EntityData {..}))) = wrapper ix isSelected ed [
  Just $ case _state of
    Fetched (WithTotalCount _xs totalCount) -> paginatedHeading ed appState "Actions" (str [i|(#{totalCount})|])
    Fetching maybeWorkflows -> case maybeWorkflows of
      Just (WithTotalCount _xs totalCount) -> paginatedHeading ed appState "Actions" (str [i|(#{totalCount}) |] <+> getQuarterCircleSpinner (_appAnimationCounter appState))
      Nothing -> paginatedHeading ed appState "Actions" (str "(" <+> getQuarterCircleSpinner (_appAnimationCounter appState) <+> str ")")
    NotFetched -> paginatedHeading ed appState "Actions" (str [i|(not fetched)|])
    Errored err -> paginatedHeading ed appState "Actions" (str [i|(error fetching: #{err})|])
  ]
listDrawElement appState ix isSelected (SomeNode (SingleWorkflowNode ed@(EntityData {_static=wf, ..}))) = wrapper ix isSelected ed [
  Just $ workflowLine (_appAnimationCounter appState) _toggled wf _state
  , do
      guard _toggled
      guardFetchedOrHasPrevious _state $ \_ ->
        return $ padLeft (Pad 4) $
          fixedHeightOrViewportPercent (InnerViewport [i|viewport_#{_ident}|]) 50 $
            workflowInner wf _state
  ]

-- * Branches and commits

listDrawElement appState ix isSelected (SomeNode (PaginatedBranchesNode ed@(EntityData {..}))) = wrapper ix isSelected ed [
  Just $ case _state of
    Fetched branches -> paginatedHeading ed appState "Branches" (str [i|(#{length branches})|])
    Fetching maybeBranches -> case maybeBranches of
      Just branches -> paginatedHeading ed appState "Branches" (str [i|(#{length branches}) |] <+> getQuarterCircleSpinner (_appAnimationCounter appState))
      Nothing -> paginatedHeading ed appState "Branches" (str "(" <+> getQuarterCircleSpinner (_appAnimationCounter appState) <+> str ")")
    NotFetched -> paginatedHeading ed appState "Branches" (str [i|(not fetched)|])
    Errored err -> paginatedHeading ed appState "Branches" (str [i|(error fetching: #{err})|])
  ]

listDrawElement appState ix isSelected (SomeNode (SingleBranchNode ed@(EntityData {_static=branch, _state, ..}))) = wrapper ix isSelected ed [
  Just $ branchLine _toggled branch appState _state
  ]

listDrawElement _appState ix isSelected (SomeNode (SingleCommitNode ed@(EntityData {_static=commit, ..}))) = wrapper ix isSelected ed [
  Just $ commitLine _toggled commit
  ]

-- * Jobs

listDrawElement appState ix isSelected (SomeNode (SingleJobNode ed@(EntityData {_static=job, ..}))) = wrapper ix isSelected ed [
  Just $ jobLine (_appAnimationCounter appState) _toggled job _state
  , do
      guard _toggled
      return $ padLeft (Pad 4) $
        fixedHeightOrViewportPercent (InnerViewport [i|viewport_#{_ident}|]) 50 $
          jobInner (_appAnimationCounter appState) job maybeJobLogs
  ]
  where
    maybeJobLogs = case _state of
      Fetched logs -> Just logs
      _ -> Nothing

-- * Notifications

listDrawElement appState ix isSelected (SomeNode (PaginatedNotificationsNode ed@(EntityData {..}))) = wrapper ix isSelected ed [
  Just $ case _state of
    Fetched notifications -> paginatedHeading ed appState "Notifications" (str [i|(#{length notifications})|])
    Fetching maybeNotifications -> case maybeNotifications of
      Just notifications -> paginatedHeading ed appState "Notifications" (str [i|(#{length notifications}) |] <+> getQuarterCircleSpinner (_appAnimationCounter appState))
      Nothing -> paginatedHeading ed appState "Notifications" (str "(" <+> getQuarterCircleSpinner (_appAnimationCounter appState) <+> str ")")
    NotFetched -> paginatedHeading ed appState "Notifications" (str [i|(not fetched)|])
    Errored err -> paginatedHeading ed appState "Notifications" (str [i|(error fetching: #{err})|])
  ]

listDrawElement appState ix isSelected (SomeNode (SingleNotificationNode ed@(EntityData {_static=notification, ..}))) = wrapper ix isSelected ed [
  Just $ notificationLine _toggled notification (_appAnimationCounter appState) _state
  , do
      guard _toggled
      return $ padLeft (Pad 4) $
        fixedHeightOrViewportPercent (InnerViewport [i|viewport_#{_ident}|]) 50 $
          notificationInner notification
  ]

-- * Job Log Groups

listDrawElement _appState ix isSelected (SomeNode (JobLogGroupNode ed@(EntityData {_static=jobLogGroup, ..}))) = wrapper ix isSelected ed [
  Just $ jobLogGroupLine _toggled jobLogGroup
  ]

-- * Headings

listDrawElement _appState ix isSelected (SomeNode (HeadingNode ed@(EntityData {_static=label, ..}))) = wrapper ix isSelected ed [
  Just $ hBox $ catMaybes [
    Just $ withAttr openMarkerAttr $ str (if _toggled then "[-] " else "[+] ")
    , Just (hBox [str (toString label)])
    , Just (padLeft Max (str " "))
    ]
  ]

jobLogGroupLine :: Bool -> JobLogGroup -> Widget n
jobLogGroupLine _toggled' (JobLogLines _timestamp contents) = vBox $ map (\content -> hBox $
  str "  " : parseAnsiText content
  ) contents
jobLogGroupLine toggled' (JobLogGroup _timestamp title _children) = hBox [
  withAttr openMarkerAttr $ str (if toggled' then "[-] " else "[+] "),
  withAttr normalAttr $ str $ toString title
  ]

paginatedHeading ::
  EntityData Fixed a
  -> AppState
  -> String
  -> Widget ClickableName
  -> Widget ClickableName
paginatedHeading (EntityData {..}) appState l countInParens = hBox $ catMaybes [
  Just $ withAttr openMarkerAttr $ str (if _toggled then "[-] " else "[+] ")
  , Just $ padRight (Pad 1) $ str l
  , Just $ countInParens
  , Just (hCenter (padRight (Pad 4) (searchInfo appState _ident _search) <+> paginationInfo _pageInfo))
  ]

wrapper :: Int -> Bool -> EntityData a f -> [Maybe (Widget ClickableName)] -> Widget ClickableName
wrapper ix isSelected x = clickable (ListRow ix) . padLeft (Pad (4 * (_depth x))) . (if isSelected then border else id) . vBox . catMaybes
