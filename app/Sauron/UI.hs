{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Sauron.UI (
  listDrawElement
  , listDrawElement'
  ) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Control.Monad
import Data.Maybe
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Vector as V
import GitHub hiding (Status)
import Relude
import Sauron.Fetch.Core
import Sauron.Types
import Sauron.UI.AnsiUtil
import Sauron.UI.AttrMap
import Sauron.UI.Branch
import Sauron.UI.Commit (commitLine, commitInner)
import Sauron.UI.Issue
import Sauron.UI.Job
import Sauron.UI.Notification
import Sauron.UI.Pagination
import Sauron.UI.Pull
import Sauron.UI.Repo
import Sauron.UI.Search
import Sauron.UI.Statuses (getQuarterCircleSpinner, statusToIconAnimated, chooseWorkflowStatus)
import Sauron.UI.Util
import Sauron.UI.Workflow


listDrawElement :: AppState -> Int -> Bool -> SomeNode Fixed -> Widget ClickableName
listDrawElement appState ix isSelected node@(SomeNode inner) = wrapper ix isSelected (getEntityData inner) $ listDrawElement' appState node

listDrawElement' :: AppState -> SomeNode Fixed -> Widget ClickableName

-- * Repos

listDrawElement' appState (SomeNode (PaginatedReposNode ed@(EntityData {..}))) = vBox $ catMaybes [
  Just $ case _state of
    Fetched (SearchResult totalCount repos) -> paginatedHeading ed appState "Repositories" (str [i|(#{V.length repos}) of #{totalCount}|])
    Fetching maybeRepos -> case maybeRepos of
      Just (SearchResult totalCount repos) -> paginatedHeading ed appState "Repositories" (str [i|(#{V.length repos}) of #{totalCount} |] <+> getQuarterCircleSpinner (_appAnimationCounter appState))
      Nothing -> paginatedHeading ed appState "Repositories" (str "(" <+> getQuarterCircleSpinner (_appAnimationCounter appState) <+> str ")")
    NotFetched -> paginatedHeading ed appState "Repositories" (str [i|(not fetched)|])
    Errored err -> paginatedHeading ed appState "Repositories" (str [i|(error fetching: #{err})|])
  ]
listDrawElement' appState (SomeNode (RepoNode (EntityData {_static=(owner, name), ..}))) = vBox $ catMaybes [
  Just $ renderRepoLine _toggled (owner, name) _state _healthCheck (_appAnimationCounter appState)
  ]

-- * Issues

listDrawElement' appState (SomeNode (PaginatedIssuesNode x@(EntityData {_state}))) = vBox $ catMaybes [
  Just $ case _state of
    Fetched (SearchResult totalCount _xs) -> paginatedHeading x appState "Issues" (str [i|(#{totalCount})|])
    Fetching maybeIssues -> case maybeIssues of
      Just (SearchResult totalCount _xs) -> paginatedHeading x appState "Issues" (str [i|(#{totalCount}) |] <+> getQuarterCircleSpinner (_appAnimationCounter appState))
      Nothing -> paginatedHeading x appState "Issues" (str "(" <+> getQuarterCircleSpinner (_appAnimationCounter appState) <+> str ")")
    NotFetched -> paginatedHeading x appState "Issues" (str [i|(not fetched)|])
    Errored err -> paginatedHeading x appState "Issues" (str [i|(error fetching: #{err})|])
  ]
listDrawElement' appState (SomeNode (SingleIssueNode (EntityData {_static=issue, ..}))) = vBox $ catMaybes [
  Just $ issueLine (_appNow appState) _toggled issue (_appAnimationCounter appState) _state
  , do
      guard _toggled
      guardFetchedOrHasPrevious _state $ \comments ->
        return $ padLeft (Pad 4) $
          fixedHeightOrViewportPercent (InnerViewport [i|viewport_#{_ident}|]) 50 $
            issueInner (_appNow appState) issue comments
  ]

-- * Pulls

listDrawElement' appState (SomeNode (PaginatedPullsNode ed@(EntityData {_state}))) = vBox $ catMaybes [
  Just $ case _state of
    Fetched (SearchResult totalCount _xs) -> paginatedHeading ed appState "Pulls" (str [i|(#{totalCount})|])
    Fetching maybePulls -> case maybePulls of
      Just (SearchResult totalCount _xs) -> paginatedHeading ed appState "Pulls" (str [i|(#{totalCount}) |] <+> getQuarterCircleSpinner (_appAnimationCounter appState))
      Nothing -> paginatedHeading ed appState "Pulls" (str "(" <+> getQuarterCircleSpinner (_appAnimationCounter appState) <+> str ")")
    NotFetched -> paginatedHeading ed appState "Pulls" (str [i|(not fetched)|])
    Errored err -> paginatedHeading ed appState "Pulls" (str [i|(error fetching: #{err})|])
  ]
listDrawElement' appState (SomeNode (SinglePullNode (EntityData {_static=issue, ..}))) = vBox $ catMaybes [
  Just $ pullLine (_appNow appState) _toggled issue (_appAnimationCounter appState) _state
  , do
      guard _toggled
      guardFetchedOrHasPrevious _state $ \comments ->
        return $ padLeft (Pad 4) $
          fixedHeightOrViewportPercent (InnerViewport [i|viewport_#{_ident}|]) 50 $
            issueInner (_appNow appState) issue comments
  ]

-- * Workflows

listDrawElement' appState (SomeNode (PaginatedWorkflowsNode ed@(EntityData {..}))) = vBox $ catMaybes [
  Just $ case _state of
    Fetched (WithTotalCount _xs totalCount) -> paginatedHeading ed appState "Actions" (str [i|(#{totalCount})|])
    Fetching maybeWorkflows -> case maybeWorkflows of
      Just (WithTotalCount _xs totalCount) -> paginatedHeading ed appState "Actions" (str [i|(#{totalCount}) |] <+> getQuarterCircleSpinner (_appAnimationCounter appState))
      Nothing -> paginatedHeading ed appState "Actions" (str "(" <+> getQuarterCircleSpinner (_appAnimationCounter appState) <+> str ")")
    NotFetched -> paginatedHeading ed appState "Actions" (str [i|(not fetched)|])
    Errored err -> paginatedHeading ed appState "Actions" (str [i|(error fetching: #{err})|])
  ]
listDrawElement' appState (SomeNode (SingleWorkflowNode (EntityData {_static=wf, ..}))) = vBox $ catMaybes [
  Just $ workflowLine (_appAnimationCounter appState) _toggled wf _state
  , do
      guard _toggled
      guardFetchedOrHasPrevious _state $ \_ ->
        return $ padLeft (Pad 4) $
          fixedHeightOrViewportPercent (InnerViewport [i|viewport_#{_ident}|]) 50 $
            workflowInner wf _state
  ]

-- * Branches and commits

listDrawElement' appState (SomeNode (PaginatedBranchesNode ed@(EntityData {..}))) = vBox $ catMaybes [
  Just $ case _state of
    Fetched branches -> paginatedHeading ed appState "Branches" (countWidget _pageInfo branches)
    Fetching maybeBranches -> case maybeBranches of
      Just branches -> paginatedHeading ed appState "Branches" (countWidget _pageInfo branches <+> str " " <+> getQuarterCircleSpinner (_appAnimationCounter appState))
      Nothing -> paginatedHeading ed appState "Branches" (str "(" <+> getQuarterCircleSpinner (_appAnimationCounter appState) <+> str ")")
    NotFetched -> paginatedHeading ed appState "Branches" (str [i|(not fetched)|])
    Errored err -> paginatedHeading ed appState "Branches" (str [i|(error fetching: #{err})|])
  ]

listDrawElement' appState (SomeNode (SingleBranchNode (EntityData {_static=branch, _state, ..}))) = vBox $ catMaybes [
  Just $ branchLine _toggled branch appState _state
  ]

listDrawElement' appState (SomeNode (SingleCommitNode (EntityData {_static=commit, _state, ..}))) = vBox $ catMaybes [
  Just $ commitLine (_appNow appState) _toggled commit
  , do
      guard _toggled
      guardFetchedOrHasPrevious _state $ \detailedCommit ->
        return $ padLeft (Pad 4) $
          fixedHeightOrViewportPercent (InnerViewport [i|viewport_#{_ident}|]) 50 $
            commitInner detailedCommit
  ]

-- * Jobs

listDrawElement' appState (SomeNode (SingleJobNode (EntityData {..}))) = vBox $ catMaybes [
  Just $ case _state of
    Fetched (job, _) -> jobLine (_appAnimationCounter appState) _toggled job _state
    Fetching (Just (job, _)) -> jobLine (_appAnimationCounter appState) _toggled job _state
    Fetching Nothing -> str "Loading job..."
    NotFetched -> str [i|Job not fetched|]
    Errored e -> str [i|Job fetch errored: #{e}|]
  , do
      guard _toggled
      guardFetchedOrHasPrevious _state $ \(job, logs) ->
        return $ padLeft (Pad 4) $
          fixedHeightOrViewportPercent (InnerViewport [i|viewport_#{_ident}|]) 50 $
            jobInner (_appAnimationCounter appState) job (Just logs)
  ]

-- * Notifications

listDrawElement' appState (SomeNode (PaginatedNotificationsNode ed@(EntityData {..}))) = vBox $ catMaybes [
  Just $ case _state of
    Fetched notifications -> paginatedHeading ed appState "Notifications" (countWidget _pageInfo notifications)
    Fetching maybeNotifications -> case maybeNotifications of
      Just notifications -> paginatedHeading ed appState "Notifications" (countWidget _pageInfo notifications <+> str " " <+> getQuarterCircleSpinner (_appAnimationCounter appState))
      Nothing -> paginatedHeading ed appState "Notifications" (str "(" <+> getQuarterCircleSpinner (_appAnimationCounter appState) <+> str ")")
    NotFetched -> paginatedHeading ed appState "Notifications" (str [i|(not fetched)|])
    Errored err -> paginatedHeading ed appState "Notifications" (str [i|(error fetching: #{err})|])
  ]

listDrawElement' appState (SomeNode (SingleNotificationNode (EntityData {_static=notification, ..}))) = vBox $ catMaybes [
  Just $ notificationLine (_appNow appState) _toggled notification (_appAnimationCounter appState) _state
  , do
      guard _toggled
      return $ padLeft (Pad 4) $
        fixedHeightOrViewportPercent (InnerViewport [i|viewport_#{_ident}|]) 50 $
          notificationInner (_appNow appState) notification
  ]

-- * Job Log Groups

listDrawElement' appState (SomeNode (JobLogGroupNode (EntityData {_static=jobLogGroup, ..}))) = vBox $ catMaybes [
  Just $ jobLogGroupLine (_appAnimationCounter appState) _toggled jobLogGroup
  , do
      guard _toggled
      case jobLogGroup of
        JobLogGroup _timestamp _title (Just _status) children' ->
          return $ padLeft (Pad 4) $
            fixedHeightOrViewportPercent (InnerViewport [i|viewport_#{_ident}|]) 50 $
              jobLogGroupInner children'
        _ -> Nothing
  ]

-- * Headings

listDrawElement' _appState (SomeNode (HeadingNode (EntityData {_static=label, ..}))) = vBox $ catMaybes [
  Just $ hBox $ catMaybes [
    Just $ withAttr openMarkerAttr $ str (if _toggled then "[-] " else "[+] ")
    , Just (hBox [str (toString label)])
    , Just (padLeft Max (str " "))
    ]
  ]

jobLogGroupLine :: Int -> Bool -> JobLogGroup -> Widget n
jobLogGroupLine _animationCounter _toggled' (JobLogLines _timestamp contents) = vBox $ map (\content -> padRight Max $ hBox $
  str "  " : parseAnsiText content
  ) contents
jobLogGroupLine animationCounter toggled' (JobLogGroup _timestamp title status _children) = padRight Max $ hBox $ catMaybes [
  Just $ withAttr openMarkerAttr $ str (if toggled' then "[-] " else "[+] "),
  Just $ withAttr normalAttr $ str $ toString title,
  statusWidget
  ]
  where
    statusWidget = case status of
      Just s -> Just $ padLeft (Pad 1) $ statusToIconAnimated animationCounter $ chooseWorkflowStatus s
      Nothing -> Nothing

jobLogGroupInner :: [JobLogGroup] -> Widget n
jobLogGroupInner logGroups = vBox $ map renderLogGroup logGroups
  where
    renderLogGroup (JobLogLines _timestamp contents) = vBox $ map renderLogLine contents
    renderLogGroup (JobLogGroup _timestamp title _status children') = vBox [
      withAttr normalAttr $ str $ toString title,
      vBox $ map renderLogGroup children'
      ]

    renderLogLine content
      | "[command]" `T.isPrefixOf` content = hBox $ renderCommandLine content
      | "##[error]" `T.isPrefixOf` content = hBox $ renderErrorLine content
      | otherwise = hBox $ parseAnsiText content

    renderCommandLine content =
      let commandText = T.drop 9 content  -- Remove "[command]"
      in [ str "▶ "
         , withAttr commandAttr $ str $ toString commandText
         ]

    renderErrorLine content =
      let text = T.drop 9 content  -- Remove "##[error]"
      in [ withAttr erroredAttr $ str $ toString text ]

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

countWidget :: PageInfo -> V.Vector a -> Widget n
countWidget pageInfo' items = case pageInfoLastPage pageInfo' of
  Just lastPage -> str [i|(~#{lastPage * pageSize})|]
  Nothing -> str [i|(#{V.length items})|]

wrapper :: Int -> Bool -> EntityData a f -> Widget ClickableName -> Widget ClickableName
wrapper ix isSelected x = clickable (ListRow ix) . padLeft (Pad (4 * (_depth x))) . (if isSelected then border else id)
