
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
import Data.Typeable
import GitHub hiding (Status)
import Lens.Micro hiding (ix)
import Relude
import Sauron.Types
import Sauron.UI.AnsiUtil
import Sauron.UI.AttrMap
import Sauron.UI.Border
import Sauron.UI.BottomBar
import Sauron.UI.Issue
import Sauron.UI.Job
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

listDrawElement :: AppState -> Int -> Bool -> MainListElem -> Widget ClickableName

-- * Repos

listDrawElement appState ix isSelected (SomeMainListElem (cast -> Just x@(MainListElemItem {_typ=(RepoNode owner name), ..} :: MainListElem' Fixed RepoNodeT))) = wrapper ix isSelected x [
  Just $ renderRepoLine _toggled (owner, name) _state _healthCheck (_appAnimationCounter appState)
  ]

-- * Issues

listDrawElement appState ix isSelected (SomeMainListElem (cast -> Just x@(MainListElemItem {_typ=PaginatedIssues, ..} :: MainListElem' Fixed PaginatedIssuesT))) = wrapper ix isSelected x [
  Just $ case _state of
    Fetched (SearchResult totalCount _xs) -> paginatedHeading x appState "Issues" (str [i|(#{totalCount})|])
    Fetching -> paginatedHeading x appState "Issues" (str "(" <+> getQuarterCircleSpinner (_appAnimationCounter appState) <+> str ")")
    NotFetched -> paginatedHeading x appState "Issues" (str [i|(not fetched)|])
    Errored err -> paginatedHeading x appState "Issues" (str [i|(error fetching: #{err})|])
  ]
listDrawElement appState ix isSelected (SomeMainListElem (cast -> Just x@(MainListElemItem {_typ=(SingleIssue issue), ..} :: MainListElem' Fixed SingleIssueT))) = wrapper ix isSelected x [
  Just $ issueLine (_appNow appState) _toggled issue (_appAnimationCounter appState) _state
  , do
      guard _toggled
      guardFetched _state $ \comments ->
        guardJust (issueBody issue) $ \body ->
          return $ padLeft (Pad 4) $
            fixedHeightOrViewportPercent (InnerViewport [i|viewport_#{_ident}|]) 50 $
              issueInner (_appNow appState) issue body (Fetched comments)
  ]

-- * Pulls

listDrawElement appState ix isSelected (SomeMainListElem (cast -> Just x@(MainListElemItem {_typ=PaginatedPulls, ..} :: MainListElem' Fixed PaginatedPullsT))) = wrapper ix isSelected x [
  Just $ case _state of
    Fetched (SearchResult totalCount _xs) -> paginatedHeading x appState "Pulls" (str [i|(#{totalCount})|])
    Fetching -> paginatedHeading x appState "Pulls" (str "(" <+> getQuarterCircleSpinner (_appAnimationCounter appState) <+> str ")")
    NotFetched -> paginatedHeading x appState "Pulls" (str [i|(not fetched)|])
    Errored err -> paginatedHeading x appState "Pulls" (str [i|(error fetching: #{err})|])
    _ -> str ""
  ]
listDrawElement appState ix isSelected (SomeMainListElem (cast -> Just x@(MainListElemItem {_typ=(SinglePull issue), ..} :: MainListElem' Fixed SinglePullT))) = wrapper ix isSelected x [
  Just $ pullLine (_appNow appState) _toggled issue (_appAnimationCounter appState) _state
  , do
      guard _toggled
      guardFetched _state $ \comments ->
        guardJust (issueBody issue) $ \body ->
          return $ padLeft (Pad 4) $
            fixedHeightOrViewportPercent (InnerViewport [i|viewport_#{_ident}|]) 50 $
              issueInner (_appNow appState) issue body (Fetched comments)
  ]

-- * Workflows

listDrawElement appState ix isSelected (SomeMainListElem (cast -> Just x@(MainListElemItem {_typ=PaginatedWorkflows, ..} :: MainListElem' Fixed PaginatedWorkflowsT))) = wrapper ix isSelected x [
  Just $ case _state of
    Fetched (WithTotalCount _xs totalCount) -> paginatedHeading x appState "Actions" (str [i|(#{totalCount})|])
    Fetching -> paginatedHeading x appState "Actions" (str "(" <+> getQuarterCircleSpinner (_appAnimationCounter appState) <+> str ")")
    NotFetched -> paginatedHeading x appState "Actions" (str [i|(not fetched)|])
    Errored err -> paginatedHeading x appState "Actions" (str [i|(error fetching: #{err})|])
  ]
listDrawElement appState ix isSelected (SomeMainListElem (cast -> Just x@(MainListElemItem {_typ=(SingleWorkflow wf), ..} :: MainListElem' Fixed SingleWorkflowT))) = wrapper ix isSelected x [
  Just $ workflowLine (_appAnimationCounter appState) _toggled wf _state
  , do
      guard _toggled
      guardFetched _state $ \_ ->
        return $ padLeft (Pad 4) $
          fixedHeightOrViewportPercent (InnerViewport [i|viewport_#{_ident}|]) 50 $
            workflowInner wf _state
  ]

-- * Jobs

listDrawElement appState ix isSelected (SomeMainListElem (cast -> Just x@(MainListElemItem {_typ=(SingleJob job), ..} :: MainListElem' Fixed SingleJobT))) = wrapper ix isSelected x [
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

-- * Job Log Groups

listDrawElement _appState ix isSelected (SomeMainListElem (cast -> Just x@(MainListElemItem {_typ=(JobLogGroupNode jobLogGroup), ..} :: MainListElem' Fixed JobLogGroupNodeT))) = wrapper ix isSelected x [
  Just $ jobLogGroupLine _toggled jobLogGroup
  ]

-- * Headings

listDrawElement _appState ix isSelected (SomeMainListElem (cast -> Just x@(MainListElemItem {_typ=(HeadingNode label), ..} :: MainListElem' Fixed HeadingNodeT))) = wrapper ix isSelected x [
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
-- jobLogGroupLine _toggled' (JobLogLines _timestamp contents) = vBox $ map (\content -> hBox [
--   str "  ",
--   withAttr normalAttr $ str $ toString content
--   ]) contents
jobLogGroupLine toggled' (JobLogGroup _timestamp title _children) = hBox [
  withAttr openMarkerAttr $ str (if toggled' then "[-] " else "[+] "),
  withAttr normalAttr $ str $ toString title
  ]

paginatedHeading ::
  MainListElem' Fixed a
  -> AppState
  -> String
  -> Widget ClickableName
  -> Widget ClickableName
paginatedHeading (MainListElemItem {..}) appState l countInParens = hBox $ catMaybes [
  Just $ withAttr openMarkerAttr $ str (if _toggled then "[-] " else "[+] ")
  , Just $ padRight (Pad 1) $ str l
  , Just $ countInParens
  , Just (hCenter (padRight (Pad 4) (searchInfo appState _ident _search) <+> paginationInfo _pageInfo))
  ]

wrapper :: Int -> Bool -> MainListElem' a f -> [Maybe (Widget ClickableName)] -> Widget ClickableName
wrapper ix isSelected x = clickable (ListRow ix) . padLeft (Pad (4 * (_depth x))) . (if isSelected then border else id) . vBox . catMaybes
