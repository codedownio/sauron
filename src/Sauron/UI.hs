
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
import Sauron.UI.AttrMap
import Sauron.UI.Border
import Sauron.UI.BottomBar
import Sauron.UI.Issue
import Sauron.UI.Job
import Sauron.UI.Pagination
import Sauron.UI.Pull
import Sauron.UI.Repo
import Sauron.UI.Search
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
listDrawElement _appState ix isSelected x@(MainListElemHeading {..}) = wrapper ix isSelected x [
  Just $ hBox $ catMaybes [
    Just $ withAttr openMarkerAttr $ str (if _toggled then "[-] " else "[+] ")
    , Just (hBox [str (toString _label)])
    , Just (padLeft Max (str " "))
    ]
  ]
listDrawElement _appState ix isSelected x@(MainListElemRepo {..}) = wrapper ix isSelected x [
  Just $ renderRepoLine _toggled _namespaceName _repo _healthCheck
  ]

-- * Issues

listDrawElement appState ix isSelected x@(MainListElemItem {_typ=PaginatedIssues, ..}) = wrapper ix isSelected x [
  Just $ case _state of
    Fetched (PaginatedItemsIssues (SearchResult totalCount _xs)) -> paginatedHeading x appState "Issues" (str [i|(#{totalCount})|])
    Fetching -> paginatedHeading x appState "Issues" (str [i|(...)|])
    NotFetched -> paginatedHeading x appState "Issues" (str [i|(not fetched)|])
    Errored err -> paginatedHeading x appState "Issues" (str [i|(error fetching: #{err})|])
    _ -> str ""
  ]
listDrawElement appState ix isSelected x@(MainListElemItem {_typ=(SingleIssue issue), ..}) = wrapper ix isSelected x [
  Just $ issueLine (_appNow appState) _toggled issue
  , do
      guard _toggled
      guardFetched _state $ \case
        PaginatedItemIssue comments -> guardJust (issueBody issue) $ \body ->
          return $ padLeft (Pad 4) $
            fixedHeightOrViewportPercent (InnerViewport [i|viewport_#{_ident}|]) 50 $
              issueInner (_appNow appState) issue body (Fetched comments)
        _ -> return $ str ""
  ]

-- * Pulls

listDrawElement appState ix isSelected x@(MainListElemItem {_typ=PaginatedPulls, ..}) = wrapper ix isSelected x [
  Just $ case _state of
    Fetched (PaginatedItemsPulls (SearchResult totalCount _xs)) -> paginatedHeading x appState "Pulls" (str [i|(#{totalCount})|])
    Fetching -> paginatedHeading x appState "Pulls" (str [i|(...)|])
    NotFetched -> paginatedHeading x appState "Pulls" (str [i|(not fetched)|])
    Errored err -> paginatedHeading x appState "Pulls" (str [i|(error fetching: #{err})|])
    _ -> str ""
  ]
listDrawElement appState ix isSelected x@(MainListElemItem {_typ=(SinglePull issue), ..}) = wrapper ix isSelected x [
  Just $ pullLine (_appNow appState) _toggled issue
  , do
      guard _toggled
      guardFetched _state $ \case
        PaginatedItemPull comments -> guardJust (issueBody issue) $ \body ->
          return $ padLeft (Pad 4) $
            fixedHeightOrViewportPercent (InnerViewport [i|viewport_#{_ident}|]) 50 $
              issueInner (_appNow appState) issue body (Fetched comments)
        _ -> return $ str ""
  ]

-- * Workflows

listDrawElement appState ix isSelected x@(MainListElemItem {_typ=PaginatedWorkflows, ..}) = wrapper ix isSelected x [
  Just $ case _state of
    Fetched (PaginatedItemsWorkflows (WithTotalCount _xs totalCount)) -> paginatedHeading x appState "Actions" (str [i|(#{totalCount})|])
    Fetching -> paginatedHeading x appState "Actions" (str [i|(...)|])
    NotFetched -> paginatedHeading x appState "Actions" (str [i|(not fetched)|])
    Errored err -> paginatedHeading x appState "Actions" (str [i|(error fetching: #{err})|])
    _ -> str ""
  ]
listDrawElement appState ix isSelected x@(MainListElemItem {_typ=(SingleWorkflow wf), ..}) = wrapper ix isSelected x [
  Just $ workflowLine (_appAnimationCounter appState) _toggled wf
  , do
      guard _toggled
      guardFetched _state $ \case
        PaginatedItemWorkflow _ ->
          return $ padLeft (Pad 4) $
            fixedHeightOrViewportPercent (InnerViewport [i|viewport_#{_ident}|]) 50 $
              workflowInner wf _state
        _ -> return $ str ""
  ]

-- * Jobs

listDrawElement appState ix isSelected x@(MainListElemItem {_typ=(SingleJob job), ..}) = wrapper ix isSelected x [
  Just $ jobLine (_appAnimationCounter appState) _toggled job
  , do
      guard _toggled
      guardFetched _state $ \case
        PaginatedItemJob _ ->
          return $ padLeft (Pad 4) $
            fixedHeightOrViewportPercent (InnerViewport [i|viewport_#{_ident}|]) 50 $
              jobInner job _state
        _ -> return $ str ""
  ]


paginatedHeading ::
  MainListElem
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
paginatedHeading _ _appState _label _countInParens = hBox []

wrapper :: Int -> Bool -> MainListElem' f -> [Maybe (Widget ClickableName)] -> Widget ClickableName
wrapper ix isSelected x = clickable (ListRow ix) . padLeft (Pad (4 * (_depth x))) . (if isSelected then border else id) . vBox . catMaybes
