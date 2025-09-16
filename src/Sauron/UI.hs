
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
listDrawElement appState ix isSelected x@(MainListElemPaginated {..}) = wrapper ix isSelected x [
  Just $ hBox $ catMaybes [
    Just $ withAttr openMarkerAttr $ str (if _toggled then "[-] " else "[+] ")
    , Just $ padRight (Pad 1) $ str $ toString _label
    , Just $ case _items of
        NotFetched -> str "(Not fetched)"
        Fetching -> str "(Fetching)"
        Errored msg -> str [i|Errored: #{msg}|]
        Fetched (PaginatedItemsIssues (SearchResult totalCount _xs)) -> str [i|(#{totalCount})|]
        Fetched (PaginatedItemsPulls (SearchResult totalCount _xs)) -> str [i|(#{totalCount})|]
        Fetched (PaginatedItemsWorkflows xs) -> str [i|(#{withTotalCountTotalCount xs})|]
    , Just (hCenter (padRight (Pad 4) (searchInfo appState _ident _search) <+> paginationInfo _pageInfo))
    ]
  ]
listDrawElement appState ix isSelected x@(MainListElemItem {..}) = wrapper ix isSelected x [
  Just $ case _item of
    Fetched (PaginatedItemIssue issue) -> issueLine (_appNow appState) _toggled issue
    Fetched (PaginatedItemPull pull) -> pullLine (_appNow appState) _toggled pull
    Fetched (PaginatedItemWorkflow wf) -> workflowLine _toggled wf
    _ -> str ""
  , do
      guard _toggled
      guardFetched _item $ \case
        PaginatedItemIssue iss@(Issue {..}) -> guardJust issueBody $ \body ->
          return $ padLeft (Pad 4) $
            fixedHeightOrViewportPercent (InnerViewport [i|viewport_#{_ident}|]) 50 $
              issueInner (_appNow appState) iss body _itemInner
        PaginatedItemPull pr@(Issue {..}) -> guardJust issueBody $ \body ->
          return $ padLeft (Pad 4) $
            fixedHeightOrViewportPercent (InnerViewport [i|viewport_#{_ident}|]) 50 $
              pullInner (_appNow appState) pr body _itemInner
        PaginatedItemWorkflow wf@(WorkflowRun {}) ->
          return $ padLeft (Pad 4) $
            fixedHeightOrViewportPercent (InnerViewport [i|viewport_#{_ident}|]) 50 $
              workflowInner wf _itemInner
  ]

wrapper :: Int -> Bool -> MainListElem' f -> [Maybe (Widget ClickableName)] -> Widget ClickableName
wrapper ix isSelected x = clickable (ListRow ix) . padLeft (Pad (4 * (_depth x))) . (if isSelected then border else id) . vBox . catMaybes
