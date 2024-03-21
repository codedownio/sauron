
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
import Data.Time
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
import Sauron.UI.TopBox
import Sauron.UI.Util
import Sauron.UI.Workflow


drawUI :: AppState -> [Widget ClickableName]
drawUI app = [vBox [
                 topBox app
                 , borderWithCounts app
                 , hCenter $ padAll 1 $ L.renderListWithIndex (listDrawElement (_appNow app)) True (app ^. appMainList)
                 , clickable InfoBar $ bottomBar app
                 ]
             ]

listDrawElement :: UTCTime -> Int -> Bool -> MainListElem -> Widget ClickableName
listDrawElement _now ix isSelected x@(MainListElemHeading {..}) = wrapper ix isSelected x [
  Just $ hBox $ catMaybes [
    Just $ withAttr openMarkerAttr $ str (if _toggled then "[-] " else "[+] ")
    , Just (hBox [str (toString _label)])
    , Just (padLeft Max (str " "))
    ]
  ]
listDrawElement _now ix isSelected x@(MainListElemRepo {..}) = wrapper ix isSelected x [
  Just $ renderRepoLine _toggled _namespaceName _repo _healthCheck
  ]
listDrawElement _now ix isSelected x@(MainListElemPaginated {..}) = wrapper ix isSelected x [
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
    , Just (hCenter (paginationInfo _pageInfo))
    ]
  ]
listDrawElement now ix isSelected x@(MainListElemItem {..}) = wrapper ix isSelected x [
  Just $ case _item of
    Fetched (PaginatedItemIssue issue) -> issueLine now _toggled issue
    Fetched (PaginatedItemPull pull) -> pullLine now _toggled pull
    Fetched (PaginatedItemWorkflow wf) -> workflowLine _toggled wf
    _ -> str ""
  , do
      guard _toggled
      guardFetched _item $ \case
        PaginatedItemIssue iss@(Issue {..}) -> guardJust issueBody $ \body ->
          return $ padLeft (Pad 4) $
            fixedHeightOrViewportPercent (InnerViewport [i|viewport_#{_ident}|]) 50 $
              issueInner now iss body _itemInner
        PaginatedItemPull pr@(Issue {..}) -> guardJust issueBody $ \body ->
          return $ padLeft (Pad 4) $
            fixedHeightOrViewportPercent (InnerViewport [i|viewport_#{_ident}|]) 50 $
              pullInner now pr body _itemInner
        PaginatedItemWorkflow wf@(WorkflowRun {}) ->
          return $ padLeft (Pad 4) $
            fixedHeightOrViewportPercent (InnerViewport [i|viewport_#{_ident}|]) 50 $
              workflowInner wf
  ]

wrapper :: Int -> Bool -> MainListElem' f -> [Maybe (Widget ClickableName)] -> Widget ClickableName
wrapper ix isSelected x = clickable (ListRow ix) . padLeft (Pad (4 * (_depth x))) . (if isSelected then border else id) . vBox . catMaybes
