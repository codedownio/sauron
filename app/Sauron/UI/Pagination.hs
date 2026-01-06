{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sauron.UI.Pagination (
  paginationInfo
  ) where

import Brick
import Brick.Widgets.Center (hCenter)
import Control.Monad
import qualified Data.List as L
import Data.Maybe
import Data.String.Interpolate
import Relude
import Sauron.Types
import Sauron.UI.AttrMap
import Sauron.UI.Search (searchInfo)
import Sauron.UI.Statuses (getQuarterCircleSpinner)


instance ListDrawable Fixed 'PaginatedReposT where
  drawLine appState ed@(EntityData {_state=(search, pageInfo, fetchable)}) =
    drawPaginatedLine "Repositories" appState ed search pageInfo fetchable

  drawInner _ _ = Nothing

instance ListDrawable Fixed 'PaginatedIssuesT where
  drawLine appState ed@(EntityData {_state=(search, pageInfo, fetchable)}) =
    drawPaginatedLine "Issues" appState ed search pageInfo fetchable

  drawInner _ _ = Nothing

instance ListDrawable Fixed 'PaginatedPullsT where
  drawLine appState ed@(EntityData {_state=(search, pageInfo, fetchable)}) =
    drawPaginatedLine "Pulls" appState ed search pageInfo fetchable

  drawInner _ _ = Nothing

instance ListDrawable Fixed 'PaginatedWorkflowsT where
  drawLine appState ed@(EntityData {_state=(search, pageInfo, fetchable)}) =
    drawPaginatedLine "Actions" appState ed search pageInfo fetchable

  drawInner _ _ = Nothing

instance ListDrawable Fixed 'PaginatedBranchesT where
  drawLine appState ed@(EntityData {_state=(search, pageInfo, fetchable)}) =
    drawPaginatedLine "All Branches" appState ed search pageInfo fetchable

  drawInner _ _ = Nothing

instance ListDrawable Fixed 'PaginatedYourBranchesT where
  drawLine appState ed@(EntityData {_state=(search, pageInfo, fetchable)}) =
    drawPaginatedLine "Your branches" appState ed search pageInfo fetchable

  drawInner _ _ = Nothing

instance ListDrawable Fixed 'PaginatedActiveBranchesT where
  drawLine appState ed@(EntityData {_state=(search, pageInfo, fetchable)}) =
    drawPaginatedLine "Active branches" appState ed search pageInfo fetchable

  drawInner _ _ = Nothing

instance ListDrawable Fixed 'PaginatedStaleBranchesT where
  drawLine appState ed@(EntityData {_state=(search, pageInfo, fetchable)}) =
    drawPaginatedLine "Stale branches" appState ed search pageInfo fetchable

  drawInner _ _ = Nothing

instance ListDrawable Fixed 'PaginatedNotificationsT where
  drawLine appState ed@(EntityData {_state=(search, pageInfo, fetchable)}) =
    drawPaginatedLine "Notifications" appState ed search pageInfo fetchable

  drawInner _ _ = Nothing

instance ListDrawable Fixed 'HeadingT where
  drawLine _appState (EntityData {_static=label, ..}) = hBox $ catMaybes [
    Just $ withAttr openMarkerAttr $ str (if _toggled then "[-] " else "[+] ")
    , Just (hBox [withAttr (mkAttrName "headingText") $ str (toString label)])
    , Just (padLeft Max (str " "))
    ]

  drawInner _ _ = Nothing


-- Helper functions

drawPaginatedLine :: String -> AppState -> EntityData Fixed a -> Search -> PageInfo -> Fetchable Int -> Widget ClickableName
drawPaginatedLine label appState ed search pageInfo fetchable = case fetchable of
  Fetched totalCount -> headingWithMessage (str [i|(#{totalCount})|])
  Fetching (Just totalCount) -> headingWithMessage (str [i|(#{totalCount}) |] <+> getQuarterCircleSpinner (_appAnimationCounter appState))
  Fetching Nothing -> headingWithMessage (str "(" <+> getQuarterCircleSpinner (_appAnimationCounter appState) <+> str ")")
  NotFetched -> headingWithMessage (str [i|(not fetched)|])
  Errored err -> headingWithMessage (str [i|(error fetching: #{err})|])
  where
    headingWithMessage msg = paginatedHeadingColored ed appState label msg search pageInfo

type PaginatedHeadingFn a =
  EntityData Fixed a
  -> AppState
  -> String
  -> Widget ClickableName
  -> Search
  -> PageInfo
  -> Widget ClickableName

paginatedHeadingColored :: PaginatedHeadingFn a
paginatedHeadingColored = paginatedHeading' (withAttr (mkAttrName "headingText"))

paginatedHeading' ::
  (Widget ClickableName -> Widget ClickableName)
  -> PaginatedHeadingFn a
paginatedHeading' modifyLabel (EntityData {..}) appState l countInParens _search _pageInfo = hBox $ catMaybes [
  Just $ withAttr openMarkerAttr $ str (if _toggled then "[-] " else "[+] ")
  , Just $ padRight (Pad 1) $ modifyLabel $ str l
  , Just countInParens
  , Just (hCenter (padRight (Pad 4) (searchInfo appState _ident _search) <+> paginationInfo _pageInfo))
  ]

paginationInfo :: PageInfo -> Widget n
paginationInfo (PageInfo {..}) =
  fmap renderSegment pageNumbers
  & (str "Page:" :)
  & L.intersperse (str " ")
  & hBox
  where
    pageNumbers :: [PageSegment]
    pageNumbers = catMaybes [pageInfoFirstPage, pageInfoPrevPage, Just pageInfoCurrentPage, pageInfoNextPage, pageInfoLastPage]
                & sort
                & L.nub
                & insertEllipses

    insertEllipses :: [Int] -> [PageSegment]
    insertEllipses (x:y:xs)
      | x == y - 1 = PageSegmentNumber x : insertEllipses (y:xs)
      | otherwise = PageSegmentNumber x : PageSegmentEllipses : insertEllipses (y:xs)
    insertEllipses [x] = [PageSegmentNumber x]
    insertEllipses [] = []

    renderSegment (PageSegmentNumber x)
      | x == pageInfoCurrentPage = withAttr selectedPageAttr $ str [i|#{x}|]
      | otherwise = withAttr notSelectedPageAttr $ str [i|#{x}|]
    renderSegment PageSegmentEllipses = withAttr pageEllipsesAttr $ str "..."

data PageSegment =
  PageSegmentNumber Int
  | PageSegmentEllipses
