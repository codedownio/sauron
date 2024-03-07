
module Sauron.UI.Draw.Pagination (
  paginationInfo
  ) where

import Brick
import qualified Data.List as L
import Data.String.Interpolate
import Relude
import Sauron.Types
import Sauron.UI.AttrMap


paginationInfo :: PageInfo -> Widget n
paginationInfo (PageInfo {..}) =
  fmap renderSegment pageNumbers
  & ((str "Page ") :)
  & L.intersperse (str " ")
  & hBox
  where
    pageNumbers :: [PageSegment]
    pageNumbers = catMaybes [pageInfoFirstPage, pageInfoPrevPage, Just pageInfoCurrentPage, pageInfoNextPage, pageInfoLastPage]
                & sort
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
