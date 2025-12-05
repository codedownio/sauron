{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sauron.UI.Pagination (
  paginationInfo
  ) where

import Brick
import Brick.Widgets.Center
import Control.Monad
import qualified Data.List as L
import Data.Maybe
import Data.String.Interpolate
import qualified Data.Vector as V
import GitHub hiding (Status)
import Relude
import Sauron.Fetch.Core (pageSize)
import Sauron.Types
import Sauron.Types.Branches
import Sauron.UI.AttrMap
import Sauron.UI.Search
import Sauron.UI.Statuses (getQuarterCircleSpinner)


instance ListDrawable Fixed 'PaginatedReposT where
  drawLine appState ed@(EntityData {..}) = case _state of
    Fetched (SearchResult totalCount repos) -> paginatedHeading ed appState "Repositories" (str [i|(#{V.length repos}) of #{totalCount}|])
    Fetching maybeRepos -> case maybeRepos of
      Just (SearchResult totalCount repos) -> paginatedHeading ed appState "Repositories" (str [i|(#{V.length repos}) of #{totalCount} |] <+> getQuarterCircleSpinner (_appAnimationCounter appState))
      Nothing -> paginatedHeading ed appState "Repositories" (str "(" <+> getQuarterCircleSpinner (_appAnimationCounter appState) <+> str ")")
    NotFetched -> paginatedHeading ed appState "Repositories" (str [i|(not fetched)|])
    Errored err -> paginatedHeading ed appState "Repositories" (str [i|(error fetching: #{err})|])

  drawInner _ _ = Nothing

instance ListDrawable Fixed 'PaginatedIssuesT where
  drawLine appState x@(EntityData {_state}) = case _state of
    Fetched (SearchResult totalCount _xs) -> paginatedHeading x appState "Issues" (str [i|(#{totalCount})|])
    Fetching maybeIssues -> case maybeIssues of
      Just (SearchResult totalCount _xs) -> paginatedHeading x appState "Issues" (str [i|(#{totalCount}) |] <+> getQuarterCircleSpinner (_appAnimationCounter appState))
      Nothing -> paginatedHeading x appState "Issues" (str "(" <+> getQuarterCircleSpinner (_appAnimationCounter appState) <+> str ")")
    NotFetched -> paginatedHeading x appState "Issues" (str [i|(not fetched)|])
    Errored err -> paginatedHeading x appState "Issues" (str [i|(error fetching: #{err})|])

  drawInner _ _ = Nothing

instance ListDrawable Fixed 'PaginatedPullsT where
  drawLine appState ed@(EntityData {_state}) = case _state of
    Fetched (SearchResult totalCount _xs) -> paginatedHeading ed appState "Pulls" (str [i|(#{totalCount})|])
    Fetching maybePulls -> case maybePulls of
      Just (SearchResult totalCount _xs) -> paginatedHeading ed appState "Pulls" (str [i|(#{totalCount}) |] <+> getQuarterCircleSpinner (_appAnimationCounter appState))
      Nothing -> paginatedHeading ed appState "Pulls" (str "(" <+> getQuarterCircleSpinner (_appAnimationCounter appState) <+> str ")")
    NotFetched -> paginatedHeading ed appState "Pulls" (str [i|(not fetched)|])
    Errored err -> paginatedHeading ed appState "Pulls" (str [i|(error fetching: #{err})|])

  drawInner _ _ = Nothing

instance ListDrawable Fixed 'PaginatedWorkflowsT where
  drawLine appState ed@(EntityData {..}) = case _state of
    Fetched (WithTotalCount _xs totalCount) -> paginatedHeading ed appState "Actions" (str [i|(#{totalCount})|])
    Fetching maybeWorkflows -> case maybeWorkflows of
      Just (WithTotalCount _xs totalCount) -> paginatedHeading ed appState "Actions" (str [i|(#{totalCount}) |] <+> getQuarterCircleSpinner (_appAnimationCounter appState))
      Nothing -> paginatedHeading ed appState "Actions" (str "(" <+> getQuarterCircleSpinner (_appAnimationCounter appState) <+> str ")")
    NotFetched -> paginatedHeading ed appState "Actions" (str [i|(not fetched)|])
    Errored err -> paginatedHeading ed appState "Actions" (str [i|(error fetching: #{err})|])

  drawInner _ _ = Nothing

instance ListDrawable Fixed 'PaginatedBranchesT where
  drawLine appState ed@(EntityData {..}) = case _state of
    Fetched branches -> paginatedHeading ed appState "All Branches" (countWidget _pageInfo branches)
    Fetching maybeBranches -> case maybeBranches of
      Just branches -> paginatedHeading ed appState "All Branches" (countWidget _pageInfo branches <+> str " " <+> getQuarterCircleSpinner (_appAnimationCounter appState))
      Nothing -> paginatedHeading ed appState "All Branches" (str "(" <+> getQuarterCircleSpinner (_appAnimationCounter appState) <+> str ")")
    NotFetched -> paginatedHeading ed appState "All Branches" (str [i|(not fetched)|])
    Errored err -> paginatedHeading ed appState "All Branches" (str [i|(error fetching: #{err})|])

  drawInner _ _ = Nothing

instance ListDrawable Fixed 'OverallBranchesT where
  drawLine _appState (EntityData {_static=(), _toggled}) = hBox $ catMaybes [
    Just $ withAttr openMarkerAttr $ str (if _toggled then "[-] " else "[+] ")
    , Just (hBox [str "GitHub-style Branches"])
    , Just (padLeft Max (str " "))
    ]

  drawInner _ _ = Nothing

instance ListDrawable Fixed 'PaginatedYourBranchesT where
  drawLine appState ed@(EntityData {..}) = case _state of
    Fetched branchesPayload -> paginatedHeading ed appState "Your branches" (branchPayloadCountWidget _pageInfo branchesPayload)
    Fetching maybeBranchesPayload -> case maybeBranchesPayload of
      Just branchesPayload -> paginatedHeading ed appState "Your branches" (branchPayloadCountWidget _pageInfo branchesPayload <+> str " " <+> getQuarterCircleSpinner (_appAnimationCounter appState))
      Nothing -> paginatedHeading ed appState "Your branches" (str "(" <+> getQuarterCircleSpinner (_appAnimationCounter appState) <+> str ")")
    NotFetched -> paginatedHeading ed appState "Your branches" (str [i|(not fetched)|])
    Errored err -> paginatedHeading ed appState "Your branches" (str [i|(error fetching: #{err})|])

  drawInner _ _ = Nothing

instance ListDrawable Fixed 'PaginatedActiveBranchesT where
  drawLine appState ed@(EntityData {..}) = case _state of
    Fetched branchesPayload -> paginatedHeading ed appState "Active branches" (branchPayloadCountWidget _pageInfo branchesPayload)
    Fetching maybeBranchesPayload -> case maybeBranchesPayload of
      Just branchesPayload -> paginatedHeading ed appState "Active branches" (branchPayloadCountWidget _pageInfo branchesPayload <+> str " " <+> getQuarterCircleSpinner (_appAnimationCounter appState))
      Nothing -> paginatedHeading ed appState "Active branches" (str "(" <+> getQuarterCircleSpinner (_appAnimationCounter appState) <+> str ")")
    NotFetched -> paginatedHeading ed appState "Active branches" (str [i|(not fetched)|])
    Errored err -> paginatedHeading ed appState "Active branches" (str [i|(error fetching: #{err})|])

  drawInner _ _ = Nothing

instance ListDrawable Fixed 'PaginatedStaleBranchesT where
  drawLine appState ed@(EntityData {..}) = case _state of
    Fetched branchesPayload -> paginatedHeading ed appState "Stale branches" (branchPayloadCountWidget _pageInfo branchesPayload)
    Fetching maybeBranchesPayload -> case maybeBranchesPayload of
      Just branchesPayload -> paginatedHeading ed appState "Stale branches" (branchPayloadCountWidget _pageInfo branchesPayload <+> str " " <+> getQuarterCircleSpinner (_appAnimationCounter appState))
      Nothing -> paginatedHeading ed appState "Stale branches" (str "(" <+> getQuarterCircleSpinner (_appAnimationCounter appState) <+> str ")")
    NotFetched -> paginatedHeading ed appState "Stale branches" (str [i|(not fetched)|])
    Errored err -> paginatedHeading ed appState "Stale branches" (str [i|(error fetching: #{err})|])

  drawInner _ _ = Nothing

instance ListDrawable Fixed 'PaginatedNotificationsT where
  drawLine appState ed@(EntityData {..}) = case _state of
    Fetched notifications -> paginatedHeading ed appState "Notifications" (countWidget _pageInfo notifications)
    Fetching maybeNotifications -> case maybeNotifications of
      Just notifications -> paginatedHeading ed appState "Notifications" (countWidget _pageInfo notifications <+> str " " <+> getQuarterCircleSpinner (_appAnimationCounter appState))
      Nothing -> paginatedHeading ed appState "Notifications" (str "(" <+> getQuarterCircleSpinner (_appAnimationCounter appState) <+> str ")")
    NotFetched -> paginatedHeading ed appState "Notifications" (str [i|(not fetched)|])
    Errored err -> paginatedHeading ed appState "Notifications" (str [i|(error fetching: #{err})|])

  drawInner _ _ = Nothing

instance ListDrawable Fixed 'HeadingT where
  drawLine _appState (EntityData {_static=label, ..}) = hBox $ catMaybes [
    Just $ withAttr openMarkerAttr $ str (if _toggled then "[-] " else "[+] ")
    , Just (hBox [str (toString label)])
    , Just (padLeft Max (str " "))
    ]

  drawInner _ _ = Nothing


-- Helper functions

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

branchPayloadCountWidget :: PageInfo -> GitHubBranchesPayload -> Widget n
branchPayloadCountWidget pageInfo' branchesPayload = case pageInfoLastPage pageInfo' of
  Just lastPage -> str [i|(~#{lastPage * pageSize})|]
  Nothing -> str [i|(#{length (gitHubBranchesPayloadBranches branchesPayload)})|]


paginationInfo :: PageInfo -> Widget n
paginationInfo (PageInfo {..}) =
  fmap renderSegment pageNumbers
  & ((str "Page:") :)
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
