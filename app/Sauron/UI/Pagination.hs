{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sauron.UI.Pagination (
  paginationInfo
  ) where

import Brick
import Brick.Forms
import Brick.Widgets.Center (hCenter)
import Control.Monad
import qualified Data.List as L
import Data.Maybe
import Data.String.Interpolate
import GitHub
import qualified Graphics.Vty as V
import Lens.Micro
import Relude
import Sauron.Actions.Util (findRepoParent)
import Sauron.Event.Helpers (withFixedElemAndParents)
import Sauron.Event.NewIssueModal (openNewIssueModal)
import Sauron.Event.Search (ensureNonEmptySearch)
import Sauron.Types
import Sauron.UI.Search (searchInfo)
import Sauron.UI.AttrMap
import Sauron.UI.Keys
import Sauron.UI.Statuses (getQuarterCircleSpinner)


instance ListDrawable Fixed 'PaginatedReposT where
  drawLine appState ed@(EntityData {_state=(search, pageInfo, fetchable)}) =
    drawPaginatedLine Nothing "Repositories" appState ed search pageInfo fetchable
  getExtraTopBoxWidgets _ _ = searchableExtraWidgets
  handleHotkey s key ed = searchableHandleHotkey s key ed

instance ListDrawable Fixed 'PaginatedIssuesT where
  drawLine appState ed@(EntityData {_static=label, _state=(search, pageInfo, fetchable)}) =
    drawPaginatedLine (Just jumpToIssuesKey) (toString label) appState ed search pageInfo fetchable
  getExtraTopBoxWidgets _ _ = searchableExtraWidgets ++
    [hBox [str "["
          , withAttr hotkeyAttr $ str $ showKey newIssueKey
          , str "] "
          , withAttr hotkeyMessageAttr $ str "New issue"
          ]
    ]
  handleHotkey s key ed
    | key == newIssueKey = do
        withFixedElemAndParents s $ \_ _ parents ->
          case findRepoParent parents of
            Just (RepoNode (EntityData {_static=(owner, name)})) ->
              openNewIssueModal owner name
            _ -> return ()
        return True
    | otherwise = searchableHandleHotkey s key ed

instance ListDrawable Fixed 'PaginatedPullsT where
  drawLine appState ed@(EntityData {_static=label, _state=(search, pageInfo, fetchable)}) =
    drawPaginatedLine (Just jumpToPullsKey) (toString label) appState ed search pageInfo fetchable
  getExtraTopBoxWidgets _ _ = searchableExtraWidgets
  handleHotkey s key ed = searchableHandleHotkey s key ed

instance ListDrawable Fixed 'PaginatedWorkflowsT where
  drawLine appState ed@(EntityData {_state=(search, pageInfo, fetchable)}) =
    drawPaginatedLine (Just jumpToActionsKey) "Actions" appState ed search pageInfo fetchable
  -- Workflows are not searchable

instance ListDrawable Fixed 'PaginatedBranchesT where
  drawLine appState ed@(EntityData {_state=(search, pageInfo, fetchable)}) =
    drawPaginatedLine (Just jumpToBranchesKey) "All Branches" appState ed search pageInfo fetchable
  getExtraTopBoxWidgets _ _ = searchableExtraWidgets
  handleHotkey s key ed = searchableHandleHotkey s key ed

instance ListDrawable Fixed 'PaginatedYourBranchesT where
  drawLine appState ed@(EntityData {_state=(search, pageInfo, fetchable)}) =
    drawPaginatedLine (Just jumpToBranchesKey) "Your branches" appState ed search pageInfo fetchable
  getExtraTopBoxWidgets _ _ = searchableExtraWidgets
  handleHotkey s key ed = searchableHandleHotkey s key ed

instance ListDrawable Fixed 'PaginatedActiveBranchesT where
  drawLine appState ed@(EntityData {_state=(search, pageInfo, fetchable)}) =
    drawPaginatedLine (Just jumpToBranchesKey) "Active branches" appState ed search pageInfo fetchable
  getExtraTopBoxWidgets _ _ = searchableExtraWidgets
  handleHotkey s key ed = searchableHandleHotkey s key ed

instance ListDrawable Fixed 'PaginatedStaleBranchesT where
  drawLine appState ed@(EntityData {_state=(search, pageInfo, fetchable)}) =
    drawPaginatedLine (Just jumpToBranchesKey) "Stale branches" appState ed search pageInfo fetchable
  getExtraTopBoxWidgets _ _ = searchableExtraWidgets
  handleHotkey s key ed = searchableHandleHotkey s key ed

instance ListDrawable Fixed 'PaginatedNotificationsT where
  drawLine appState ed@(EntityData {_state=(search, pageInfo, fetchable), _children}) =
    drawNotificationsLine appState ed search pageInfo fetchable _children
  getExtraTopBoxWidgets _ _ = searchableExtraWidgets
  handleHotkey s key ed = searchableHandleHotkey s key ed

instance ListDrawable Fixed 'HeadingT where
  drawLine _appState (EntityData {_static=label, ..}) = hBox $ catMaybes [
    Just $ withAttr openMarkerAttr $ str (if _toggled then "[-] " else "[+] ")
    , Just (hBox [withAttr (mkAttrName "headingText") $ str (toString label)])
    , Just (padLeft Max (str " "))
    ]

-- Helper functions

drawPaginatedLine :: Maybe V.Key -> String -> AppState -> EntityData Fixed a -> Search -> PageInfo -> Fetchable Int -> Widget ClickableName
drawPaginatedLine maybeJumpKey label appState ed search pageInfo fetchable = case fetchable of
  Fetched totalCount -> headingWithMessage (str [i|(#{totalCount})|]) Nothing
  Fetching (Just totalCount) -> headingWithMessage (str [i|(#{totalCount})|]) (Just $ getQuarterCircleSpinner (_appAnimationCounter appState))
  Fetching Nothing -> headingWithMessage (str "(-)") (Just $ getQuarterCircleSpinner (_appAnimationCounter appState))
  NotFetched -> headingWithMessage (str [i|(not fetched)|]) Nothing
  Errored err -> headingWithMessage (str [i|(error fetching: #{err})|]) Nothing
  where
    headingWithMessage msg spinner = paginatedHeading' maybeJumpKey spinner (withAttr (mkAttrName "headingText")) ed appState label msg search pageInfo

paginatedHeading' ::
  Maybe V.Key
  -> Maybe (Widget ClickableName)
  -> (Widget ClickableName -> Widget ClickableName)
  -> EntityData Fixed a
  -> AppState
  -> String
  -> Widget ClickableName
  -> Search
  -> PageInfo
  -> Widget ClickableName
paginatedHeading' maybeJumpKey maybeSpinner modifyLabel (EntityData {..}) appState l countInParens _search _pageInfo = hBox $ catMaybes [
  Just $ withAttr openMarkerAttr $ str (if _toggled then "[-] " else "[+] ")
  , Just $ padRight (Pad 1) $ modifyLabel $ str l
  , Just countInParens
  , fmap (\k -> hBox [str " [", withAttr hotkeyAttr $ str $ showKey k, str "]"]) maybeJumpKey
  , fmap (\s -> str " " <+> s) maybeSpinner
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

-- Special function for drawing notifications line with unread indicator
drawNotificationsLine :: AppState -> EntityData Fixed PaginatedNotificationsT -> Search -> PageInfo -> Fetchable Int -> [Node Fixed SingleNotificationT] -> Widget ClickableName
drawNotificationsLine appState ed search pageInfo fetchable children' = case fetchable of
  Fetched totalCount -> headingWithMessage (str [i|(#{totalCount})|] <+> unreadIndicator) Nothing
  Fetching (Just totalCount) -> headingWithMessage (str [i|(#{totalCount})|] <+> unreadIndicator) (Just $ getQuarterCircleSpinner (_appAnimationCounter appState))
  Fetching Nothing -> headingWithMessage (str "(-)" <+> unreadIndicator) (Just $ getQuarterCircleSpinner (_appAnimationCounter appState))
  NotFetched -> headingWithMessage (str [i|(not fetched)|]) Nothing
  Errored err -> headingWithMessage (str [i|(error fetching: #{err})|]) Nothing
  where
    headingWithMessage msg spinner = paginatedHeading' Nothing spinner (withAttr (mkAttrName "headingText")) ed appState "Notifications" msg search pageInfo

    unreadIndicator = case hasUnreadNotifications children' of
      True -> withAttr blueDotAttr $ str " ●"
      False -> emptyWidget

    hasUnreadNotifications :: [Node Fixed SingleNotificationT] -> Bool
    hasUnreadNotifications = any checkNotification
      where
        checkNotification :: Node Fixed SingleNotificationT -> Bool
        checkNotification (SingleNotificationNode (EntityData {_static=notification})) = notificationUnread notification

-- * Search functionality helpers for paginated nodes

searchableExtraWidgets :: [Widget ClickableName]
searchableExtraWidgets =
  [hBox [str "["
        , withAttr hotkeyAttr $ str $ showKey editSearchKey
        , str "] "
        , withAttr hotkeyMessageAttr $ str "Search"
        ]
  ]

searchableHandleHotkey :: AppState -> V.Key -> EntityData Fixed a -> EventM ClickableName AppState Bool
searchableHandleHotkey s key (EntityData {_ident})
  | key == editSearchKey = do
      withFixedElemAndParents s $ \_ (SomeNode variableEl) _ -> do
        searchText <- liftIO $ atomically $ ensureNonEmptySearch variableEl
        modify (appForm ?~ (newForm [editTextField id TextForm (Just 1)] searchText, _ident))
      return True
searchableHandleHotkey _ _ _ = return False
