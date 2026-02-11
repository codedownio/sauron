{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Sauron.UI.TopBox (
  topBox

  , highlightKeyIfPredicate
  , highlightMessageIfPredicate

  , isSearchable
  , isSearchable'
  ) where

import Brick
import Brick.Widgets.List
import qualified Data.List as L
import qualified Data.Vector as V
import Lens.Micro
import Relude
import Sauron.Event.Helpers (isPaginationNode)
import Sauron.Types
import Sauron.UI.AttrMap
import Sauron.UI.Branch ()
import Sauron.UI.BranchWithInfo ()
import Sauron.UI.Commit ()
import Sauron.UI.Issue ()
import Sauron.UI.Job ()
import Sauron.UI.Keys
import Sauron.UI.Notification ()
import Sauron.UI.Pagination ()
import Sauron.UI.Pull ()
import Sauron.UI.Repo ()
import Sauron.UI.Workflow ()


topBox app = hBox [columnPadding column1
                  , columnPadding column2
                  , columnPadding column3]
  where
    column1 = keybindingBox [keyIndicator (L.intersperse '/' [unKChar nextKey, unKChar previousKey, '↑', '↓']) "Navigate"
                            , keyIndicator (showKeys toggleKeys <> "/←/→") "Open/close node"
                            , keyIndicatorHasSelectedRepoOpen app "Control-v/Meta-v" "Scroll node"
                            , keyIndicator "q" "Exit"
                            ]

    column2 = keybindingBox [hBox [str "["
                                  , highlightKeyIfPredicate someRepoSelected app (str $ showKey browserToHomeKey)
                                  , str "/"
                                  , highlightKeyIfPredicate someRepoSelected app (str $ showKey browserToIssuesKey)
                                  , str "/"
                                  , highlightKeyIfPredicate someRepoSelected app (str $ showKey browserToPullsKey)
                                  , str "/"
                                  , highlightKeyIfPredicate someRepoSelected app (str $ showKey browserToActionsKey)
                                  , str "] "
                                  , highlightMessageIfPredicate someRepoSelected app (str "Open ")
                                  , highlightMessageIfPredicate someRepoSelected app (str "repo")
                                  , str "/"
                                  , highlightMessageIfPredicate someRepoSelected app (str "issues")
                                  , str "/"
                                  , highlightMessageIfPredicate someRepoSelected app (str "pulls")
                                  , str "/"
                                  , highlightMessageIfPredicate someRepoSelected app (str "actions")
                                  ]
                            , hBox [str "["
                                   , highlightKeyIfPredicate hasNextPageKey app (str $ showKey nextPageKey)
                                   , str "/"
                                   , highlightKeyIfPredicate hasPrevPageKey app (str $ showKey prevPageKey)
                                   , str "/"
                                   , highlightKeyIfPredicate hasFirstPageKey app (str $ showKey firstPageKey)
                                   , str "/"
                                   , highlightKeyIfPredicate hasLastPageKey app (str $ showKey lastPageKey)
                                   , str "] "

                                   , highlightMessageIfPredicate hasNextPageKey app (str "Next")
                                   , str "/"
                                   , highlightMessageIfPredicate hasPrevPageKey app (str "Previous")
                                   , str "/"
                                   , highlightMessageIfPredicate hasFirstPageKey app (str "First")
                                   , str "/"
                                   , highlightMessageIfPredicate hasLastPageKey app (str "Last")
                                   , highlightMessageIfPredicate hasNextPageKey app (str " page")
                                   ]
                            , hBox [str "["
                                   , highlightKeyIfPredicate someRepoSelected app (str $ showKey refreshSelectedKey)
                                   , str "/"
                                   , highlightKeyIfPredicate (const True) app (str $ showKey refreshAllKey)
                                   , str "] "
                                   , withAttr hotkeyMessageAttr $ str "Refresh "
                                   , highlightMessageIfPredicate someRepoSelected app (str "selected")
                                   , str "/"
                                   , highlightMessageIfPredicate (const True) app (str "all")
                                   ]
                            , hBox [str "["
                                   , highlightKeyIfPredicate someRepoSelected app (str $ showKey openSelectedKey)
                                   , str "] "
                                   , withAttr hotkeyMessageAttr $ str "Open "
                                   , highlightMessageIfPredicate someRepoSelected app (str "selected")
                                   ]
                            ]

    column3 = keybindingBox $ getExtraTopBoxWidgetsForSelected app

getExtraTopBoxWidgetsForSelected :: AppState -> [Widget ClickableName]
getExtraTopBoxWidgetsForSelected s = case snd <$> listSelectedElement (s ^. appMainList) of
  Nothing -> []
  Just sn -> getExtraTopBoxWidgetsForSomeNode s sn

getExtraTopBoxWidgetsForSomeNode :: AppState -> SomeNode Fixed -> [Widget ClickableName]
getExtraTopBoxWidgetsForSomeNode s (SomeNode node) = case node of
  HeadingNode ed -> getExtraTopBoxWidgets s ed
  RepoNode ed -> getExtraTopBoxWidgets s ed
  PaginatedIssuesNode ed -> getExtraTopBoxWidgets s ed
  PaginatedPullsNode ed -> getExtraTopBoxWidgets s ed
  PaginatedWorkflowsNode ed -> getExtraTopBoxWidgets s ed
  PaginatedReposNode ed -> getExtraTopBoxWidgets s ed
  PaginatedBranchesNode ed -> getExtraTopBoxWidgets s ed
  PaginatedYourBranchesNode ed -> getExtraTopBoxWidgets s ed
  PaginatedActiveBranchesNode ed -> getExtraTopBoxWidgets s ed
  PaginatedStaleBranchesNode ed -> getExtraTopBoxWidgets s ed
  PaginatedNotificationsNode ed -> getExtraTopBoxWidgets s ed
  SingleIssueNode ed -> getExtraTopBoxWidgets s ed
  SinglePullNode ed -> getExtraTopBoxWidgets s ed
  SingleWorkflowNode ed -> getExtraTopBoxWidgets s ed
  SingleJobNode ed -> getExtraTopBoxWidgets s ed
  SingleBranchNode ed -> getExtraTopBoxWidgets s ed
  SingleBranchWithInfoNode ed -> getExtraTopBoxWidgets s ed
  SingleCommitNode ed -> getExtraTopBoxWidgets s ed
  SingleNotificationNode ed -> getExtraTopBoxWidgets s ed
  JobLogGroupNode ed -> getExtraTopBoxWidgets s ed

hasNextPageKey :: AppState -> Bool
hasNextPageKey = hasAncestorMatching isPaginationNode
hasPrevPageKey :: AppState -> Bool
hasPrevPageKey = hasAncestorMatching isPaginationNode
hasFirstPageKey :: AppState -> Bool
hasFirstPageKey = hasAncestorMatching isPaginationNode
hasLastPageKey :: AppState -> Bool
hasLastPageKey = hasAncestorMatching isPaginationNode

isSearchable s = case snd <$> listSelectedElement (s ^. appMainList) of
  Nothing -> False
  Just x -> isSearchable' x

isSearchable' :: SomeNode f -> Bool
isSearchable' (SomeNode (PaginatedIssuesNode {})) = True
isSearchable' (SomeNode (PaginatedPullsNode {})) = True
isSearchable' (SomeNode (PaginatedWorkflowsNode {})) = False
isSearchable' (SomeNode (PaginatedReposNode {})) = True
isSearchable' (SomeNode (PaginatedBranchesNode {})) = True
isSearchable' (SomeNode (PaginatedYourBranchesNode {})) = True
isSearchable' (SomeNode (PaginatedActiveBranchesNode {})) = True
isSearchable' (SomeNode (PaginatedStaleBranchesNode {})) = True
isSearchable' (SomeNode (PaginatedNotificationsNode {})) = True
isSearchable' _ = False

columnPadding = padLeft (Pad 1) . padRight (Pad 3) -- . padTop (Pad 1)

keybindingBox = vBox


highlightKeyIfPredicate p app x = case p app of
  True -> withAttr hotkeyAttr x
  False -> withAttr disabledHotkeyAttr x

highlightMessageIfPredicate p app x = case p app of
  True -> withAttr hotkeyMessageAttr x
  False -> withAttr disabledHotkeyMessageAttr x

keyIndicator key msg = keyIndicator' key (withAttr hotkeyMessageAttr $ str msg)

keyIndicator' key l = hBox [str "[", withAttr hotkeyAttr $ str key, str "] ", l]

keyIndicatorHasSelectedRepoOpen app = keyIndicatorContextual app selectedRepoToggled

keyIndicatorContextual app p key msg = case p app of
  True -> hBox [str "[", withAttr hotkeyAttr $ str key, str "] ", withAttr hotkeyMessageAttr $ str msg]
  False -> hBox [str "[", withAttr disabledHotkeyAttr $ str key, str "] ", withAttr disabledHotkeyMessageAttr $ str msg]

-- * Predicates

-- | Check if the selected element matches a predicate or has an ancestor that does.
-- Finds ancestors by walking backwards through the flattened list, collecting
-- the first node at each decreasing depth level.
--
-- Note that we can't use nthChildVector on this, because we want the function to be
-- pure, and thus we have to work with appMainList. But appMainList is flattened
-- (whereas appMainListVariable is a tree).
hasAncestorMatching :: (forall a. Node Fixed a -> Bool) -> AppState -> Bool
hasAncestorMatching predicate s = case listSelectedElement (s ^. appMainList) of
  Nothing -> False
  Just (idx, SomeNode selected) ->
    predicate selected || any (\(SomeNode n) -> predicate n) ancestors
    where
      selectedDepth = _depth (getEntityData selected)
      preceding = V.toList $ V.take idx (listElements (s ^. appMainList))
      ancestors = findAncestors (selectedDepth - 1) (L.reverse preceding)

      -- Walk backwards through the list, collecting the first node at each depth level
      findAncestors :: Int -> [SomeNode Fixed] -> [SomeNode Fixed]
      findAncestors targetDepth _ | targetDepth < 0 = []
      findAncestors targetDepth nodes =
        case L.dropWhile (\(SomeNode n) -> _depth (getEntityData n) /= targetDepth) nodes of
          [] -> []
          (ancestor : rest) -> ancestor : findAncestors (targetDepth - 1) rest

isRepoNode :: Node f a -> Bool
isRepoNode (RepoNode {}) = True
isRepoNode _ = False

someRepoSelected :: AppState -> Bool
someRepoSelected = hasAncestorMatching isRepoNode

selectedRepoToggled = const False
