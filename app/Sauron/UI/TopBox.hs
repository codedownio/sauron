{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Sauron.UI.TopBox (
  topBox

  , isSearchable'
  ) where

import Brick
import Brick.Widgets.List
import qualified Data.List as L
import Lens.Micro
import Relude
import Sauron.Types
import Sauron.UI.AttrMap
import Sauron.UI.Keys


topBox app = hBox [columnPadding settingsColumn
                  , columnPadding actionsColumn
                  , columnPadding otherActionsColumn]
  where
    settingsColumn = keybindingBox [keyIndicator (L.intersperse '/' [unKChar nextKey, unKChar previousKey, '↑', '↓']) "Navigate"
                                   , keyIndicatorHasSelected app (showKeys toggleKeys <> "/←/→") "Open/close node"
                                   , keyIndicatorHasSelectedOpen app "Control-v/Meta-v" "Scroll node"
                                   ]

    actionsColumn = keybindingBox [hBox [str "["
                                         , highlightKeyIfPredicate someRepoSelected app (str $ showKey browserToHomeKey)
                                         , str "/"
                                         , highlightKeyIfPredicate someRepoSelected app (str $ showKey browserToIssuesKey)
                                         , str "/"
                                         , highlightKeyIfPredicate someRepoSelected app (str $ showKey browserToPullsKey)
                                         , str "/"
                                         , highlightKeyIfPredicate someRepoSelected app (str $ showKey browserToActionsKey)
                                         , str "] "
                                         , withAttr hotkeyMessageAttr $ str "Open "
                                         , highlightMessageIfPredicate someRepoSelected app (str "repo")
                                         , str "/"
                                         , highlightMessageIfPredicate someRepoSelected app (str "issues")
                                         , str "/"
                                         , highlightMessageIfPredicate someRepoSelected app (str "pulls")
                                         , str "/"
                                         , highlightMessageIfPredicate someRepoSelected app (str "actions")
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

    otherActionsColumn = keybindingBox [-- keyIndicator' (showKey cycleVisibilityThresholdKey) (visibilityThresholdWidget app)
      hBox [str "["
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
           , withAttr hotkeyMessageAttr $ str " page"
           ]
      , hBox [str "["
             , highlightKeyIfPredicate isSearchable app (str $ showKey editSearchKey)
             , str "] "
             , highlightMessageIfPredicate isSearchable app (str "Search")
             ]
      , hBox [str "["
             , highlightKeyIfPredicate isSearchable app (str $ showKey zoomModalKey)
             , str "] "
             , highlightMessageIfPredicate isSearchable app (str "Zoom")
             ]
      , keyIndicator "q" "Exit"
      ]

hasNextPageKey = const True -- TODO
hasPrevPageKey = const True -- TODO
hasFirstPageKey = const True -- TODO
hasLastPageKey = const True -- TODO

isSearchable s = case snd <$> listSelectedElement (s ^. appMainList) of
  Nothing -> False
  Just x -> isSearchable' x

isSearchable' :: SomeNode f -> Bool
isSearchable' (SomeNode (PaginatedIssuesNode {})) = True
isSearchable' (SomeNode (PaginatedPullsNode {})) = True
isSearchable' (SomeNode (PaginatedWorkflowsNode {})) = False
isSearchable' (SomeNode (PaginatedReposNode {})) = True
isSearchable' (SomeNode (PaginatedBranchesNode {})) = True
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

keyIndicatorHasSelected app = keyIndicatorContextual app someRepoSelected

keyIndicatorHasSelectedOpen app = keyIndicatorContextual app selectedRepoToggled

keyIndicatorContextual app p key msg = case p app of
  True -> hBox [str "[", withAttr hotkeyAttr $ str key, str "] ", withAttr hotkeyMessageAttr $ str msg]
  False -> hBox [str "[", withAttr disabledHotkeyAttr $ str key, str "] ", withAttr disabledHotkeyMessageAttr $ str msg]


-- * Predicates

-- someRepoSelected s = runIdentity $ withNthChildAndRepoParent s $ \_ _ repo ->
--   pure $ isJust maybeRepo
someRepoSelected s = case listSelectedElement (s ^. appMainList) of
  Nothing -> False
  Just _ -> True

selectedRepoToggled = const False

-- selectedRepoRunning s = case L.listSelectedElement (s ^. appMainList) of
--   Nothing -> False
--   Just (_, MainListElem {..}) -> isRunning status
