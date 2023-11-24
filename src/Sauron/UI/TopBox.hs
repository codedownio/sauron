{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Sauron.UI.TopBox (
  topBox
  ) where

import Brick
import qualified Data.List as L
import Relude
import Sauron.UI.AttrMap
import Sauron.UI.Keys

-- import qualified Brick.Widgets.List as L
-- import Data.Either
-- import Data.Maybe
-- import Lens.Micro


topBox app = hBox [columnPadding settingsColumn
                  , columnPadding actionsColumn
                  , columnPadding otherActionsColumn]
  where
    settingsColumn = keybindingBox [keyIndicator (L.intersperse '/' [unKChar nextKey, unKChar previousKey, '↑', '↓']) "Navigate"
                                   , keyIndicatorHasSelected app (showKeys toggleKeys) "Open/close node"
                                   , keyIndicatorHasSelectedOpen app "Control-v/Meta-v" "Scroll node"
                                   , keyIndicatorHasSelected app (unKChar closeNodeKey : '/' : [unKChar openNodeKey]) "Fold/unfold node"
                                   , keyIndicator "Meta + [0-9]" "Unfold top # nodes"
                                   , keyIndicator (unKChar nextFailureKey : '/' : [unKChar previousFailureKey]) "Next/previous failure"
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
                                         , highlightMessageIfPredicate someRepoSelected app (str "home")
                                         , str "/"
                                         , highlightMessageIfPredicate someRepoSelected app (str "issues")
                                         , str "/"
                                         , highlightMessageIfPredicate someRepoSelected app (str "pulls")
                                         , str "/"
                                         , highlightMessageIfPredicate someRepoSelected app (str "actions")
                                         ]
                                  ]

    otherActionsColumn = keybindingBox [-- keyIndicator' (showKey cycleVisibilityThresholdKey) (visibilityThresholdWidget app)
                                       -- , hBox [str "["
                                       --        , str $ showKey toggleShowRunTimesKey
                                       --        , str "/"
                                       --        , str $ showKey toggleFileLocationsKey
                                       --        , str "/"
                                       --        , str $ showKey toggleVisibilityThresholdsKey
                                       --        , str "] "
                                       --        , highlightMessageIfPredicate (^. appShowRunTimes) app (str "Times")
                                       --        , str "/"
                                       --        , highlightMessageIfPredicate (^. appShowFileLocations) app (str "locations")
                                       --        , str "/"
                                       --        , highlightMessageIfPredicate (^. appShowVisibilityThresholds) app (str "thresholds")
                                       --   ]
                                       -- , hBox [str "["
                                       --        , highlightIfLogLevel app LevelDebug [unKChar debugKey]
                                       --        , str "/"
                                       --        , highlightIfLogLevel app LevelInfo [unKChar infoKey]
                                       --        , str "/"
                                       --        , highlightIfLogLevel app LevelWarn [unKChar warnKey]
                                       --        , str "/"
                                       --        , highlightIfLogLevel app LevelError [unKChar errorKey]
                                       --        , str "] "
                                       --        , str "Log level"]

                                       keyIndicator "q" "Exit"]

-- visibilityThresholdWidget app = hBox $
--   [withAttr hotkeyMessageAttr $ str "Visibility threshold ("]
--   <> L.intersperse (str ", ") [withAttr (if x == app ^. appVisibilityThreshold then visibilityThresholdSelectedAttr else visibilityThresholdNotSelectedAttr) $ str $ show x | x <- (app ^. appVisibilityThresholdSteps)]
--   <> [(str ")")]

columnPadding = padLeft (Pad 1) . padRight (Pad 3) -- . padTop (Pad 1)

keybindingBox = vBox

-- highlightIfLogLevel app desiredLevel thing =
--   if | app ^. appLogLevel == Just desiredLevel -> withAttr visibilityThresholdSelectedAttr $ str thing
--      | otherwise -> withAttr hotkeyAttr $ str thing

highlightKeyIfPredicate p app x = case p app of
  True -> withAttr hotkeyAttr x
  False -> withAttr disabledHotkeyAttr x

highlightMessageIfPredicate p app x = case p app of
  True -> withAttr hotkeyMessageAttr x
  False -> withAttr disabledHotkeyMessageAttr x

keyIndicator key msg = keyIndicator' key (withAttr hotkeyMessageAttr $ str msg)

keyIndicator' key label = hBox [str "[", withAttr hotkeyAttr $ str key, str "] ", label]

keyIndicatorHasSelected app = keyIndicatorContextual app someRepoSelected

keyIndicatorHasSelectedOpen app = keyIndicatorContextual app selectedRepoToggled

keyIndicatorContextual app p key msg = case p app of
  True -> hBox [str "[", withAttr hotkeyAttr $ str key, str "] ", withAttr hotkeyMessageAttr $ str msg]
  False -> hBox [str "[", withAttr disabledHotkeyAttr $ str key, str "] ", withAttr disabledHotkeyMessageAttr $ str msg]


-- * Predicates

someRepoSelected = const False

selectedRepoToggled = const False

-- selectedRepoRunning s = case L.listSelectedElement (s ^. appMainList) of
--   Nothing -> False
--   Just (_, MainListElem {..}) -> isRunning status
