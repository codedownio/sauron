{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Sauron.UI.TopBox (
  topBox
  ) where

import Brick
import qualified Data.List as L
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

    actionsColumn = keybindingBox []

    otherActionsColumn = keybindingBox []

--     actionsColumn = keybindingBox [hBox [str "["
--                                          , highlightKeyIfPredicate selectedRepoRunning app (str $ showKey cancelSelectedKey)
--                                          , str "/"
--                                          , highlightKeyIfPredicate someTestRunning app (str $ showKey cancelAllKey)
--                                          , str "] "
--                                          , withAttr hotkeyMessageAttr $ str "Cancel "
--                                          , highlightMessageIfPredicate selectedRepoRunning app (str "selected")
--                                          , str "/"
--                                          , highlightMessageIfPredicate someTestRunning app (str "all")
--                                          ]
--                                   , hBox [str "["
--                                          , highlightKeyIfPredicate selectedRepoDone app (str $ showKey runSelectedKey)
--                                          , str "/"
--                                          , highlightKeyIfPredicate noTestsRunning app (str $ showKey runAllKey)
--                                          , str "] "
--                                          , withAttr hotkeyMessageAttr $ str "Run "
--                                          , highlightMessageIfPredicate selectedRepoDone app (str "selected")
--                                          , str "/"
--                                          , highlightMessageIfPredicate noTestsRunning app (str "all")
--                                          ]
--                                   , hBox [str "["
--                                          , highlightKeyIfPredicate selectedRepoDone app (str $ showKey clearSelectedKey)
--                                          , str "/"
--                                          , highlightKeyIfPredicate allTestsDone app (str $ showKey clearAllKey)
--                                          , str "] "
--                                          , withAttr hotkeyMessageAttr $ str "Clear "
--                                          , highlightMessageIfPredicate selectedRepoDone app (str "selected")
--                                          , str "/"
--                                          , highlightMessageIfPredicate allTestsDone app (str "all")
--                                          ]
--                                   , hBox [str "["
--                                          , highlightKeyIfPredicate someRepoSelected app (str $ showKey openSelectedFolderInFileExplorer)
--                                          , str "/"
--                                          , highlightKeyIfPredicate (const True) app (str $ showKey openTestRootKey)
--                                          , str "] "
--                                          , withAttr hotkeyMessageAttr $ str "Open "
--                                          , highlightMessageIfPredicate someRepoSelected app (str "selected")
--                                          , str "/"
--                                          , highlightMessageIfPredicate (const True) app (str "root")
--                                          , withAttr hotkeyMessageAttr $ str " folder"
--                                          ]
--                                   , hBox [str "["
--                                          , highlightKeyIfPredicate someRepoSelected app (str $ showKey openTestInEditorKey)
--                                          , str "/"
--                                          , highlightKeyIfPredicate someRepoSelected app (str $ showKey openLogsInEditorKey)
--                                          , str "/"
--                                          , highlightKeyIfPredicate someRepoSelected app (str $ showKey openFailureInEditorKey)
--                                          , str "] "
--                                          , withAttr hotkeyMessageAttr $ str "Edit "
--                                          , highlightMessageIfPredicate someRepoSelected app (str "test")
--                                          , str "/"
--                                          , highlightMessageIfPredicate someRepoSelected app (str "logs")
--                                          , str "/"
--                                          , highlightMessageIfPredicate selectedRepoHasCallStack app (str "failure")
--                                          ]
--                                   ]

--     otherActionsColumn = keybindingBox [keyIndicator' (showKey cycleVisibilityThresholdKey) (visibilityThresholdWidget app)
--                                        , hBox [str "["
--                                               , str $ showKey toggleShowRunTimesKey
--                                               , str "/"
--                                               , str $ showKey toggleFileLocationsKey
--                                               , str "/"
--                                               , str $ showKey toggleVisibilityThresholdsKey
--                                               , str "] "
--                                               , highlightMessageIfPredicate (^. appShowRunTimes) app (str "Times")
--                                               , str "/"
--                                               , highlightMessageIfPredicate (^. appShowFileLocations) app (str "locations")
--                                               , str "/"
--                                               , highlightMessageIfPredicate (^. appShowVisibilityThresholds) app (str "thresholds")
--                                          ]
--                                        , hBox [str "["
--                                               , highlightIfLogLevel app LevelDebug [unKChar debugKey]
--                                               , str "/"
--                                               , highlightIfLogLevel app LevelInfo [unKChar infoKey]
--                                               , str "/"
--                                               , highlightIfLogLevel app LevelWarn [unKChar warnKey]
--                                               , str "/"
--                                               , highlightIfLogLevel app LevelError [unKChar errorKey]
--                                               , str "] "
--                                               , str "Log level"]

--                                        , keyIndicator "q" "Exit"]

-- visibilityThresholdWidget app = hBox $
--   [withAttr hotkeyMessageAttr $ str "Visibility threshold ("]
--   <> L.intersperse (str ", ") [withAttr (if x == app ^. appVisibilityThreshold then visibilityThresholdSelectedAttr else visibilityThresholdNotSelectedAttr) $ str $ show x | x <- (app ^. appVisibilityThresholdSteps)]
--   <> [(str ")")]

columnPadding = padLeft (Pad 1) . padRight (Pad 3) -- . padTop (Pad 1)

keybindingBox = vBox

-- highlightIfLogLevel app desiredLevel thing =
--   if | app ^. appLogLevel == Just desiredLevel -> withAttr visibilityThresholdSelectedAttr $ str thing
--      | otherwise -> withAttr hotkeyAttr $ str thing

-- highlightKeyIfPredicate p app x = case p app of
--   True -> withAttr hotkeyAttr x
--   False -> withAttr disabledHotkeyAttr x

-- highlightMessageIfPredicate p app x = case p app of
--   True -> withAttr hotkeyMessageAttr x
--   False -> withAttr disabledHotkeyMessageAttr x

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
