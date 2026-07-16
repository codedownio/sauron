{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Sauron.UI.LogPane (
  renderLogPane,
  filterLogsByLevel,
  autoScrollLogsToBottom
  ) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center (hCenter)
import Control.Monad.Logger (LogLevel(..))
import Data.String.Interpolate
import qualified Data.Sequence as Seq
import Data.Time
import GHC.Stack (SrcLoc(..))
import Lens.Micro
import Relude
import Sauron.Types
import Sauron.UI.AttrMap (errorLogAttr, warningLogAttr, infoLogAttr, debugLogAttr, normalAttr, hotkeyMessageAttr, disabledHotkeyMessageAttr, hotkeyAttr, disabledHotkeyAttr, boldText, hashNumberAttr)


-- | Render the split-view log pane: a hotkey top box (like the main app's TopBox),
-- an info border, and the scrollable log viewport.
renderLogPane :: AppState -> Widget ClickableName
renderLogPane appState = vBox [
    logTopBox appState
    , logInfoBorder filteredLogs
    , padBottom Max $ withVScrollBars OnRight $ withVScrollBarHandles $ viewport LogSplitContent Vertical $
        vBox (renderLogEntries (appState ^. appNow) (appState ^. appShowStackTraces) filteredLogs)
    ]
  where
    filteredLogs = filterLogsByLevel (appState ^. appLogLevelFilter) (appState ^. appLogs)

-- | Hotkey box for the log pane, modeled on the main app's TopBox. Filter levels and
-- the stack-trace toggle are highlighted to show which one is currently active.
logTopBox :: AppState -> Widget ClickableName
logTopBox appState = hBox [columnPadding column1, columnPadding column2]
  where
    currentFilter = appState ^. appLogLevelFilter
    showStackTraces = appState ^. appShowStackTraces

    column1 = vBox [keyIndicator "Meta+↑/↓" "Scroll line"
                   , keyIndicator "Meta+PgUp/PgDn" "Scroll page"
                   , keyIndicator "Ctrl+C" "Clear logs"
                   , keyIndicator "Ctrl+L" "Close logs"
                   ]

    column2 = vBox [hBox [str "["
                         , highlightKey (isFilter LevelDebug) "d"
                         , str "/"
                         , highlightKey (isFilter LevelInfo) "i"
                         , str "/"
                         , highlightKey (isFilter LevelWarn) "w"
                         , str "/"
                         , highlightKey (isFilter LevelError) "e"
                         , str "] "
                         , withAttr hotkeyMessageAttr $ str "Filter: "
                         , withAttr boldText $ str (filterLabel currentFilter)
                         ]
                   , hBox [str "["
                          , highlightKey showStackTraces "s"
                          , str "] "
                          , highlightMessage showStackTraces "Stack traces "
                          , withAttr boldText $ str (if showStackTraces then "(on)" else "(off)")
                          ]
                   ]

    isFilter lvl = currentFilter == lvl

columnPadding = padLeft (Pad 1) . padRight (Pad 3)

keyIndicator key msg = hBox [str "[", withAttr hotkeyAttr $ str key, str "] ", withAttr hotkeyMessageAttr $ str msg]

highlightKey :: Bool -> String -> Widget ClickableName
highlightKey True x = withAttr hotkeyAttr $ str x
highlightKey False x = withAttr disabledHotkeyAttr $ str x

highlightMessage :: Bool -> String -> Widget ClickableName
highlightMessage True x = withAttr hotkeyMessageAttr $ str x
highlightMessage False x = withAttr disabledHotkeyMessageAttr $ str x

filterLabel :: LogLevel -> String
filterLabel LevelDebug = "All"
filterLabel LevelInfo = "Info"
filterLabel LevelWarn = "Warn"
filterLabel LevelError = "Error"
filterLabel (LevelOther t) = toString t

logInfoBorder :: Seq LogEntry -> Widget ClickableName
logInfoBorder logs = hBorderWithLabel $ padLeftRight 1 $ hBox [
    withAttr boldText $ str "Application Logs"
    , str [i| (#{Seq.length logs} lines)|]
    ]

renderLogEntries :: UTCTime -> Bool -> Seq LogEntry -> [Widget ClickableName]
renderLogEntries currentTime showStackTraces logs =
  if Seq.null logs
    then [hCenter $ withAttr hotkeyMessageAttr $ str "No log entries yet"]
    else toList $ fmap (renderLogEntry currentTime showStackTraces) logs

renderLogEntry :: UTCTime -> Bool -> LogEntry -> Widget ClickableName
renderLogEntry _currentTime showStackTraces (LogEntry timestamp level message maybeDuration maybeStackTrace) =
  vBox [
    hBox [
      withAttr timeAttr $ str (formatLogTime timestamp)
      , str " "
      , withAttr levelAttr $ str (formatLevel level)
      , str " "
      , txtWrap message
      , case maybeDuration of
          Nothing -> emptyWidget
          Just duration -> hBox [str " ", formatDuration duration]
    ]
    , if showStackTraces
        then case maybeStackTrace of
          Nothing -> emptyWidget
          Just callStack' -> renderStackTrace callStack'
        else emptyWidget
  ]
  where
    levelAttr = case level of
      LevelError   -> errorLogAttr
      LevelWarn    -> warningLogAttr
      LevelInfo    -> infoLogAttr
      LevelDebug   -> debugLogAttr
      _            -> normalAttr

    timeAttr = debugLogAttr  -- Use a muted color for timestamps

    formatDuration :: NominalDiffTime -> Widget ClickableName
    formatDuration duration =
      let durationMs = round (duration * 1000) :: Integer
          durationAttr
            | durationMs < 100   = debugLogAttr      -- Fast (< 100ms) - gray
            | durationMs < 1000  = infoLogAttr       -- Normal (100-1000ms) - white
            | durationMs < 5000  = warningLogAttr    -- Slow (1-5s) - yellow
            | otherwise          = errorLogAttr      -- Very slow (>5s) - red
          durationText = show durationMs <> "ms"
      in withAttr durationAttr $ str durationText

renderStackTrace :: CallStack -> Widget ClickableName
renderStackTrace callStack' =
  vBox [
    vBox $ map renderStackFrame (getCallStack callStack')
  ]
  where
    renderStackFrame :: (String, SrcLoc) -> Widget ClickableName
    renderStackFrame (funcName, srcLoc) =
      hBox [
        str "    "
        , withAttr hotkeyMessageAttr $ str funcName
        , str " @ "
        , withAttr debugLogAttr $ str (srcLocFile srcLoc)
        , str ":"
        , withAttr hashNumberAttr $ str (show (srcLocStartLine srcLoc))
        , str ":"
        , withAttr hashNumberAttr $ str (show (srcLocStartCol srcLoc))
      ]

formatLevel :: LogLevel -> String
formatLevel LevelError = "[ERROR]"
formatLevel LevelWarn = "[WARN]"
formatLevel LevelInfo = "[INFO]"
formatLevel LevelDebug = "[DEBUG]"
formatLevel (LevelOther t) = "[" <> toString t <> "]"

formatLogTime :: UTCTime -> String
formatLogTime = formatTime defaultTimeLocale "%H:%M:%S.%06q"

filterLogsByLevel :: LogLevel -> Seq LogEntry -> Seq LogEntry
filterLogsByLevel filterLevel = Seq.filter (\logEntry -> logLevelPriority (_logEntryLevel logEntry) >= logLevelPriority filterLevel)
  where
    logLevelPriority :: LogLevel -> Int
    logLevelPriority LevelError = 4
    logLevelPriority LevelWarn = 3
    logLevelPriority LevelInfo = 2
    logLevelPriority LevelDebug = 1
    logLevelPriority (LevelOther _) = 0

autoScrollLogsToBottom :: EventM ClickableName AppState ()
autoScrollLogsToBottom = autoScrollViewportIfAtBottom LogSplitContent

autoScrollViewportIfAtBottom :: ClickableName -> EventM ClickableName AppState ()
autoScrollViewportIfAtBottom viewportName = do
  lookupViewport viewportName >>= \case
    Just viewportInfo -> do
      let isAtBottom = _vpTop viewportInfo + _vpSize viewportInfo ^. _2 >= _vpContentSize viewportInfo ^. _2
      when isAtBottom $ vScrollToEnd (viewportScroll viewportName)
    Nothing -> return ()
