module Sauron.UI.Modals.LogModal (
  renderLogModal
  ) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Control.Monad.Logger (LogLevel(..))
import qualified Data.Sequence as Seq
import Data.Time
import Lens.Micro
import Relude
import Sauron.Types
import Sauron.UI.AttrMap (errorLogAttr, warningLogAttr, infoLogAttr, debugLogAttr, normalAttr, hotkeyMessageAttr, boldText)


renderLogModal :: AppState -> ModalState Fixed -> Widget ClickableName
renderLogModal appState LogModalState = vBox [
    hCenter $ withAttr boldText $ str ("Application Logs - Filter: " <> filterText)
    , hBorder
    -- Scrollable content area with log entries
    , padBottom Max $ withVScrollBars OnRight $ withVScrollBarHandles $ viewport LogModalContent Vertical $
      vBox (renderLogEntries (appState ^. appNow) filteredLogs)
    , hBorder
    , hCenter $ withAttr hotkeyMessageAttr $ str "Press [Esc] or [Ctrl+Q] to close, [c] to clear logs, [d/i/w/e] to filter levels"
  ]
  & border
  & withAttr normalAttr
  & hLimitPercent 80
  & vLimitPercent 90
  & centerLayer
  where
    currentFilter = appState ^. appLogLevelFilter
    filteredLogs = filterLogsByLevel currentFilter (appState ^. appLogs)
    filterText = case currentFilter of
      LevelDebug -> "All"
      LevelInfo -> "Info"
      LevelWarn -> "Warn"
      LevelError -> "Error"
      LevelOther t -> toString t
renderLogModal _ _ = str "Invalid modal state" -- This should never happen

renderLogEntries :: UTCTime -> Seq LogEntry -> [Widget ClickableName]
renderLogEntries currentTime logs =
  if Seq.null logs
    then [hCenter $ withAttr hotkeyMessageAttr $ str "No log entries yet"]
    else toList $ fmap (renderLogEntry currentTime) logs

renderLogEntry :: UTCTime -> LogEntry -> Widget ClickableName
renderLogEntry _currentTime (LogEntry timestamp level message maybeDuration) =
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
