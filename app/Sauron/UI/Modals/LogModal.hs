module Sauron.UI.Modals.LogModal (
  renderLogModal,
  renderLogPanel,
  filterLogsByLevel,
  autoScrollLogsToBottom
  ) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Control.Monad.Logger (LogLevel(..))
import qualified Data.Sequence as Seq
import Data.Time
import GHC.Stack (SrcLoc(..))
import qualified Graphics.Vty as V
import Lens.Micro
import Relude
import Sauron.Types
import Sauron.UI.AttrMap (errorLogAttr, warningLogAttr, infoLogAttr, debugLogAttr, normalAttr, hotkeyMessageAttr, boldText, hashNumberAttr)


renderLogModal :: AppState -> ModalState Fixed -> Widget ClickableName
renderLogModal appState (LogModalState _) =
  renderLogPanel appState LogModalContent
  & border
  & withAttr normalAttr
  & hLimitPercent 80
  & vLimitPercent 90
  & centerLayer
renderLogModal _ _ = str "Invalid modal state" -- This should never happen

-- | Render the log panel content, can be used in modal or split view
renderLogPanel :: AppState -> ClickableName -> Widget ClickableName
renderLogPanel appState viewportName = vBox [
    hCenter $ withAttr boldText $ str (
        "Application Logs - Filter: "
        <> filterText
        <> " (" <> show (Seq.length filteredLogs) <> " lines)"
        <> " - Colors: " <> showColorModeMaybe (appState ^. appCliColorMode) <> " (cfg) / " <> showColorMode (appState ^. appActualColorMode) <> " (actual)"
        )
    , hBorder
    -- Simple scrollable viewport with scrollbar - always works
    , padBottom Max $ withVScrollBars OnRight $ withVScrollBarHandles $ viewport viewportName Vertical $
      vBox (renderLogEntries (appState ^. appNow) (appState ^. appShowStackTraces) filteredLogs)
    , hBorder
    , hCenter $ withAttr hotkeyMessageAttr $ str "Press [Esc] or [Ctrl+Q] to close, [c] to clear logs, [d/i/w/e] to filter levels, [s] to toggle stack traces, [↑↓] to scroll"
  ]
  where
    currentFilter = appState ^. appLogLevelFilter
    filteredLogs = filterLogsByLevel currentFilter (appState ^. appLogs)
    filterText = case currentFilter of
      LevelDebug -> "All"
      LevelInfo -> "Info"
      LevelWarn -> "Warn"
      LevelError -> "Error"
      LevelOther t -> toString t

showColorModeMaybe :: Maybe V.ColorMode -> String
showColorModeMaybe Nothing = "Nothing"
showColorModeMaybe (Just x) = showColorMode x

showColorMode :: V.ColorMode -> String
showColorMode V.FullColor = "Full"
showColorMode (V.ColorMode240 _) = "240"
showColorMode V.ColorMode16 = "16"
showColorMode V.ColorMode8 = "8"
showColorMode V.NoColor = "None"

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
autoScrollLogsToBottom = do
  autoScrollViewportIfAtBottom LogModalContent
  autoScrollViewportIfAtBottom LogSplitContent

autoScrollViewportIfAtBottom :: ClickableName -> EventM ClickableName AppState ()
autoScrollViewportIfAtBottom viewportName = do
  lookupViewport viewportName >>= \case
    Just viewportInfo -> do
      let isAtBottom = _vpTop viewportInfo + _vpSize viewportInfo ^. _2 >= _vpContentSize viewportInfo ^. _2
      when isAtBottom $ vScrollToEnd (viewportScroll viewportName)
    Nothing -> return ()
