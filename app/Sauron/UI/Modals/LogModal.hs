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
renderLogModal appState LogModalState =
  vBox [
    hCenter $ withAttr boldText $ str "Application Logs"
    , hBorder
    -- Scrollable content area with log entries
    , padBottom Max $ withVScrollBars OnRight $ withVScrollBarHandles $ viewport LogModalContent Vertical $
      vBox (renderLogEntries (appState ^. appNow) (appState ^. appLogs))
    , hBorder
    , hCenter $ withAttr hotkeyMessageAttr $ str "Press [Esc] or [Ctrl+Q] to close, [c] to clear logs"
  ]
  & border
  & withAttr normalAttr
  & hLimitPercent 80
  & vLimitPercent 90
  & centerLayer
renderLogModal _ _ = str "Invalid modal state" -- This should never happen

renderLogEntries :: UTCTime -> Seq LogEntry -> [Widget ClickableName]
renderLogEntries currentTime logs =
  if Seq.null logs
    then [hCenter $ withAttr hotkeyMessageAttr $ str "No log entries yet"]
    else toList $ fmap (renderLogEntry currentTime) logs

renderLogEntry :: UTCTime -> LogEntry -> Widget ClickableName
renderLogEntry _currentTime (LogEntry timestamp level message) =
  hBox [
    withAttr timeAttr $ str (formatLogTime timestamp)
    , str " "
    , withAttr levelAttr $ str (formatLevel level)
    , str " "
    , txtWrap message
  ]
  where
    levelAttr = case level of
      LevelError   -> errorLogAttr
      LevelWarn    -> warningLogAttr
      LevelInfo    -> infoLogAttr
      LevelDebug   -> debugLogAttr
      _            -> normalAttr

    timeAttr = debugLogAttr  -- Use a muted color for timestamps

formatLevel :: LogLevel -> String
formatLevel LevelError = "[ERROR]"
formatLevel LevelWarn = "[WARN]"
formatLevel LevelInfo = "[INFO]"
formatLevel LevelDebug = "[DEBUG]"
formatLevel (LevelOther t) = "[" <> toString t <> "]"

formatLogTime :: UTCTime -> String
formatLogTime = formatTime defaultTimeLocale "%H:%M:%S.%06q"
