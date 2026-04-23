{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sauron.UI.Job (
  jobLine
  , jobInner
  ) where

import Brick
import Control.Lens
import Control.Monad
import Data.Maybe
import Data.String.Interpolate
import qualified Data.Text as T
import Data.Time.Clock
import GitHub
import qualified Graphics.Vty as V
import Relude
import Sauron.Actions
import Sauron.Event.Helpers
import Sauron.HealthCheck.Stop (healthCheckIndicatorWidget)
import Sauron.Types
import Sauron.UI.AnsiUtil
import Sauron.UI.AttrMap
import Sauron.UI.Keys
import Sauron.UI.Statuses (statusToIconAnimated, chooseWorkflowStatus, fetchableQuarterCircleSpinner)
import Sauron.UI.Util
import Sauron.UI.Util.TimeDiff
import UnliftIO.Async (Async)


instance ListDrawable Fixed 'SingleJobT where
  drawLine appState (EntityData {..}) = case _state of
    JobNodeState {jnsJob = Fetched job, jnsLogs = logsFetchable, jnsMaxSiblingDuration = maxSib} ->
      jobLine (_appAnimationCounter appState) _toggled job logsFetchable maxSib _healthCheckThread
    JobNodeState {jnsJob = Fetching (Just job), jnsLogs = logsFetchable, jnsMaxSiblingDuration = maxSib} ->
      jobLine (_appAnimationCounter appState) _toggled job logsFetchable maxSib _healthCheckThread
    JobNodeState {jnsJob = Fetching Nothing} -> str [i|Loading job...|]
    JobNodeState {jnsJob = NotFetched} -> str [i|Job not fetched|]
    JobNodeState {jnsJob = Errored e} -> str [i|Job fetch errored: #{e}|]

  drawInner appState (EntityData {_state, _ident, ..}) = do
    guard _toggled
    let JobNodeState {jnsJob = jobFetchable, jnsLogs = logsFetchable} = _state
    let splitMethod = case logsFetchable of
          Fetched (_, method) -> method
          _ -> LogsNotSplit
    guardFetchedOrHasPrevious jobFetchable $ \job ->
      return $ jobInner (_appAnimationCounter appState) job splitMethod

  getExtraTopBoxWidgets _app (EntityData {}) =
    [hBox [str "["
          , withAttr hotkeyAttr $ str $ showKey zoomModalKey
          , str "] "
          , withAttr hotkeyMessageAttr $ str "Zoom"
          ]
    ]

  handleHotkey s key (EntityData {})
    | key == zoomModalKey = do
        withFixedElemAndParents s $ \(SomeNode _) (SomeNode variableEl) parents -> do
          refreshOnZoom (s ^. appBaseContext) variableEl parents
          liftIO $ atomically $ writeTVar (_appModalVariable s) (Just (ZoomModalState (SomeNode variableEl) (toList parents)))
        return True
  handleHotkey _ _ _ = return False

instance ListDrawable Fixed 'JobLogGroupT where
  drawLine appState (EntityData {_static=jobLogGroup, ..}) =
    jobLogGroupLine (_appAnimationCounter appState) _toggled jobLogGroup

  drawInner _appState (EntityData {_static=jobLogGroup, _state, ..}) = do
    guard _toggled
    case jobLogGroup of
      JobLogGroup {jobLogGroupStatus = Just _status, jobLogGroupChildren = children'} ->
        return $ jobLogGroupInner _state children'
      _ -> Nothing

  getExtraTopBoxWidgets _app (EntityData {}) =
    [hBox [str "["
          , withAttr hotkeyAttr $ str $ showKey zoomModalKey
          , str "] "
          , withAttr hotkeyMessageAttr $ str "Zoom"
          ]
    ]

  handleHotkey s key (EntityData {})
    | key == zoomModalKey = do
        withFixedElemAndParents s $ \(SomeNode _) (SomeNode variableEl) parents -> do
          refreshOnZoom (s ^. appBaseContext) variableEl parents
          liftIO $ atomically $ writeTVar (_appModalVariable s) (Just (ZoomModalState (SomeNode variableEl) (toList parents)))
        return True
  handleHotkey _ _ _ = return False

brightnessInterpolatedDuration :: Maybe NominalDiffTime -> Maybe NominalDiffTime -> Widget n
brightnessInterpolatedDuration (Just d) (Just maxD) | maxD > 0 =
  let ratio = realToFrac d / realToFrac maxD :: Double
      brightness = round (80 + 175 * min 1 ratio) :: Int
      b = fromIntegral (min 255 brightness) :: Word8
      attr = V.Attr V.Default (V.SetTo (V.RGBColor b b b)) V.Default V.Default
  in modifyDefAttr (const attr) $ str $ timeDiff d
brightnessInterpolatedDuration (Just d) _ = str $ timeDiff d
brightnessInterpolatedDuration Nothing _ = emptyWidget

jobLogGroupLine :: Int -> Bool -> JobLogGroup -> Widget n
jobLogGroupLine _animationCounter _toggled' (JobLogLines {jobLogLinesLines = contents}) = vBox [padRight Max $ hBox $ str "  " : parseAnsiText c | c <- contents]
jobLogGroupLine animationCounter toggled' (JobLogGroup {jobLogGroupTitle = title, jobLogGroupStatus = status, jobLogGroupDuration = duration, jobLogGroupMaxSiblingDuration = maxSibDuration}) = hBox $ catMaybes [
  Just $ padRight Max $ hBox $ catMaybes [
    Just $ withAttr openMarkerAttr $ str (if toggled' then "[-] " else "[+] "),
    Just $ withAttr normalAttr $ str $ toString title,
    statusWidget
    ],
  durationW
  ]
  where
    statusWidget = case status of
      Just s -> Just $ padLeft (Pad 1) $ statusToIconAnimated animationCounter $ chooseWorkflowStatus s
      Nothing -> Nothing
    durationW = case duration of
      Just _ -> Just $ padLeft (Pad 1) $ brightnessInterpolatedDuration duration maxSibDuration
      Nothing -> Nothing

jobLogGroupInner :: Maybe ScrollTarget -> [JobLogGroup] -> Widget n
jobLogGroupInner scrollTarget logGroups = vBox $ applyVisible $ concatMap renderLogGroup logGroups
  where
    applyVisible ws = case targetIndex of
      Just n -> case splitAt (clamp' 0 (length ws - 1) n) ws of
        (before, w:after) -> before ++ [visible w] ++ after
        _ -> ws
      Nothing -> ws
      where
        targetIndex = case scrollTarget of
          Just (ScrollToLine n) -> Just n
          Just ScrollToEnd      -> Just (length ws - 1)
          _                     -> Nothing
        clamp' lo hi = max lo . min hi

    renderLogGroup (JobLogLines {jobLogLinesLines}) = map renderLogLine jobLogLinesLines
    renderLogGroup (JobLogGroup {jobLogGroupTitle, jobLogGroupChildren}) =
      withAttr normalAttr (str (toString jobLogGroupTitle)) : concatMap renderLogGroup jobLogGroupChildren

    renderLogLine content
      | "[command]" `T.isPrefixOf` content = hBox $ renderCommandLine content
      | "##[error]" `T.isPrefixOf` content = hBox $ renderErrorLine content
      | otherwise = hBox $ parseAnsiText content

    renderCommandLine content = [str "▶ ", withAttr commandAttr $ txt $ T.drop 9 content]

    renderErrorLine content = [withAttr erroredAttr $ txt $ T.drop 9 content]

jobLine :: Int -> Bool -> Job -> Fetchable a -> Maybe NominalDiffTime -> Maybe (Async (), Int) -> Widget n
jobLine animationCounter toggled' (Job {..}) fetchableState maxSibDuration healthCheckThreadData = vBox [line1, line2]
  where
    line1 = hBox [
      withAttr openMarkerAttr $ str (if toggled' then "[-] " else "[+] ")
      , withAttr normalAttr $ str $ toString $ untagName jobName
      , padLeft (Pad 1) $ statusToIconAnimated animationCounter $ chooseWorkflowStatus $ fromMaybe jobStatus jobConclusion
      , fetchableQuarterCircleSpinner animationCounter fetchableState
      , healthCheckIndicatorWidget healthCheckThreadData
      , padLeft Max $ calculateDurationWidget jobStartedAt jobCompletedAt
      ]

    line2 = padRight Max $ padLeft (Pad 4) $ hBox $ catMaybes [
      do
        guard toggled'
        runnerNameWidget jobRunnerName
      ]

    calculateDurationWidget :: UTCTime -> Maybe UTCTime -> Widget n
    calculateDurationWidget started (Just completed) = brightnessInterpolatedDuration (Just (diffUTCTime completed started)) maxSibDuration
    calculateDurationWidget _ Nothing = str "running"

    runnerNameWidget :: Maybe Text -> Maybe (Widget n)
    runnerNameWidget (Just name) = Just $ hBox [
      str "Runner: "
      , withAttr normalAttr $ str (toString name)
      ]
    runnerNameWidget Nothing = Nothing

jobInner :: Int -> Job -> LogSplitMethod -> Widget n
jobInner _animationCounter (Job {}) splitMethod = vBox $
  case splitMethod of
    FlatLogTimestampSplit -> [withAttr erroredAttr $ str "⚠ Job step boundaries may be unreliable (flat log format). See https://github.com/codedownio/sauron/issues/25"]
    _ -> []
