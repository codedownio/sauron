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
    (Fetched job, logsFetchable) -> jobLine (_appAnimationCounter appState) _toggled job logsFetchable _healthCheckThread
    (Fetching (Just job), logsFetchable) -> jobLine (_appAnimationCounter appState) _toggled job logsFetchable _healthCheckThread
    (Fetching Nothing, _) -> str [i|Loading job...|]
    (NotFetched, _) -> str [i|Job not fetched|]
    (Errored e, _) -> str [i|Job fetch errored: #{e}|]

  drawInner appState (EntityData {_state, _ident, ..}) = do
    guard _toggled
    let (jobFetchable, logsFetchable) = _state
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

  drawInner _appState (EntityData {_static=jobLogGroup, ..}) = do
    guard _toggled
    case jobLogGroup of
      JobLogGroup _timestamp _title (Just _status) _duration children' ->
        return $ jobLogGroupInner children'
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

jobLogGroupLine :: Int -> Bool -> JobLogGroup -> Widget n
jobLogGroupLine _animationCounter _toggled' (JobLogLines _timestamp contents) = vBox $ map (\content -> padRight Max $ hBox $
  str "  " : parseAnsiText content
  ) contents
jobLogGroupLine animationCounter toggled' (JobLogGroup _timestamp title status duration _children) = hBox $ catMaybes [
  Just $ padRight Max $ hBox $ catMaybes [
    Just $ withAttr openMarkerAttr $ str (if toggled' then "[-] " else "[+] "),
    Just $ withAttr normalAttr $ str $ toString title,
    statusWidget
    ],
  durationWidget
  ]
  where
    statusWidget = case status of
      Just s -> Just $ padLeft (Pad 1) $ statusToIconAnimated animationCounter $ chooseWorkflowStatus s
      Nothing -> Nothing
    durationWidget = case duration of
      Just d -> Just $ padLeft (Pad 1) $ str $ timeDiff d
      Nothing -> Nothing

jobLogGroupInner :: [JobLogGroup] -> Widget n
jobLogGroupInner logGroups = vBox $ map renderLogGroup logGroups
  where
    renderLogGroup (JobLogLines _timestamp contents) = vBox $ map renderLogLine contents
    renderLogGroup (JobLogGroup _timestamp title _status _duration children') = vBox [
      withAttr normalAttr $ str $ toString title,
      vBox $ map renderLogGroup children'
      ]

    renderLogLine content
      | "[command]" `T.isPrefixOf` content = hBox $ renderCommandLine content
      | "##[error]" `T.isPrefixOf` content = hBox $ renderErrorLine content
      | otherwise = hBox $ parseAnsiText content

    renderCommandLine content =
      let commandText = T.drop 9 content  -- Remove "[command]"
      in [ str "▶ "
         , withAttr commandAttr $ str $ toString commandText
         ]

    renderErrorLine content =
      let text = T.drop 9 content  -- Remove "##[error]"
      in [ withAttr erroredAttr $ str $ toString text ]

jobLine :: Int -> Bool -> Job -> Fetchable a -> Maybe (Async (), Int) -> Widget n
jobLine animationCounter toggled' (Job {..}) fetchableState healthCheckThreadData = vBox [line1, line2]
  where
    line1 = hBox [
      withAttr openMarkerAttr $ str (if toggled' then "[-] " else "[+] ")
      , withAttr normalAttr $ str $ toString $ untagName jobName
      , padLeft (Pad 1) $ statusToIconAnimated animationCounter $ chooseWorkflowStatus $ fromMaybe jobStatus jobConclusion
      , fetchableQuarterCircleSpinner animationCounter fetchableState
      , healthCheckIndicatorWidget healthCheckThreadData
      , padLeft Max $ str $ calculateDuration jobStartedAt jobCompletedAt
      ]

    line2 = padRight Max $ padLeft (Pad 4) $ hBox $ catMaybes [
      do
        guard toggled'
        runnerNameWidget jobRunnerName
      ]

    calculateDuration :: UTCTime -> Maybe UTCTime -> String
    calculateDuration started (Just completed) = timeDiff $ diffUTCTime completed started
    calculateDuration _ Nothing = "running"

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
