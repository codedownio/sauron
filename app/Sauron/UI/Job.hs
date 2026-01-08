{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Sauron.UI.Job (
  jobLine
  , jobInner
  ) where

import Brick
import Control.Monad
import Data.Maybe
import Data.String.Interpolate
import qualified Data.Text as T
import Data.Time.Clock
import GitHub
import Relude
import Sauron.HealthCheck.Stop (healthCheckIndicatorWidget)
import Sauron.Types
import Sauron.UI.AnsiUtil
import Sauron.UI.AttrMap
import Sauron.UI.Statuses (statusToIconAnimated, chooseWorkflowStatus, fetchableQuarterCircleSpinner)
import Sauron.UI.Util
import Sauron.UI.Util.TimeDiff
import UnliftIO.Async (Async)


instance ListDrawable Fixed 'SingleJobT where
  drawLine appState (EntityData {..}) = case _state of
    Fetched job -> jobLine (_appAnimationCounter appState) _toggled job _state _healthCheckThread
    Fetching (Just job) -> jobLine (_appAnimationCounter appState) _toggled job _state _healthCheckThread
    Fetching Nothing -> str [i|Loading job...|]
    NotFetched -> str [i|Job not fetched|]
    Errored e -> str [i|Job fetch errored: #{e}|]

  drawInner appState (EntityData {_state, _ident, ..}) = do
    guard _toggled
    guardFetchedOrHasPrevious _state $ \job ->
      return $ jobInner (_appAnimationCounter appState) job Nothing

instance ListDrawable Fixed 'JobLogGroupT where
  drawLine appState (EntityData {_static=jobLogGroup, ..}) = 
    jobLogGroupLine (_appAnimationCounter appState) _toggled jobLogGroup

  drawInner _appState (EntityData {_static=jobLogGroup, ..}) = do
    guard _toggled
    case jobLogGroup of
      JobLogGroup _timestamp _title (Just _status) children' ->
        return $ jobLogGroupInner children'
      _ -> Nothing

jobLogGroupLine :: Int -> Bool -> JobLogGroup -> Widget n
jobLogGroupLine _animationCounter _toggled' (JobLogLines _timestamp contents) = vBox $ map (\content -> padRight Max $ hBox $
  str "  " : parseAnsiText content
  ) contents
jobLogGroupLine animationCounter toggled' (JobLogGroup _timestamp title status _children) = padRight Max $ hBox $ catMaybes [
  Just $ withAttr openMarkerAttr $ str (if toggled' then "[-] " else "[+] "),
  Just $ withAttr normalAttr $ str $ toString title,
  statusWidget
  ]
  where
    statusWidget = case status of
      Just s -> Just $ padLeft (Pad 1) $ statusToIconAnimated animationCounter $ chooseWorkflowStatus s
      Nothing -> Nothing

jobLogGroupInner :: [JobLogGroup] -> Widget n
jobLogGroupInner logGroups = vBox $ map renderLogGroup logGroups
  where
    renderLogGroup (JobLogLines _timestamp contents) = vBox $ map renderLogLine contents
    renderLogGroup (JobLogGroup _timestamp title _status children') = vBox [
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

jobInner :: Int -> Job -> Maybe [JobLogGroup] -> Widget n
jobInner _animationCounter (Job {}) _maybeJobLogs' = vBox []
