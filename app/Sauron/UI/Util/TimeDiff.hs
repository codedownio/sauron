
module Sauron.UI.Util.TimeDiff (
  timeDiff
  , timeFromNow
  ) where

import Data.Time
import Relude

timeFromNow :: NominalDiffTime -> String
timeFromNow diffTime
  -- Clock skew?
  | diffTime < 0 = "just now"
  | otherwise = timeDiff diffTime ++ " ago"

timeDiff :: NominalDiffTime -> String
timeDiff diffTime
  | minutes < 1 = show secs ++ "s"
  | hours < 1 = show mins ++ "m" ++ if remSecs > 0 then " " ++ show remSecs ++ "s" else ""
  | days < 1 = show hrs ++ "h" ++ if remMins > 0 then " " ++ show remMins ++ "m" else ""
  | otherwise = show ds ++ "d" ++ if remHrs > 0 then " " ++ show remHrs ++ "h" else ""
  where
    totalSeconds = round (realToFrac diffTime :: Double) :: Int
    secs = totalSeconds
    minutes = totalSeconds `div` 60
    hours = totalSeconds `div` 3600
    days = totalSeconds `div` 86400

    mins = minutes
    remSecs = totalSeconds `mod` 60

    hrs = hours
    remMins = (totalSeconds `mod` 3600) `div` 60

    ds = days
    remHrs = (totalSeconds `mod` 86400) `div` 3600
