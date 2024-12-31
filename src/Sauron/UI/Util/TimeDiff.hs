
module Sauron.UI.Util.TimeDiff (
  timeDiff
  , timeFromNow
  ) where

import Data.Time
import Relude

timeFromNow :: NominalDiffTime -> String
timeFromNow diffTime = show (round value :: Integer) ++ " " ++ unit ++ suffix
  where
    (value, unit) = timeDiffNumberAndString diffTime
    suffix = if value == 1 then " ago" else "s ago"

timeDiff :: NominalDiffTime -> String
timeDiff diffTime = show (round value :: Integer) ++ " " ++ unit ++ suffix
  where
    (value, unit) = timeDiffNumberAndString diffTime
    suffix = if value == 1 then "" else "s"

timeDiffNumberAndString :: NominalDiffTime -> (Double, String)
timeDiffNumberAndString diffTime =
  if | minutesAgo < 1 -> (secondsAgo, "second")
     | hoursAgo < 1 -> (minutesAgo, "minute")
     | daysAgo < 1 -> (hoursAgo, "hour")
     | weeksAgo < 1 -> (daysAgo, "day")
     | monthsAgo < 1 -> (weeksAgo, "week")
     | yearsAgo < 1 -> (monthsAgo, "month")
     | otherwise -> (yearsAgo, "year")

  where
    secondsAgo :: Double = realToFrac diffTime
    minutesAgo :: Double = secondsAgo / 60
    hoursAgo :: Double = minutesAgo / 60
    daysAgo :: Double = hoursAgo / 24
    weeksAgo :: Double = daysAgo / 7
    monthsAgo :: Double = daysAgo / (30.44 :: Double)
    yearsAgo :: Double = daysAgo / (365.25 :: Double)
