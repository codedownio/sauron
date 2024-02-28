{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Sauron.UI.Util.TimeFromNow (
  timeFromNow
  ) where

import Data.Time
import Relude

timeFromNow :: NominalDiffTime -> String
timeFromNow diffTime =
  if | secondsAgo < 60 -> formatResult "second" secondsAgo
     | minutesAgo < 60 -> formatResult "minute" minutesAgo
     | hoursAgo < 24 -> formatResult "hour" hoursAgo
     | daysAgo < 7 -> formatResult "day" daysAgo
     | weeksAgo < 4 -> formatResult "week" weeksAgo
     | monthsAgo < 12 -> formatResult "month" monthsAgo
     | otherwise -> formatResult "year" yearsAgo

  where
    secondsAgo = truncate (abs diffTime)
    minutesAgo = secondsAgo `div` 60
    hoursAgo = minutesAgo `div` 60
    daysAgo = hoursAgo `div` 24
    weeksAgo = daysAgo `div` 7
    monthsAgo = truncate $ fromIntegral daysAgo / (30.44 :: Double)
    yearsAgo = truncate $ fromIntegral daysAgo / (365.25 :: Double)

    formatResult unit value = show value ++ " " ++ unit ++ (if value == 1 then " ago" else "s ago")
