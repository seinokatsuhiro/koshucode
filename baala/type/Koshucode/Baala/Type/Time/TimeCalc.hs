{-# OPTIONS_GHC -Wall #-}

-- | Calculation on time.

module Koshucode.Baala.Type.Time.TimeCalc
  ( -- * Arithmetic
    timeAddDay, timeAddWeek, timeAddMonth, timeAddYear,
    timeAddClock, timeDiff,

    -- * First day
    timeFloorMonth, timeFloorYear,
    timeCeilMonth, timeCeilYaer,

    -- * Range
    timeRangeDay, timeRangeMonth, timeRangeYear,
  ) where

import qualified Data.Time.Calendar                        as Tim
import qualified Koshucode.Baala.Overture                  as O
import qualified Koshucode.Baala.Base                      as B
import qualified Koshucode.Baala.Type.Time.Clock           as T
import qualified Koshucode.Baala.Type.Time.ClockCalc       as T
import qualified Koshucode.Baala.Type.Time.Date            as T
import qualified Koshucode.Baala.Type.Time.Parts           as T
import qualified Koshucode.Baala.Type.Time.Time            as T
import qualified Koshucode.Baala.Base.Message              as Msg


-- ----------------------  Add

-- | Add days to time.
timeAddDay :: T.Days -> O.Map T.Time
timeAddDay n = T.timeAltDays (+ n)

-- | Add weeks to time.
timeAddWeek :: Integer -> O.Map T.Time
timeAddWeek n = timeAddDay (7 * n)

-- | Add months to time.
timeAddMonth :: Integer -> O.Map T.Time
timeAddMonth n time = T.ymdTimeClip y' (fromInteger m') d where
    (y, m, d)  = T.mjdYmd time
    (yd, m')   = (toInteger m + n) `divMod` 12
    y'         = y + yd

-- | Add years to time.
timeAddYear :: T.Year -> O.Map T.Time
timeAddYear n time = T.ymdTimeClip (y + n) m d where
    (y, m, d) = T.mjdYmd time

-- | Add clock to time.
timeAddClock :: T.Clock -> B.AbMap T.Time
timeAddClock c1 (T.TimeYmdcz d2 c2 z2) = timeAddClockWith time c1 d2 c2 where
    time d c = T.TimeYmdcz d c z2
timeAddClock c1 (T.TimeYmdc d2 c2) = timeAddClockWith T.TimeYmdc c1 d2 c2
timeAddClock (T.ClockD d) t@(T.TimeYmd _) = Right $ timeAddDay d t
timeAddClock _ _ = Msg.adlib "add-clock"

timeAddClockWith :: (T.Date -> T.Clock -> T.Time) -> T.Clock -> T.Date -> T.Clock -> B.Ab T.Time
timeAddClockWith time c1 d2 c2 =
    do c3 <- T.clockAdd c1 c2
       let (d3, _) = T.clockDaysSec c3
           c4      = T.clockCutDay  c3
       Right $ timeAddDay d3 $ time d2 c4


-- ----------------------  Sub

-- | Calculate clock from time to time.
timeDiff :: T.Time -> T.Time -> B.Ab T.Clock
timeDiff (T.TimeYmdcz d2 c2 _) (T.TimeYmdcz d1 c1 _)  = timeDiffDc d2 c2 d1 c1
timeDiff (T.TimeYmdc d2 c2)    (T.TimeYmdc d1 c1)     = timeDiffDc d2 c2 d1 c1
timeDiff (T.TimeYmd d2) (T.TimeYmd d1)  = Right $ T.ClockD $ timeDiffDate d2 d1
timeDiff (T.TimeYm  d2) (T.TimeYm  d1)  = Right $ T.ClockD $ timeDiffDay  d2 d1
timeDiff _ _ = Msg.adlib "time-diff"

timeDiffDc :: T.Date -> T.Clock -> T.Date -> T.Clock -> B.Ab T.Clock
timeDiffDc d2 c2 d1 c1 =
    do let d3 = timeDiffDate d2 d1
       c3 <- T.clockSub c2 c1
       Right $ T.clockAddDay d3 c3

timeDiffDate :: T.Date -> T.Date -> Integer
timeDiffDate d2 d1 = T.toMjd d2 `timeDiffDay` T.toMjd d1

timeDiffDay :: T.Mjd -> T.Mjd -> Integer
timeDiffDay d2 d1 = Tim.toModifiedJulianDay d2 - Tim.toModifiedJulianDay d1


-- ----------------------  First day

-- | Convert to the first day of month.
--
--   >>> timeFloorMonth $ ymdTimeClip 2014 11 3
--   2014-11-01
--
timeFloorMonth :: O.Map T.Time
timeFloorMonth time =
    case T.mjdYmd time of
      (y, m, _) -> T.ymdTimeClip y m 1

-- | Convert to the first day of year.
--
--   >>> timeFloorYear $ ymdTimeClip 2014 11 3
--   2014-01-01
--
timeFloorYear :: O.Map T.Time
timeFloorYear time =
    case T.mjdYmd time of
      (y, _, _) -> T.ymdTimeClip y 1 1

-- | Convert to the first day of next month.
--
--   >>> timeCeilMonth $ ymdTimeClip 2014 11 3
--   2014-12-01
--
--   >>> timeCeilMonth $ ymdTimeClip 2014 12 25
--   2015-01-01
--
timeCeilMonth :: O.Map T.Time
timeCeilMonth time =
    case T.mjdYmd time of
      (y, m, _) -> T.mjdTimeClip $ monthUp (y, m, 1)

-- | Convert to the first day of next year.
--
--    >>> timeCeilYaer $ ymdTimeClip 2014 11 3
--    2015-01-01
--
timeCeilYaer :: O.Map T.Time
timeCeilYaer time =
    case T.mjdYmd time of
      (y, _, _) -> T.mjdTimeClip $ yearUp (y, 1, 1)


-- ----------------------  Range

-- | Create range of time.
--
--   >>> timeRangeDay (T.ymdTimeClip 2014 11 3) (T.ymdTimeClip 2014 11 5)
--   [2014-11-03, 2014-11-04, 2014-11-05]
--
timeRangeDay :: B.RangeBy T.Time
timeRangeDay from to = map T.mjdTime [T.toMjd from .. T.toMjd to]

-- | Create range of time.
--
--   >>> timeRangeMonth (ymdTimeClip 2014 12 31) (ymdTimeClip 2015 03 5)
--   [2014-12-31, 2015-01-31, 2015-02-28]
--
timeRangeMonth :: B.RangeBy T.Time
timeRangeMonth = timeRangeBy monthUp

-- | Create range of time.
timeRangeYear :: B.RangeBy T.Time
timeRangeYear = timeRangeBy yearUp

timeRangeBy :: O.Map T.Ymd -> B.RangeBy T.Time
timeRangeBy step from to = times where
    dayFrom =  T.mjdYmd from
    dayTo   =  T.mjdYmd to
    times   =  map T.mjdTimeClip $ B.rangeBy step dayFrom dayTo

-- | Increment month.
monthUp :: O.Map T.Ymd
monthUp (y, m, d) | m < 12    = (y, m + 1, d)
                  | otherwise = (y + 1, 1, d)

-- | Increment year.
yearUp :: O.Map T.Ymd
yearUp (y, m, d)  | y == (-1) = (1, m, d)
                  | otherwise = (y + 1, m, d)
