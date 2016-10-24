{-# OPTIONS_GHC -Wall #-}

-- | Calculation on time.

module Koshucode.Baala.Data.Type.Time.TimeCalc
  ( -- * Arithmetic
    timeAddDay, timeAddWeek, timeAddMonth, timeAddYear,
    timeAddClock, timeDiff,

    -- * First day
    timeFloorMonth, timeFloorYear,
    timeCeilMonth, timeCeilYaer,

    -- * Range
    timeRangeDay, timeRangeMonth, timeRangeYear,
  ) where

import qualified Data.Time.Calendar                         as Tim
import qualified Koshucode.Baala.Overture                   as O
import qualified Koshucode.Baala.Base                       as B
import qualified Koshucode.Baala.Data.Type.Time.Clock       as D
import qualified Koshucode.Baala.Data.Type.Time.ClockCalc   as D
import qualified Koshucode.Baala.Data.Type.Time.Date        as D
import qualified Koshucode.Baala.Data.Type.Time.Time        as D
import qualified Koshucode.Baala.Base.Message               as Msg


-- ----------------------  Add

-- | Add days to time.
timeAddDay :: D.Days -> O.Map D.Time
timeAddDay n = D.timeMapMjd (+ n)

-- | Add weeks to time.
timeAddWeek :: Integer -> O.Map D.Time
timeAddWeek n = timeAddDay (7 * n)

-- | Add months to time.
timeAddMonth :: Integer -> O.Map D.Time
timeAddMonth n time = D.timeFromYmd y' (fromInteger m') d where
    (y, m, d)  = D.timeYmdTuple time
    (yd, m')   = (toInteger m + n) `divMod` 12
    y'         = y + yd

-- | Add years to time.
timeAddYear :: D.Year -> O.Map D.Time
timeAddYear n time = D.timeFromYmd (y + n) m d where
    (y, m, d) = D.timeYmdTuple time

-- | Add clock to time.
timeAddClock :: D.Clock -> B.AbMap D.Time
timeAddClock c1 (D.TimeYmdcz d2 c2 z2) = timeAddClockWith time c1 d2 c2 where
    time d c = D.TimeYmdcz d c z2
timeAddClock c1 (D.TimeYmdc d2 c2) = timeAddClockWith D.TimeYmdc c1 d2 c2
timeAddClock (D.ClockD d) t@(D.TimeYmd _) = Right $ timeAddDay d t
timeAddClock _ _ = Msg.adlib "add-clock"

timeAddClockWith :: (D.Date -> D.Clock -> D.Time) -> D.Clock -> D.Date -> D.Clock -> B.Ab D.Time
timeAddClockWith time c1 d2 c2 =
    do c3 <- D.clockAdd c1 c2
       let (d3, _) = D.clockDaysSec c3
           c4      = D.clockCutDay  c3
       Right $ timeAddDay d3 $ time d2 c4


-- ----------------------  Sub

-- | Calculate clock from time to time.
timeDiff :: D.Time -> D.Time -> B.Ab D.Clock
timeDiff (D.TimeYmdcz d2 c2 _) (D.TimeYmdcz d1 c1 _)  = timeDiffDc d2 c2 d1 c1
timeDiff (D.TimeYmdc d2 c2)    (D.TimeYmdc d1 c1)     = timeDiffDc d2 c2 d1 c1
timeDiff (D.TimeYmd d2) (D.TimeYmd d1)  = Right $ D.ClockD $ timeDiffDate d2 d1
timeDiff (D.TimeYm  d2) (D.TimeYm  d1)  = Right $ D.ClockD $ timeDiffDay  d2 d1
timeDiff _ _ = Msg.adlib "time-diff"

timeDiffDc :: D.Date -> D.Clock -> D.Date -> D.Clock -> B.Ab D.Clock
timeDiffDc d2 c2 d1 c1 =
    do let d3 = timeDiffDate d2 d1
       c3 <- D.clockSub c2 c1
       Right $ D.clockAddDay d3 c3

timeDiffDate :: D.Date -> D.Date -> Integer
timeDiffDate d2 d1 = D.dateMjd d2 `timeDiffDay` D.dateMjd d1

timeDiffDay :: D.Mjd -> D.Mjd -> Integer
timeDiffDay d2 d1 = Tim.toModifiedJulianDay d2 - Tim.toModifiedJulianDay d1


-- ----------------------  First day

-- | Convert to the first day of month.
--
--   >>> timeFloorMonth $ timeFromYmd 2014 11 3
--   2014-11-01
--
timeFloorMonth :: O.Map D.Time
timeFloorMonth time =
    case D.timeYmdTuple time of
      (y, m, _) -> D.timeFromYmd y m 1

-- | Convert to the first day of year.
--
--   >>> timeFloorYear $ timeFromYmd 2014 11 3
--   2014-01-01
--
timeFloorYear :: O.Map D.Time
timeFloorYear time =
    case D.timeYmdTuple time of
      (y, _, _) -> D.timeFromYmd y 1 1

-- | Convert to the first day of next month.
--
--   >>> timeCeilMonth $ timeFromYmd 2014 11 3
--   2014-12-01
--
--   >>> timeCeilMonth $ timeFromYmd 2014 12 25
--   2015-01-01
--
timeCeilMonth :: O.Map D.Time
timeCeilMonth time =
    case D.timeYmdTuple time of
      (y, m, _) -> D.timeFromYmdTuple $ monthUp (y, m, 1)

-- | Convert to the first day of next year.
--
--    >>> timeCeilYaer $ timeFromYmd 2014 11 3
--    2015-01-01
--
timeCeilYaer :: O.Map D.Time
timeCeilYaer time =
    case D.timeYmdTuple time of
      (y, _, _) -> D.timeFromYmdTuple $ yearUp (y, 1, 1)


-- ----------------------  Range

-- | Create range of time.
--
--   >>> timeRangeDay (timeFromYmd 2014 11 3) (timeFromYmd 2014 11 5)
--   [2014-11-03, 2014-11-04, 2014-11-05]
--
timeRangeDay :: B.RangeBy D.Time
timeRangeDay from to = map D.timeFromMjd [D.timeMjd from .. D.timeMjd to]

-- | Create range of time.
--
--   >>> timeRangeMonth (timeFromYmd 2014 12 31) (timeFromYmd 2015 03 5)
--   [2014-12-31, 2015-01-31, 2015-02-28]
--
timeRangeMonth :: B.RangeBy D.Time
timeRangeMonth = timeRangeBy monthUp

-- | Create range of time.
timeRangeYear :: B.RangeBy D.Time
timeRangeYear = timeRangeBy yearUp

timeRangeBy :: O.Map D.Ymd -> B.RangeBy D.Time
timeRangeBy step from to = times where
    dayFrom =  D.timeYmdTuple from
    dayTo   =  D.timeYmdTuple to
    times   =  map D.timeFromYmdTuple $ B.rangeBy step dayFrom dayTo

-- | Increment month.
monthUp :: O.Map D.Ymd
monthUp (y, m, d) | m < 12    = (y, m + 1, d)
                  | otherwise = (y + 1, 1, d)

-- | Increment year.
yearUp :: O.Map D.Ymd
yearUp (y, m, d)  | y == (-1) = (1, m, d)
                  | otherwise = (y + 1, m, d)
