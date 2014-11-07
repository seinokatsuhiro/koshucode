{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Data.Time
  ( -- * Type
    Time (..), Year, Month, Day,

    -- * Construct
    timeFromYmAb, timeFromYmdAb, timeFromYmdcAb,
    timeFromMjd, timeMjd,
    timeMapMjd, timePrecision,

    -- * First day
    timeFloorMonth, timeFloorYear,
    timeCeilMonth, timeCeilYaer,
    -- $FirstDay

    -- * Range
    timeRangeDay, timeRangeMonth, timeRangeYear,

    -- * Add
    timeAddDay, timeAddWeek, timeAddMonth, timeAddYear,
    timeAddClock, timeDiff,
  ) where

import qualified Data.Time.Calendar               as T
import qualified Koshucode.Baala.Base.Abort       as B
import qualified Koshucode.Baala.Base.Prelude     as B
import qualified Koshucode.Baala.Base.Text        as B
import qualified Koshucode.Baala.Base.Data.Clock  as B
import qualified Koshucode.Baala.Base.Message     as Msg

data Time
    = TimeYmdc T.Day B.Clock
    | TimeYmd  T.Day
    | TimeYm   T.Day
      deriving (Show, Eq, Ord)

instance B.Write Time where
    write _ = writeTime

writeTime :: Time -> B.Doc
writeTime (TimeYmdc day clock)  = writeDay day B.<+> B.writeClockBody clock
writeTime (TimeYmd  day)        = writeDay day
writeTime (TimeYm   day)        = case T.toGregorian day of
                                    (y, m, _) -> B.doc y `h` B.doc02 m

writeDay :: T.Day -> B.Doc
writeDay day = case T.toGregorian day of
                 (y, m, d) -> B.doc y `h` B.doc02 m `h` B.doc02 d

h :: B.Bin B.Doc
h = B.docConcat "-"

type Year  = Integer
type Month = Int
type Day   = Int


-- ----------------------  Construct

timeFromYmAb :: Year -> Month -> B.Ab Time
timeFromYmAb y m =
    case T.fromGregorianValid y m 1 of
      Just day -> Right $ TimeYm day
      Nothing  -> Msg.notDate y m 1

timeFromYmdAb :: Year -> Month -> Day -> B.Ab Time
timeFromYmdAb y m d =
    case T.fromGregorianValid y m d of
      Just day -> Right $ TimeYmd day
      Nothing  -> Msg.notDate y m d

timeFromYmdcAb :: Year -> Month -> Day -> B.Clock -> B.Ab Time
timeFromYmdcAb y m d c =
    case T.fromGregorianValid y m d of
      Just day -> Right $ TimeYmdc day c
      Nothing  -> Msg.notDate y m d

timeFromYmd :: Year -> Month -> Day -> Time
timeFromYmd y m d = TimeYmd $ T.fromGregorian y m d

timeFromYmdTuple :: (Year, Month, Day) -> Time
timeFromYmdTuple = TimeYmd . fromGregorianTuple

fromGregorianTuple :: (Year, Month, Day) -> T.Day
fromGregorianTuple (y, m, d) = T.fromGregorian y m d

timePrecision :: Time -> String
timePrecision (TimeYmdc _ c)  = B.clockPrecision c
timePrecision (TimeYmd  _)    = "day"
timePrecision (TimeYm   _)    = "month"

timeDay :: Time -> T.Day
timeDay (TimeYmdc day _)  = day
timeDay (TimeYmd  day)    = day
timeDay (TimeYm   day)    = day

timeMjd :: Time -> B.DayCount
timeMjd = T.toModifiedJulianDay . timeDay

timeFromMjd :: B.DayCount -> Time
timeFromMjd = TimeYmd . T.ModifiedJulianDay

timeMapMjd :: B.Map B.DayCount -> B.Map Time
timeMapMjd f time = timeMapDay g time where
    g (T.ModifiedJulianDay d) = T.ModifiedJulianDay $ f d

timeMapDay :: B.Map T.Day -> B.Map Time
timeMapDay f (TimeYmdc day c)  = TimeYmdc (f day) c
timeMapDay f (TimeYmd  day)    = TimeYmd  $ f day
timeMapDay f (TimeYm   day)    = TimeYm   $ f day


-- ----------------------  First day

-- $FirstDay
--
--  /Examples/
--
--  The first day of month.
--
--    >>> timeFloorMonth $ timeFromYmd 2014 11 3
--    TimeYmd 2014-11-01
--
--  The first day of year.
--
--    >>> timeFloorYear $ timeFromYmd 2014 11 3
--    TimeYmd 2014-01-01
--
--  The first day of next month.
--
--    >>> timeCeilMonth $ timeFromYmd 2014 11 3
--    TimeYmd 2014-12-01
--
--  The first day of next year.
--
--    >>> timeCeilYaer $ timeFromYmd 2014 11 3
--    TimeYmd 2015-01-01

timeFloorMonth :: B.Map Time
timeFloorMonth time =
    case timeGregorian time of
      (y, m, _) -> timeFromYmd y m 1

timeFloorYear :: B.Map Time
timeFloorYear time =
    case timeGregorian time of
      (y, _, _) -> timeFromYmd y 1 1

timeCeilMonth :: B.Map Time
timeCeilMonth time =
    case timeGregorian time of
      (y, m, _) -> timeFromYmdTuple $ monthStep (y, m, 1)

timeCeilYaer :: B.Map Time
timeCeilYaer time =
    case T.toGregorian $ timeDay time of
      (y, _, _) -> timeFromYmdTuple $ yearStep (y, 1, 1)


-- ----------------------  Range

timeRangeDay :: B.RangeBy Time
timeRangeDay from to = map timeFromMjd [timeMjd from .. timeMjd to]

timeRangeMonth :: B.RangeBy Time
timeRangeMonth = timeRangeBy monthStep

timeRangeYear :: B.RangeBy Time
timeRangeYear = timeRangeBy yearStep

timeRangeBy :: B.Map (Year, Month, Day) -> B.RangeBy Time
timeRangeBy step from to = times where
    dayFrom =  T.toGregorian $ timeDay from
    dayTo   =  T.toGregorian $ timeDay to
    time    =  TimeYmd . fromGregorianTuple
    times   =  map time $ B.rangeBy step dayFrom dayTo

monthStep :: B.Map (Year, Month, Day)
monthStep (y, m, d) | m < 12    = (y, m + 1, d)
                    | otherwise = (y + 1, 1, d)

yearStep :: B.Map (Year, Month, Day)
yearStep (y, m, d)  | y == (-1) = (1, m, d)
                    | otherwise = (y + 1, m, d)

timeGregorian :: Time -> (Year, Month, Day)
timeGregorian = T.toGregorian . timeDay


-- ----------------------  Add

timeAddDay :: B.DayCount -> B.Map Time
timeAddDay n = timeMapMjd (+ n)

timeAddWeek :: B.DayCount -> B.Map Time
timeAddWeek n = timeAddDay (7 * n)

timeAddMonth :: B.DayCount -> B.Map Time
timeAddMonth n time =
    let (y, m, d) = T.toGregorian $ timeDay time
        (yd, m')  = (toInteger m + n) `divMod` 12
        y'        = y + yd
    in timeFromYmd y' (fromInteger m') d

timeAddYear :: Year -> B.Map Time
timeAddYear n time =
    let (y, m, d) = T.toGregorian $ timeDay time
        y'        = y + n
    in timeFromYmd y' m d

timeAddClock :: B.Clock -> B.AbMap Time
timeAddClock c1 (TimeYmdc d2 c2) =
    do c3 <- B.clockAdd c1 c2
       let d3 = B.clockDayCount c3
           c4 = B.clockCutDay   c3
       Right $ timeAddDay d3 $ TimeYmdc d2 c4
timeAddClock (B.ClockD d) t@(TimeYmd _) = Right $ timeAddDay d t
timeAddClock _ _ = Msg.adlib "clock-time"

timeDiff :: Time -> Time -> B.Ab B.Clock
timeDiff (TimeYmdc d2 c2) (TimeYmdc d1 c1) =
    do let d3 = T.toModifiedJulianDay d2 - T.toModifiedJulianDay d1
       c3 <- B.clockSub c2 c1
       Right $ B.clockAddDay d3 c3
timeDiff (TimeYmd d2) (TimeYmd d1) =
    Right $ B.ClockD $ T.toModifiedJulianDay d2 - T.toModifiedJulianDay d1
timeDiff (TimeYm d2) (TimeYm d1) =
    Right $ B.ClockD $ T.toModifiedJulianDay d2 - T.toModifiedJulianDay d1
timeDiff _ _ = Msg.adlib "timeDiff"

