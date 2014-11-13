{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Data.Time
  ( -- * Data type
    Time (..),
    timeYmd,

    -- * Construct
    timeFromYmAb, timeFromYwAb,
    timeFromYmdAb, timeFromYwdAb, timeFromYdAb,
    timeFromYmdcAb, timeFromYmdczAb,
    timeFromMjd, timeMjd,
    timeMapDate, timeMapMjd, timePrecision,

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
import qualified Data.Time.Calendar.WeekDate      as T
import qualified Data.Time.Calendar.OrdinalDate   as T
import qualified Koshucode.Baala.Base.Abort       as B
import qualified Koshucode.Baala.Base.Prelude     as B
import qualified Koshucode.Baala.Base.Text        as B
import qualified Koshucode.Baala.Base.Data.Clock  as B
import qualified Koshucode.Baala.Base.Data.Date   as B
import qualified Koshucode.Baala.Base.Message     as Msg


-- ----------------------  Time

data Time
    = TimeYmdcz B.Date B.Clock B.Sec  -- ^ Date and time with time zone
    | TimeYmdc  B.Date B.Clock        -- ^ Date and time
    | TimeYmd   B.Date                -- ^ Year, month, and day
    | TimeYw    T.Day                 -- ^ Year and week
    | TimeYm    T.Day                 -- ^ Year and month
      deriving (Show, Eq, Ord)

timeYmdc  :: T.Day -> B.Clock -> Time
timeYmdc   = TimeYmdc . B.Monthly

timeYmd   :: T.Day -> Time
timeYmd    = TimeYmd . B.Monthly


-- ----------------------  Write

instance B.Write Time where
    write _ = writeTime

writeTime :: Time -> B.Doc
writeTime (TimeYmdcz d c z) = body B.<+> szone where
    body       = writeDateTime d $ B.clockAddSec z c
    (_,h,m,_)  = B.dhmsFromSec z
    zone       = B.docConcat ":" (B.doc02 h) (B.doc02 m)
    szone      = case z `compare` 0 of
                   EQ -> B.doc "UTC"
                   LT -> B.doc "-" B.<> zone
                   GT -> B.doc "+" B.<> zone

writeTime (TimeYmdc  d c)   = writeDateTime d c
writeTime (TimeYmd   d)     = B.write id d
writeTime (TimeYw    d)     = case T.toWeekDate d of
                                (y, w, _) -> B.doc y `hyw` B.doc w
writeTime (TimeYm    d)     = case T.toGregorian d of
                                (y, m, _) -> B.doc y `hy` B.doc02 m

writeDateTime :: B.Date -> B.Clock -> B.Doc
writeDateTime d c = B.write id d B.<+> B.writeClockBody c

hy, hyw :: B.Bin B.Doc
hy   = B.docConcat "-"
hyw  = B.docConcat "-#"


-- ----------------------  Construct

-- | Create time data from year and month.
timeFromYmAb :: B.Year -> B.Month -> B.Ab Time
timeFromYmAb y m =
    case T.fromGregorianValid y m 1 of
      Just day -> Right $ TimeYm day
      Nothing  -> Msg.notDate y m 1

-- | Create time data from year and week.
timeFromYwAb :: B.Year -> B.Week -> B.Ab Time
timeFromYwAb y w =
    case T.fromWeekDateValid y w 1 of
      Just day -> Right $ TimeYw day
      Nothing  -> Msg.notDate y w 1

-- | Create time data from year, month, and day.
timeFromYmdAb :: B.Year -> B.Month -> B.Day -> B.Ab Time
timeFromYmdAb y m d =
    case T.fromGregorianValid y m d of
      Just day -> Right $ timeYmd day
      Nothing  -> Msg.notDate y m d

-- | Create time data from year, week, and day.
timeFromYwdAb :: B.Year -> B.Week -> B.Day -> B.Ab Time
timeFromYwdAb y w d =
    case T.fromWeekDateValid y w d of
      Just day -> Right $ TimeYmd $ B.Weekly day
      Nothing  -> Msg.notDate y w d

-- | Create time data from year and day.
timeFromYdAb :: B.Year -> a -> B.Day -> B.Ab Time
timeFromYdAb y _ d =
    case T.fromOrdinalDateValid y d of
      Just day -> Right $ TimeYmd $ B.Yearly day
      Nothing  -> Msg.notDate y 0 d

-- | Create time data from year, month, day, and clock.
timeFromYmdcAb :: B.Year -> B.Month -> B.Day -> B.Clock -> B.Ab Time
timeFromYmdcAb y m d c =
    case T.fromGregorianValid y m d of
      Just day -> Right $ timeYmdc day c
      Nothing  -> Msg.notDate y m d

-- | Create time data from year, month, day, clock, and time zone.
timeFromYmdczAb :: B.Date -> B.Clock -> Maybe B.Sec -> B.Ab Time
timeFromYmdczAb d c Nothing   = Right $ TimeYmdc  d c
timeFromYmdczAb d c (Just z)  = Right $ TimeYmdcz d c z

timeFromYmd :: B.Year -> B.Month -> B.Day -> Time
timeFromYmd y m d = timeYmd $ T.fromGregorian y m d

timeFromYmdTuple :: (B.Year, B.Month, B.Day) -> Time
timeFromYmdTuple = timeYmd . fromGregorianTuple

fromGregorianTuple :: (B.Year, B.Month, B.Day) -> T.Day
fromGregorianTuple (y, m, d) = T.fromGregorian y m d

timePrecision :: Time -> String
timePrecision (TimeYmdcz _ c _)  = B.clockPrecision c ++ " zone"
timePrecision (TimeYmdc  _ c)    = B.clockPrecision c
timePrecision (TimeYmd   _)      = "day"
timePrecision (TimeYw    _)      = "week"
timePrecision (TimeYm    _)      = "month"

timeDay :: Time -> T.Day
timeDay (TimeYmdcz d _ _)  = B.dateDay d
timeDay (TimeYmdc  d _)    = B.dateDay d
timeDay (TimeYmd   d)      = B.dateDay d
timeDay (TimeYw    d)      = d
timeDay (TimeYm    d)      = d

timeMjd :: Time -> B.DayCount
timeMjd = T.toModifiedJulianDay . timeDay

-- | Create time data form modified Julian date.
timeFromMjd :: B.DayCount -> Time
timeFromMjd = timeYmd . T.ModifiedJulianDay

timeMapMjd :: B.Map B.DayCount -> B.Map Time
timeMapMjd f time = timeMapDay g time where
    g (T.ModifiedJulianDay d) = T.ModifiedJulianDay $ f d

timeMapDay :: B.Map T.Day -> B.Map Time
timeMapDay f (TimeYmdcz d c z)  = TimeYmdcz (B.dateMapDay f d) c z
timeMapDay f (TimeYmdc  d c)    = TimeYmdc  (B.dateMapDay f d) c
timeMapDay f (TimeYmd   d)      = TimeYmd   (B.dateMapDay f d)
timeMapDay f (TimeYw    d)      = TimeYw    (f d)
timeMapDay f (TimeYm    d)      = TimeYm    (f d)

timeMapDate :: B.Map B.Date -> B.Map Time
timeMapDate f (TimeYmdcz d c z)  = TimeYmdcz (f d) c z
timeMapDate f (TimeYmdc  d c)    = TimeYmdc  (f d) c
timeMapDate f (TimeYmd   d)      = TimeYmd   (f d)
timeMapDate _ (TimeYw    d)      = TimeYw    d
timeMapDate _ (TimeYm    d)      = TimeYm    d


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

timeRangeBy :: B.Map (B.Year, B.Month, B.Day) -> B.RangeBy Time
timeRangeBy step from to = times where
    dayFrom =  T.toGregorian $ timeDay from
    dayTo   =  T.toGregorian $ timeDay to
    time    =  timeYmd . fromGregorianTuple
    times   =  map time $ B.rangeBy step dayFrom dayTo

monthStep :: B.Map (B.Year, B.Month, B.Day)
monthStep (y, m, d) | m < 12    = (y, m + 1, d)
                    | otherwise = (y + 1, 1, d)

yearStep :: B.Map (B.Year, B.Month, B.Day)
yearStep (y, m, d)  | y == (-1) = (1, m, d)
                    | otherwise = (y + 1, m, d)

timeGregorian :: Time -> (B.Year, B.Month, B.Day)
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

timeAddYear :: B.Year -> B.Map Time
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
    do let d3 = timeDiffBody d2 d1
       c3 <- B.clockSub c2 c1
       Right $ B.clockAddDay d3 c3
timeDiff (TimeYmd d2) (TimeYmd d1) =
    Right $ B.ClockD $ timeDiffBody d2 d1
timeDiff (TimeYm d2) (TimeYm d1) =
    Right $ B.ClockD $ T.toModifiedJulianDay d2 - T.toModifiedJulianDay d1
timeDiff _ _ = Msg.adlib "timeDiff"

timeDiffBody :: B.Date -> B.Date -> Integer
timeDiffBody d2 d1 = d2' - d1' where
    d2' = T.toModifiedJulianDay $ B.dateDay d2
    d1' = T.toModifiedJulianDay $ B.dateDay d1
