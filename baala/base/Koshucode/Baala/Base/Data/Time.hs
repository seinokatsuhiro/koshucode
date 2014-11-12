{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Data.Time
  ( -- * Data type
    DateForm (..), Time (..),
    Year, Month, Week, Day,
    timeYmd,

    -- * Construct
    timeFromYmAb, timeFromYwAb,
    timeFromYmdAb, timeFromYwdAb,
    timeFromYmdcAb, timeFromYmdczAb,
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
import qualified Data.Time.Calendar.WeekDate      as T
import qualified Data.Time.Calendar.OrdinalDate   as T
import qualified Koshucode.Baala.Base.Abort       as B
import qualified Koshucode.Baala.Base.Prelude     as B
import qualified Koshucode.Baala.Base.Text        as B
import qualified Koshucode.Baala.Base.Data.Clock  as B
import qualified Koshucode.Baala.Base.Message     as Msg


-- ----------------------  Data type

data DateForm
    = DateMonth    -- ^ Date in /YYYY-MM-DD/
    | DateWeek     -- ^ Date in /YYYY-#W-D/
    | DateDay      -- ^ Date in /YYYY-##D/
      deriving (Show, Eq, Ord)

data Time
    = TimeYmdcz DateForm T.Day B.Clock B.Sec  -- ^ Date and time with time zone
    | TimeYmdc  DateForm T.Day B.Clock        -- ^ Date and time
    | TimeYmd   DateForm T.Day                -- ^ Year, month, and day
    | TimeYw    T.Day                         -- ^ Year and week
    | TimeYm    T.Day                         -- ^ Year and month
      deriving (Show, Eq, Ord)

timeYmdcz :: T.Day -> B.Clock -> B.Sec -> Time
timeYmdcz  = TimeYmdcz DateMonth

timeYmdc  :: T.Day -> B.Clock -> Time
timeYmdc   = TimeYmdc DateMonth

timeYmd   :: T.Day -> Time
timeYmd    = TimeYmd DateMonth

type Year   = Integer
type Week   = Int
type Month  = Int
type Day    = Int


-- ----------------------  Write

instance B.Write Time where
    write _ = writeTime

writeTime :: Time -> B.Doc
writeTime (TimeYmdcz f d c z) = body B.<+> szone where
    body       = writeDateTime f d $ B.clockAddSec z c
    (_,h,m,_)  = B.dhmsFromSec z
    zone       = B.docConcat ":" (B.doc02 h) (B.doc02 m)
    szone      = case z `compare` 0 of
                   EQ -> B.doc "UTC"
                   LT -> B.doc "-" B.<> zone
                   GT -> B.doc "+" B.<> zone
writeTime (TimeYmdc  f d c)   = writeDateTime f d c
writeTime (TimeYmd   f d)     = writeDay f d
writeTime (TimeYw      d)     = case T.toWeekDate d of
                                  (y, w, _) -> B.doc y `hyw` B.doc w
writeTime (TimeYm      d)     = case T.toGregorian d of
                                  (y, m, _) -> B.doc y `hy` B.doc02 m

writeDateTime :: DateForm -> T.Day -> B.Clock -> B.Doc
writeDateTime f d c = writeDay f d B.<+> B.writeClockBody c

writeDay :: DateForm -> T.Day -> B.Doc
writeDay DateMonth day  = let (y, m, d) = T.toGregorian   day in dateMonth y m d
writeDay DateWeek  day  = let (y, w, d) = T.toWeekDate    day in dateWeek  y w d
writeDay DateDay   day  = let (y, d)    = T.toOrdinalDate day in dateDay   y d

dateMonth :: Year -> Month -> Day -> B.Doc
dateMonth y m d = B.doc y `hy` B.doc02 m `hy` B.doc02 d

dateWeek :: Year -> Week -> Day -> B.Doc
dateWeek  y w d = B.doc y `hyw` B.doc w `hy` B.doc d

dateDay :: Year -> Day -> B.Doc
dateDay   y d = B.doc y `hyww` B.doc d

hy, hyw, hyww :: B.Bin B.Doc
hy    = B.docConcat "-"
hyw   = B.docConcat "-#"
hyww  = B.docConcat "-##"


-- ----------------------  Construct

-- | Create time data from year and month.
timeFromYmAb :: Year -> Month -> B.Ab Time
timeFromYmAb y m =
    case T.fromGregorianValid y m 1 of
      Just day -> Right $ TimeYm day
      Nothing  -> Msg.notDate y m 1

-- | Create time data from year and week.
timeFromYwAb :: Year -> Week -> B.Ab Time
timeFromYwAb y w =
    case T.fromWeekDateValid y w 1 of
      Just day -> Right $ TimeYw day
      Nothing  -> Msg.notDate y w 1

-- | Create time data from year, month, and day.
timeFromYmdAb :: Year -> Month -> Day -> B.Ab Time
timeFromYmdAb y m d =
    case T.fromGregorianValid y m d of
      Just day -> Right $ timeYmd day
      Nothing  -> Msg.notDate y m d

-- | Create time data from year, week, and day.
timeFromYwdAb :: Year -> Week -> Day -> B.Ab Time
timeFromYwdAb y w d =
    case T.fromWeekDateValid y w d of
      Just day -> Right $ TimeYmd DateWeek day
      Nothing  -> Msg.notDate y w d

-- | Create time data from year, month, day, and clock.
timeFromYmdcAb :: Year -> Month -> Day -> B.Clock -> B.Ab Time
timeFromYmdcAb y m d c =
    case T.fromGregorianValid y m d of
      Just day -> Right $ timeYmdc day c
      Nothing  -> Msg.notDate y m d

-- | Create time data from year, month, day, clock, and time zone.
timeFromYmdczAb :: Year -> Month -> Day -> B.Clock -> Maybe B.Sec -> B.Ab Time
timeFromYmdczAb y m d c Nothing =
    case T.fromGregorianValid y m d of
      Just day -> Right $ timeYmdc day c
      Nothing  -> Msg.notDate y m d
timeFromYmdczAb y m d c (Just z) =
    case T.fromGregorianValid y m d of
      Just day -> Right $ timeYmdcz day c z
      Nothing  -> Msg.notDate y m d

timeFromYmd :: Year -> Month -> Day -> Time
timeFromYmd y m d = timeYmd $ T.fromGregorian y m d

timeFromYmdTuple :: (Year, Month, Day) -> Time
timeFromYmdTuple = timeYmd . fromGregorianTuple

fromGregorianTuple :: (Year, Month, Day) -> T.Day
fromGregorianTuple (y, m, d) = T.fromGregorian y m d

timePrecision :: Time -> String
timePrecision (TimeYmdcz _ _ c _)  = B.clockPrecision c ++ " zone"
timePrecision (TimeYmdc  _ _ c)    = B.clockPrecision c
timePrecision (TimeYmd   _ _)      = "day"
timePrecision (TimeYw      _)      = "week"
timePrecision (TimeYm      _)      = "month"

timeDay :: Time -> T.Day
timeDay (TimeYmdcz _ d _ _)  = d
timeDay (TimeYmdc  _ d _)    = d
timeDay (TimeYmd   _ d)      = d
timeDay (TimeYw      d)      = d
timeDay (TimeYm      d)      = d

timeMjd :: Time -> B.DayCount
timeMjd = T.toModifiedJulianDay . timeDay

-- | Create time data form modified Julian date.
timeFromMjd :: B.DayCount -> Time
timeFromMjd = timeYmd . T.ModifiedJulianDay

timeMapMjd :: B.Map B.DayCount -> B.Map Time
timeMapMjd f time = timeMapDay g time where
    g (T.ModifiedJulianDay d) = T.ModifiedJulianDay $ f d

timeMapDay :: B.Map T.Day -> B.Map Time
timeMapDay g (TimeYmdcz f day c z)  = TimeYmdcz f (g day) c z
timeMapDay g (TimeYmdc  f day c)    = TimeYmdc  f (g day) c
timeMapDay g (TimeYmd   f day)      = TimeYmd   f $ g day
timeMapDay g (TimeYw      day)      = TimeYw      $ g day
timeMapDay g (TimeYm      day)      = TimeYm      $ g day


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
    time    =  timeYmd . fromGregorianTuple
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
timeAddClock c1 (TimeYmdc f d2 c2) =
    do c3 <- B.clockAdd c1 c2
       let d3 = B.clockDayCount c3
           c4 = B.clockCutDay   c3
       Right $ timeAddDay d3 $ TimeYmdc f d2 c4
timeAddClock (B.ClockD d) t@(TimeYmd _ _) = Right $ timeAddDay d t
timeAddClock _ _ = Msg.adlib "clock-time"

timeDiff :: Time -> Time -> B.Ab B.Clock
timeDiff (TimeYmdc _ d2 c2) (TimeYmdc _ d1 c1) =
    do let d3 = T.toModifiedJulianDay d2 - T.toModifiedJulianDay d1
       c3 <- B.clockSub c2 c1
       Right $ B.clockAddDay d3 c3
timeDiff (TimeYmd _ d2) (TimeYmd _ d1) =
    Right $ B.ClockD $ T.toModifiedJulianDay d2 - T.toModifiedJulianDay d1
timeDiff (TimeYm d2) (TimeYm d1) =
    Right $ B.ClockD $ T.toModifiedJulianDay d2 - T.toModifiedJulianDay d1
timeDiff _ _ = Msg.adlib "timeDiff"

