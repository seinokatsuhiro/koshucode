{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Data.Type.Time
  ( -- * Data type
    Time (..),

    -- * Construct
    timeYmd, timeYmdc,
    timeFromYmAb, timeFromYwAb,
    timeFromDczAb,
    timeFromMjd, timeMjd,
    timeMapDate, timeMapMjd, timePrecision,

    -- * First day
    timeFloorMonth, timeFloorYear,
    timeCeilMonth, timeCeilYaer,
    -- $FirstDay

    -- * Range
    timeRangeDay, timeRangeMonth, timeRangeYear,

    -- * Arithmetic
    timeAddDay, timeAddWeek, timeAddMonth, timeAddYear,
    timeAddClock, timeDiff,
  ) where

import qualified Data.Time.Calendar               as T
import qualified Data.Time.Calendar.WeekDate      as T
import qualified Koshucode.Baala.Base             as B
import qualified Koshucode.Baala.Data.Type.Clock  as B
import qualified Koshucode.Baala.Data.Type.Date   as B
import qualified Koshucode.Baala.Base.Message     as Msg


-- ----------------------  Time

data Time
    = TimeYmdcz B.Date B.Clock B.Sec  -- ^ Date and time with time zone
    | TimeYmdc  B.Date B.Clock        -- ^ Date and time
    | TimeYmd   B.Date                -- ^ Year, month, and day
    | TimeYw    B.MJDay               -- ^ Year and week
    | TimeYm    B.MJDay               -- ^ Year and month
      deriving (Show, Eq, Ord)

timeYmdc  :: B.MJDay -> B.Clock -> Time
timeYmdc   = TimeYmdc . B.Monthly

timeYmd   :: B.MJDay -> Time
timeYmd    = TimeYmd . B.Monthly

timeMjd :: Time -> B.DayCount
timeMjd = T.toModifiedJulianDay . timeDay

timeDay :: Time -> B.MJDay
timeDay (TimeYmdcz d _ _)        = B.dateDay d
timeDay (TimeYmdc  d _)          = B.dateDay d
timeDay (TimeYmd   d)            = B.dateDay d
timeDay (TimeYw    d)            = d
timeDay (TimeYm    d)            = d

timePrecision :: Time -> String
timePrecision (TimeYmdcz _ c _)  = B.clockPrecision c ++ " zone"
timePrecision (TimeYmdc  _ c)    = B.clockPrecision c
timePrecision (TimeYmd   _)      = "day"
timePrecision (TimeYw    _)      = "week"
timePrecision (TimeYm    _)      = "month"

timeMapDay :: B.Map B.MJDay -> B.Map Time
timeMapDay f (TimeYmdcz d c z)   = TimeYmdcz (B.dateMapDay f d) c z
timeMapDay f (TimeYmdc  d c)     = TimeYmdc  (B.dateMapDay f d) c
timeMapDay f (TimeYmd   d)       = TimeYmd   (B.dateMapDay f d)
timeMapDay f (TimeYw    d)       = TimeYw    (f d)
timeMapDay f (TimeYm    d)       = TimeYm    (f d)

timeMapDate :: B.Map B.Date -> B.Map Time
timeMapDate f (TimeYmdcz d c z)  = TimeYmdcz (f d) c z
timeMapDate f (TimeYmdc  d c)    = TimeYmdc  (f d) c
timeMapDate f (TimeYmd   d)      = TimeYmd   (f d)
timeMapDate _ (TimeYw    d)      = TimeYw    d
timeMapDate _ (TimeYm    d)      = TimeYm    d


-- ----------------------  Write

instance B.Write Time where
    writeDocWith _ = writeTime

writeTime :: Time -> B.Doc
writeTime time =
    case time of
      TimeYmdcz d c z  -> dcz d c z B.<+> szone z
      TimeYmdc  d c    -> dc d c
      TimeYmd   d      -> B.writeDocWith id d
      TimeYw    day    -> yw $ T.toWeekDate  day
      TimeYm    day    -> ym $ T.toGregorian day
    where
      dcz               :: B.Date -> B.Clock -> B.Sec -> B.Doc
      dcz d c z         = let c'  = B.clockAddSec z c
                              day = B.clockDayCount c'
                          in dc (day `B.dateAdd` d) (B.clockCutDay c')

      dc                :: B.Date -> B.Clock -> B.Doc
      dc d c            = B.writeDocWith id d B.<+> B.writeClockBody c

      zone z            = hm $ B.dhmsFromSec z
      szone z           = case z `compare` 0 of
                            EQ -> B.doc "UTC"
                            LT -> B.doc "-" B.<> zone z
                            GT -> B.doc "+" B.<> zone z

      hm (_, h, m, _)   = B.doc02 h `co`  B.doc02 m
      yw (y, w, _)      = B.doc y   `hyw` B.doc w
      ym (y, m, _)      = B.doc y   `hy`  B.doc02 m
      co                = B.docConcat ":"
      hy                = B.docConcat "-"
      hyw               = B.docConcat "-#"


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

-- | Create time data from date, clock, and time zone.
timeFromDczAb :: B.Date -> B.Clock -> Maybe B.Sec -> B.Ab Time
timeFromDczAb d c Nothing   = Right $ TimeYmdc  d c
timeFromDczAb d c (Just z)  = Right $ TimeYmdcz d c z

timeFromYmd :: B.Year -> B.Month -> B.Day -> Time
timeFromYmd y m d = timeYmd $ T.fromGregorian y m d

timeFromYmdTuple :: B.YmdTuple -> Time
timeFromYmdTuple = timeYmd . dayFromYmdTuple where
    dayFromYmdTuple (y, m, d) = T.fromGregorian y m d

timeYmdTuple :: Time -> B.YmdTuple
timeYmdTuple = T.toGregorian . timeDay

-- | Create time data form modified Julian date.
timeFromMjd :: B.DayCount -> Time
timeFromMjd = timeYmd . T.ModifiedJulianDay

timeMapMjd :: B.Map B.DayCount -> B.Map Time
timeMapMjd f time = timeMapDay g time where
    g (T.ModifiedJulianDay d) = T.ModifiedJulianDay $ f d


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
    case timeYmdTuple time of
      (y, m, _) -> timeFromYmd y m 1

timeFloorYear :: B.Map Time
timeFloorYear time =
    case timeYmdTuple time of
      (y, _, _) -> timeFromYmd y 1 1

timeCeilMonth :: B.Map Time
timeCeilMonth time =
    case timeYmdTuple time of
      (y, m, _) -> timeFromYmdTuple $ monthStep (y, m, 1)

timeCeilYaer :: B.Map Time
timeCeilYaer time =
    case timeYmdTuple time of
      (y, _, _) -> timeFromYmdTuple $ yearStep (y, 1, 1)


-- ----------------------  Range

timeRangeDay :: B.RangeBy Time
timeRangeDay from to = map timeFromMjd [timeMjd from .. timeMjd to]

timeRangeMonth :: B.RangeBy Time
timeRangeMonth = timeRangeBy monthStep

timeRangeYear :: B.RangeBy Time
timeRangeYear = timeRangeBy yearStep

timeRangeBy :: B.Map B.YmdTuple -> B.RangeBy Time
timeRangeBy step from to = times where
    dayFrom =  timeYmdTuple from
    dayTo   =  timeYmdTuple to
    times   =  map timeFromYmdTuple $ B.rangeBy step dayFrom dayTo

monthStep :: B.Map B.YmdTuple
monthStep (y, m, d) | m < 12    = (y, m + 1, d)
                    | otherwise = (y + 1, 1, d)

yearStep :: B.Map B.YmdTuple
yearStep (y, m, d)  | y == (-1) = (1, m, d)
                    | otherwise = (y + 1, m, d)


-- ----------------------  Add

-- | Add days to time.
timeAddDay :: B.DayCount -> B.Map Time
timeAddDay n = timeMapMjd (+ n)

-- | Add weeks to time.
timeAddWeek :: Integer -> B.Map Time
timeAddWeek n = timeAddDay (7 * n)

-- | Add months to time.
timeAddMonth :: Integer -> B.Map Time
timeAddMonth n time = timeFromYmd y' (fromInteger m') d where
    (y, m, d)  = timeYmdTuple time
    (yd, m')   = (toInteger m + n) `divMod` 12
    y'         = y + yd

-- | Add years to time.
timeAddYear :: B.Year -> B.Map Time
timeAddYear n time = timeFromYmd (y + n) m d where
    (y, m, d) = timeYmdTuple time

-- | Add clock to time.
timeAddClock :: B.Clock -> B.AbMap Time
timeAddClock c1 (TimeYmdcz d2 c2 z2) = timeAddClockWith time c1 d2 c2 where
    time d c = TimeYmdcz d c z2
timeAddClock c1 (TimeYmdc d2 c2) = timeAddClockWith TimeYmdc c1 d2 c2
timeAddClock (B.ClockD d) t@(TimeYmd _) = Right $ timeAddDay d t
timeAddClock _ _ = Msg.adlib "add-clock"

timeAddClockWith :: (B.Date -> B.Clock -> Time) -> B.Clock -> B.Date -> B.Clock -> B.Ab Time
timeAddClockWith time c1 d2 c2 =
    do c3 <- B.clockAdd c1 c2
       let d3 = B.clockDayCount c3
           c4 = B.clockCutDay   c3
       Right $ timeAddDay d3 $ time d2 c4


-- ----------------------  Sub

-- | Calculate clock from time to time.
timeDiff :: Time -> Time -> B.Ab B.Clock
timeDiff (TimeYmdcz d2 c2 _) (TimeYmdcz d1 c1 _)  = timeDiffDc d2 c2 d1 c1
timeDiff (TimeYmdc d2 c2)    (TimeYmdc d1 c1)     = timeDiffDc d2 c2 d1 c1
timeDiff (TimeYmd d2) (TimeYmd d1)  = Right $ B.ClockD $ timeDiffDate d2 d1
timeDiff (TimeYm  d2) (TimeYm  d1)  = Right $ B.ClockD $ timeDiffDay  d2 d1
timeDiff _ _ = Msg.adlib "time-diff"

timeDiffDc :: B.Date -> B.Clock -> B.Date -> B.Clock -> B.Ab B.Clock
timeDiffDc d2 c2 d1 c1 =
    do let d3 = timeDiffDate d2 d1
       c3 <- B.clockSub c2 c1
       Right $ B.clockAddDay d3 c3

timeDiffDate :: B.Date -> B.Date -> Integer
timeDiffDate d2 d1 = B.dateDay d2 `timeDiffDay` B.dateDay d1

timeDiffDay :: B.MJDay -> B.MJDay -> Integer
timeDiffDay d2 d1 = T.toModifiedJulianDay d2 - T.toModifiedJulianDay d1
