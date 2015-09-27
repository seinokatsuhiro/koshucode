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
import qualified Koshucode.Baala.Data.Type.Clock  as D
import qualified Koshucode.Baala.Data.Type.Date   as D
import qualified Koshucode.Baala.Base.Message     as Msg


-- ----------------------  Time

data Time
    = TimeYmdcz D.Date D.Clock D.Sec  -- ^ Date and time with time zone
    | TimeYmdc  D.Date D.Clock        -- ^ Date and time
    | TimeYmd   D.Date                -- ^ Year, month, and day
    | TimeYw    D.MJDay               -- ^ Year and week
    | TimeYm    D.MJDay               -- ^ Year and month
      deriving (Show, Eq, Ord)

timeYmdc  :: D.MJDay -> D.Clock -> Time
timeYmdc   = TimeYmdc . D.Monthly

timeYmd   :: D.MJDay -> Time
timeYmd    = TimeYmd . D.Monthly

timeMjd :: Time -> D.DayCount
timeMjd = T.toModifiedJulianDay . timeDay

timeDay :: Time -> D.MJDay
timeDay (TimeYmdcz d _ _)        = D.dateDay d
timeDay (TimeYmdc  d _)          = D.dateDay d
timeDay (TimeYmd   d)            = D.dateDay d
timeDay (TimeYw    d)            = d
timeDay (TimeYm    d)            = d

timePrecision :: Time -> String
timePrecision (TimeYmdcz _ c _)  = D.clockPrecision c ++ " zone"
timePrecision (TimeYmdc  _ c)    = D.clockPrecision c
timePrecision (TimeYmd   _)      = "day"
timePrecision (TimeYw    _)      = "week"
timePrecision (TimeYm    _)      = "month"

timeMapDay :: B.Map D.MJDay -> B.Map Time
timeMapDay f (TimeYmdcz d c z)   = TimeYmdcz (D.dateMapDay f d) c z
timeMapDay f (TimeYmdc  d c)     = TimeYmdc  (D.dateMapDay f d) c
timeMapDay f (TimeYmd   d)       = TimeYmd   (D.dateMapDay f d)
timeMapDay f (TimeYw    d)       = TimeYw    (f d)
timeMapDay f (TimeYm    d)       = TimeYm    (f d)

timeMapDate :: B.Map D.Date -> B.Map Time
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
      dcz               :: D.Date -> D.Clock -> D.Sec -> B.Doc
      dcz d c z         = let c'  = D.clockAddSec z c
                              day = D.clockDayCount c'
                          in dc (day `D.dateAdd` d) (D.clockCutDay c')

      dc                :: D.Date -> D.Clock -> B.Doc
      dc d c            = B.writeDocWith id d B.<+> D.writeClockBody c

      zone z            = hm $ D.dhmsFromSec z
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
timeFromYmAb :: D.Year -> D.Month -> B.Ab Time
timeFromYmAb y m =
    case T.fromGregorianValid y m 1 of
      Just day -> Right $ TimeYm day
      Nothing  -> Msg.notDate y m 1

-- | Create time data from year and week.
timeFromYwAb :: D.Year -> D.Week -> B.Ab Time
timeFromYwAb y w =
    case T.fromWeekDateValid y w 1 of
      Just day -> Right $ TimeYw day
      Nothing  -> Msg.notDate y w 1

-- | Create time data from date, clock, and time zone.
timeFromDczAb :: D.Date -> D.Clock -> Maybe D.Sec -> B.Ab Time
timeFromDczAb d c Nothing   = Right $ TimeYmdc  d c
timeFromDczAb d c (Just z)  = Right $ TimeYmdcz d c z

timeFromYmd :: D.Year -> D.Month -> D.Day -> Time
timeFromYmd y m d = timeYmd $ T.fromGregorian y m d

timeFromYmdTuple :: D.YmdTuple -> Time
timeFromYmdTuple = timeYmd . dayFromYmdTuple where
    dayFromYmdTuple (y, m, d) = T.fromGregorian y m d

timeYmdTuple :: Time -> D.YmdTuple
timeYmdTuple = T.toGregorian . timeDay

-- | Create time data form modified Julian date.
timeFromMjd :: D.DayCount -> Time
timeFromMjd = timeYmd . T.ModifiedJulianDay

timeMapMjd :: B.Map D.DayCount -> B.Map Time
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

timeRangeBy :: B.Map D.YmdTuple -> B.RangeBy Time
timeRangeBy step from to = times where
    dayFrom =  timeYmdTuple from
    dayTo   =  timeYmdTuple to
    times   =  map timeFromYmdTuple $ B.rangeBy step dayFrom dayTo

monthStep :: B.Map D.YmdTuple
monthStep (y, m, d) | m < 12    = (y, m + 1, d)
                    | otherwise = (y + 1, 1, d)

yearStep :: B.Map D.YmdTuple
yearStep (y, m, d)  | y == (-1) = (1, m, d)
                    | otherwise = (y + 1, m, d)


-- ----------------------  Add

-- | Add days to time.
timeAddDay :: D.DayCount -> B.Map Time
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
timeAddYear :: D.Year -> B.Map Time
timeAddYear n time = timeFromYmd (y + n) m d where
    (y, m, d) = timeYmdTuple time

-- | Add clock to time.
timeAddClock :: D.Clock -> B.AbMap Time
timeAddClock c1 (TimeYmdcz d2 c2 z2) = timeAddClockWith time c1 d2 c2 where
    time d c = TimeYmdcz d c z2
timeAddClock c1 (TimeYmdc d2 c2) = timeAddClockWith TimeYmdc c1 d2 c2
timeAddClock (D.ClockD d) t@(TimeYmd _) = Right $ timeAddDay d t
timeAddClock _ _ = Msg.adlib "add-clock"

timeAddClockWith :: (D.Date -> D.Clock -> Time) -> D.Clock -> D.Date -> D.Clock -> B.Ab Time
timeAddClockWith time c1 d2 c2 =
    do c3 <- D.clockAdd c1 c2
       let d3 = D.clockDayCount c3
           c4 = D.clockCutDay   c3
       Right $ timeAddDay d3 $ time d2 c4


-- ----------------------  Sub

-- | Calculate clock from time to time.
timeDiff :: Time -> Time -> B.Ab D.Clock
timeDiff (TimeYmdcz d2 c2 _) (TimeYmdcz d1 c1 _)  = timeDiffDc d2 c2 d1 c1
timeDiff (TimeYmdc d2 c2)    (TimeYmdc d1 c1)     = timeDiffDc d2 c2 d1 c1
timeDiff (TimeYmd d2) (TimeYmd d1)  = Right $ D.ClockD $ timeDiffDate d2 d1
timeDiff (TimeYm  d2) (TimeYm  d1)  = Right $ D.ClockD $ timeDiffDay  d2 d1
timeDiff _ _ = Msg.adlib "time-diff"

timeDiffDc :: D.Date -> D.Clock -> D.Date -> D.Clock -> B.Ab D.Clock
timeDiffDc d2 c2 d1 c1 =
    do let d3 = timeDiffDate d2 d1
       c3 <- D.clockSub c2 c1
       Right $ D.clockAddDay d3 c3

timeDiffDate :: D.Date -> D.Date -> Integer
timeDiffDate d2 d1 = D.dateDay d2 `timeDiffDay` D.dateDay d1

timeDiffDay :: D.MJDay -> D.MJDay -> Integer
timeDiffDay d2 d1 = T.toModifiedJulianDay d2 - T.toModifiedJulianDay d1
