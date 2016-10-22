{-# OPTIONS_GHC -Wall #-}

-- | Time

module Koshucode.Baala.Data.Type.Time.Time
  ( -- * Data type
    Time (..),
    timeMjd, timePrecision,

    -- * Construction
    timeYmd, timeYmdc, timeFromMjd,
    timeFromYmAb, timeFromYwAb,
    timeFromDczAb,

    -- * Construction with I/O
    today, now, nowZoned,

    -- * Conversion
    timeOmitClock, timeOmitZone,
    timeMapDate, timeMapMjd,
    timeFromZonedTime,

    -- * First day
    timeFloorMonth, timeFloorYear,
    timeCeilMonth, timeCeilYaer,

    -- * Range
    timeRangeDay, timeRangeMonth, timeRangeYear,

    -- * Arithmetic
    timeAddDay, timeAddWeek, timeAddMonth, timeAddYear,
    timeAddClock, timeDiff,
  ) where

import qualified Data.Time.Calendar                     as Tim
import qualified Data.Time.Calendar.WeekDate            as Tim
import qualified Data.Time.LocalTime                    as Tim
import qualified Koshucode.Baala.Overture               as O
import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Data.Type.Time.Clock   as D
import qualified Koshucode.Baala.Data.Type.Time.Date    as D
import qualified Koshucode.Baala.Base.Message           as Msg
import qualified Koshucode.Baala.Data.Type.Message      as Msg


-- ----------------------  Time

-- | Time is a small duration on timeline.
data Time
    = TimeYmdcz D.Date D.Clock D.Sec  -- ^ Date and time with time zone
    | TimeYmdc  D.Date D.Clock        -- ^ Date and time
    | TimeYmd   D.Date                -- ^ Year, month, and day
    | TimeYw    D.MJDay               -- ^ Year and week
    | TimeYm    D.MJDay               -- ^ Year and month
      deriving (Eq, Ord)

-- | Get integer content of the Modified Julian Day of time.
timeMjd :: Time -> D.DayCount
timeMjd = Tim.toModifiedJulianDay . timeDay

-- Get the Modified Julian Day of time.
timeDay :: Time -> D.MJDay
timeDay (TimeYmdcz d _ _)   = D.dateDay d
timeDay (TimeYmdc  d _)     = D.dateDay d
timeDay (TimeYmd   d)       = D.dateDay d
timeDay (TimeYw    d)       = d
timeDay (TimeYm    d)       = d

-- | Get the name of time precision.
timePrecision :: Time -> String
timePrecision (TimeYmdcz _ c _)  = D.clockPrecision c ++ " zone"
timePrecision (TimeYmdc  _ c)    = D.clockPrecision c
timePrecision (TimeYmd   _)      = "day"
timePrecision (TimeYw    _)      = "week"
timePrecision (TimeYm    _)      = "month"


-- ----------------------  Write

instance Show Time where
    show = B.mixToFlatString . B.mixEncode

instance B.MixEncode Time where
    mixEncode = timeToMix

timeToMix :: Time -> B.MixText
timeToMix time =
    case time of
      TimeYmdcz d c z  -> dcz d c z `B.mixSep` szone z
      TimeYmdc  d c    -> dc d c
      TimeYmd   d      -> B.mixEncode d
      TimeYw    day    -> yw $ Tim.toWeekDate  day
      TimeYm    day    -> ym $ Tim.toGregorian day
    where
      dcz               :: D.Date -> D.Clock -> D.Sec -> B.MixText
      dcz d c z         = let c'  = D.clockAddSec z c
                              day = D.clockDayCount c'
                          in dc (day `D.dateAdd` d) (D.clockCutDay c')

      dc                :: D.Date -> D.Clock -> B.MixText
      dc d c            = B.mixEncode d `B.mixSep` D.clockBodyToMix c

      zone z            = hm $ D.dhmsFromSec z
      szone z           = case z `compare` 0 of
                            EQ -> B.mixString "UTC"
                            LT -> B.mixString "-" B.<> zone z
                            GT -> B.mixString "+" B.<> zone z

      hm (_, h, m, _)   = B.mixJoin ":"  [D.mix02 h, D.mix02 m]
      yw (y, w, _)      = B.mixJoin "-#" [B.mixDec y, B.mixDec w]
      ym (y, m, _)      = B.mixJoin "-"  [B.mixDec y, D.mix02 m]


-- ----------------------  Construct

-- | Create monthly date from the Modified Julian Day.
timeYmd :: D.MJDay -> Time
timeYmd = TimeYmd . D.Monthly

-- | Create monthly time from the Modified Julian Day and clock.
timeYmdc :: D.MJDay -> D.Clock -> Time
timeYmdc = TimeYmdc . D.Monthly

-- | Create time data form modified Julian date.
--
--   >>> timeFromMjd 55555
--   2010-12-25
--
timeFromMjd :: D.DayCount -> Time
timeFromMjd = timeYmd . Tim.ModifiedJulianDay

-- | Create time data from year and month.
timeFromYmAb :: D.Year -> D.Month -> B.Ab Time
timeFromYmAb y m =
    case Tim.fromGregorianValid y m 1 of
      Just day -> Right $ TimeYm day
      Nothing  -> Msg.notDate y m 1

-- | Create time data from year and week.
timeFromYwAb :: D.Year -> D.Week -> B.Ab Time
timeFromYwAb y w =
    case Tim.fromWeekDateValid y w 1 of
      Just day -> Right $ TimeYw day
      Nothing  -> Msg.notDate y w 1

-- | Create time data from date, clock, and time zone.
timeFromDczAb :: D.Date -> D.Clock -> Maybe D.Sec -> B.Ab Time
timeFromDczAb d c Nothing   = Right $ TimeYmdc  d c
timeFromDczAb d c (Just z)  = Right $ TimeYmdcz d c z

timeFromYmd :: D.Year -> D.Month -> D.Day -> Time
timeFromYmd y m d = timeYmd $ Tim.fromGregorian y m d

timeFromYmdTuple :: D.YmdTuple -> Time
timeFromYmdTuple = timeYmd . dayFromYmdTuple where
    dayFromYmdTuple (y, m, d) = Tim.fromGregorian y m d

timeYmdTuple :: Time -> D.YmdTuple
timeYmdTuple = Tim.toGregorian . timeDay

-- | Get today.
--
--   >>> today
--   2013-04-18
--
today :: IO Time
today = do time <- now
           return $ timeOmitClock time

-- | Get current time without time zone.
--
--   >>> now
--   2013-04-18 08:37:36
--
now :: IO Time
now = do time <- Tim.getZonedTime
         return $ timeOmitZone $ timeFromZonedTime time

-- | Get current time with time zone.
--
--   >>> nowZoned
--   2013-04-18 08:37:36 +09:00
--
nowZoned :: IO Time
nowZoned = do time <- Tim.getZonedTime
              return $ timeFromZonedTime time


-- ----------------------  Conversion

-- | Omit timezone.
timeOmitZone :: O.Map Time
timeOmitZone (TimeYmdcz d c _)  = TimeYmdc d c
timeOmitZone (TimeYmdc  d c)    = TimeYmdc d c
timeOmitZone (TimeYmd   d)      = TimeYmd d
timeOmitZone (TimeYw    d)      = TimeYw  d
timeOmitZone (TimeYm    d)      = TimeYm  d

-- | Omit clock part.
timeOmitClock :: O.Map Time
timeOmitClock (TimeYmdcz d _ _)  = TimeYmd d
timeOmitClock (TimeYmdc  d _)    = TimeYmd d
timeOmitClock (TimeYmd   d)      = TimeYmd d
timeOmitClock (TimeYw    d)      = TimeYw  d
timeOmitClock (TimeYm    d)      = TimeYm  d

-- | Map day part of time.
timeMapDate :: O.Map D.Date -> O.Map Time
timeMapDate f (TimeYmdcz d c z)  = TimeYmdcz (f d) c z
timeMapDate f (TimeYmdc  d c)    = TimeYmdc  (f d) c
timeMapDate f (TimeYmd   d)      = TimeYmd   (f d)
timeMapDate _ (TimeYw    d)      = TimeYw    d
timeMapDate _ (TimeYm    d)      = TimeYm    d

-- | Map day part of time.
timeMapDay :: O.Map D.MJDay -> O.Map Time
timeMapDay f (TimeYmdcz d c z)   = TimeYmdcz (D.dateMapDay f d) c z
timeMapDay f (TimeYmdc  d c)     = TimeYmdc  (D.dateMapDay f d) c
timeMapDay f (TimeYmd   d)       = TimeYmd   (D.dateMapDay f d)
timeMapDay f (TimeYw    d)       = TimeYw    (f d)
timeMapDay f (TimeYm    d)       = TimeYm    (f d)

-- | Map integer content of the Modified Julian Day.
timeMapMjd :: O.Map D.DayCount -> O.Map Time
timeMapMjd f time = timeMapDay g time where
    g (Tim.ModifiedJulianDay d) = Tim.ModifiedJulianDay $ f d

-- | Convert local time to Koshu time content.
timeFromZonedTime :: Tim.ZonedTime -> Time
timeFromZonedTime Tim.ZonedTime
                      { Tim.zonedTimeToLocalTime = Tim.LocalTime mjd (Tim.TimeOfDay h m s)
                      , Tim.zonedTimeZone = Tim.TimeZone { Tim.timeZoneMinutes = z }}
    = TimeYmdcz d (D.clockFromDhms 0 h m s') (60 * z)
      where
        d  = D.dateFromMjd $ Tim.toModifiedJulianDay mjd
        s' = fromEnum s `div` 1000000000000


-- ----------------------  First day

-- | Convert to the first day of month.
--
--   >>> timeFloorMonth $ timeFromYmd 2014 11 3
--   2014-11-01
--
timeFloorMonth :: O.Map Time
timeFloorMonth time =
    case timeYmdTuple time of
      (y, m, _) -> timeFromYmd y m 1

-- | Convert to the first day of year.
--
--   >>> timeFloorYear $ timeFromYmd 2014 11 3
--   2014-01-01
--
timeFloorYear :: O.Map Time
timeFloorYear time =
    case timeYmdTuple time of
      (y, _, _) -> timeFromYmd y 1 1

-- | Conbert to the first day of next month.
--
--   >>> timeCeilMonth $ timeFromYmd 2014 11 3
--   2014-12-01
--
--   >>> timeCeilMonth $ timeFromYmd 2014 12 25
--   2015-01-01
--
timeCeilMonth :: O.Map Time
timeCeilMonth time =
    case timeYmdTuple time of
      (y, m, _) -> timeFromYmdTuple $ monthUp (y, m, 1)

-- | Conbert to the first day of next year.
--
--    >>> timeCeilYaer $ timeFromYmd 2014 11 3
--    2015-01-01
--
timeCeilYaer :: O.Map Time
timeCeilYaer time =
    case timeYmdTuple time of
      (y, _, _) -> timeFromYmdTuple $ yearUp (y, 1, 1)


-- ----------------------  Range

-- | Create range of time.
--
--   >>> timeRangeDay (timeFromYmd 2014 11 3) (timeFromYmd 2014 11 5)
--   [2014-11-03, 2014-11-04, 2014-11-05]
--
timeRangeDay :: B.RangeBy Time
timeRangeDay from to = map timeFromMjd [timeMjd from .. timeMjd to]

-- | Create range of time.
--
--   >>> timeRangeMonth (timeFromYmd 2014 12 31) (timeFromYmd 2015 03 5)
--   [2014-12-31, 2015-01-31, 2015-02-28]
--
timeRangeMonth :: B.RangeBy Time
timeRangeMonth = timeRangeBy monthUp

-- | Create range of time.
timeRangeYear :: B.RangeBy Time
timeRangeYear = timeRangeBy yearUp

timeRangeBy :: O.Map D.YmdTuple -> B.RangeBy Time
timeRangeBy step from to = times where
    dayFrom =  timeYmdTuple from
    dayTo   =  timeYmdTuple to
    times   =  map timeFromYmdTuple $ B.rangeBy step dayFrom dayTo

-- | Increment month.
monthUp :: O.Map D.YmdTuple
monthUp (y, m, d) | m < 12    = (y, m + 1, d)
                  | otherwise = (y + 1, 1, d)

-- | Increment year.
yearUp :: O.Map D.YmdTuple
yearUp (y, m, d)  | y == (-1) = (1, m, d)
                  | otherwise = (y + 1, m, d)


-- ----------------------  Add

-- | Add days to time.
timeAddDay :: D.DayCount -> O.Map Time
timeAddDay n = timeMapMjd (+ n)

-- | Add weeks to time.
timeAddWeek :: Integer -> O.Map Time
timeAddWeek n = timeAddDay (7 * n)

-- | Add months to time.
timeAddMonth :: Integer -> O.Map Time
timeAddMonth n time = timeFromYmd y' (fromInteger m') d where
    (y, m, d)  = timeYmdTuple time
    (yd, m')   = (toInteger m + n) `divMod` 12
    y'         = y + yd

-- | Add years to time.
timeAddYear :: D.Year -> O.Map Time
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
timeDiffDay d2 d1 = Tim.toModifiedJulianDay d2 - Tim.toModifiedJulianDay d1
