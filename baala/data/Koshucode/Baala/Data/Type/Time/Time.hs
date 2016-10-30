{-# OPTIONS_GHC -Wall #-}

-- | Time

module Koshucode.Baala.Data.Type.Time.Time
  ( -- * Data type
    Time (..), Zone,
    timeMjd,
    timeYmdTuple,
    timePrecision,

    -- * Construction
    timeYmd, timeYmdc, timeFromMjd,
    timeFromYmAb, timeFromYwAb,
    timeFromDczAb,
    timeFromYmd, timeFromYmdTuple,

    -- * Construction with I/O
    nowUtc, nowZoned, now, today,

    -- * Conversion
    timeCutClock, timeAltZone, timeCutZone,
    timeLocalize,
    timeAltDate, timeAltDays,
    timeFromZonedTime,
    timeDaysSec,
  ) where

import qualified Data.Time.Calendar                         as Tim
import qualified Data.Time.Calendar.WeekDate                as Tim
import qualified Data.Time.Clock                            as Tim
import qualified Data.Time.LocalTime                        as Tim
import qualified Koshucode.Baala.Overture                   as O
import qualified Koshucode.Baala.Base                       as B
import qualified Koshucode.Baala.Data.Type.Time.Clock       as D
import qualified Koshucode.Baala.Data.Type.Time.ClockCalc   as D
import qualified Koshucode.Baala.Data.Type.Time.Date        as D
import qualified Koshucode.Baala.Data.Type.Message          as Msg


-- ----------------------  Time

-- | Time is a small duration on timeline.
data Time
    = TimeYmdcz D.Date D.Clock Zone  -- ^ Date and clock with time zone
    | TimeYmdc  D.Date D.Clock       -- ^ Date and clock
    | TimeYmd   D.Date               -- ^ Year, month, and day
    | TimeYw    D.Mjd                -- ^ Year and week
    | TimeYm    D.Mjd                -- ^ Year and month
      deriving (Eq, Ord)

-- | Time zone as offset from UTC.
type Zone = D.Sec

-- | Get integer content of the Modified Julian Day of time.
--
--   >>> timeMjd $ timeFromMjd 55555
--   55555
--
timeMjd :: Time -> D.Days
timeMjd = Tim.toModifiedJulianDay . timeDay

-- Get the Modified Julian Day of time.
timeDay :: Time -> D.Mjd
timeDay (TimeYmdcz d _ _)   = D.dateMjd d
timeDay (TimeYmdc  d _)     = D.dateMjd d
timeDay (TimeYmd   d)       = D.dateMjd d
timeDay (TimeYw    d)       = d
timeDay (TimeYm    d)       = d

-- | Convert time to year\/month\/day tuple.
--
--   >>> timeYmdTuple $ timeFromMjd 55555
--   (2010,12,25)
--
timeYmdTuple :: Time -> D.Ymd
timeYmdTuple = Tim.toGregorian . timeDay

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
      dcz               :: D.Date -> D.Clock -> Zone -> B.MixText
      dcz d c z         = let c'       = D.clockAddSec z c
                              (day, _) = D.clockDaysSec c'
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
timeYmd :: D.Mjd -> Time
timeYmd = TimeYmd . D.Monthly

-- | Create monthly time from the Modified Julian Day and clock.
timeYmdc :: D.Mjd -> D.Clock -> Time
timeYmdc = TimeYmdc . D.Monthly

-- | Create time data form modified Julian date.
--
--   >>> timeFromMjd 55555
--   2010-12-25
--
timeFromMjd :: D.Days -> Time
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
timeFromDczAb :: D.Date -> D.Clock -> Maybe Zone -> B.Ab Time
timeFromDczAb d c Nothing  = Right $ timeFromDc  d c
timeFromDczAb d c (Just z) = Right $ timeFromDcz d c z

timeFromDc :: D.Date -> D.Clock -> Time
timeFromDc d c = let (d', c') = D.clockDaysClock c
                 in TimeYmdc (D.dateAdd d' d) c'

timeFromDcz :: D.Date -> D.Clock -> Zone -> Time
timeFromDcz d c z = let (d', c') = D.clockDaysClock c
                    in TimeYmdcz (D.dateAdd d' d) c' z

timeFromYmd :: D.Year -> D.Month -> D.Day -> Time
timeFromYmd y m d = timeYmd $ Tim.fromGregorian y m d

timeFromYmdTuple :: D.Ymd -> Time
timeFromYmdTuple = timeYmd . dayFromYmdTuple where
    dayFromYmdTuple (y, m, d) = Tim.fromGregorian y m d


-- ----------------------  Construction with I/O

-- | Get UTC.
--
--   >>> nowUtc
--   2013-04-18 08:37:36 UTC
--
nowUtc :: IO Time
nowUtc = do time <- Tim.getZonedTime
            let time' = time { Tim.zonedTimeZone = Tim.utc }
            return $ timeFromZonedTime time'

-- | Get current time with time zone.
--
--   >>> nowZoned
--   2013-04-18 17:37:36 +09:00
--
nowZoned :: IO Time
nowZoned = do time <- Tim.getZonedTime
              return $ timeFromZonedTime time

-- | Get current local time without time zone.
--
--   >>> now
--   2013-04-18 17:37:36
--
now :: IO Time
now = do time <- Tim.getZonedTime
         return $ timeLocalize $ timeFromZonedTime time

-- | Get today.
--
--   >>> today
--   2013-04-18
--
today :: IO Time
today = do time <- now
           return $ timeCutClock time


-- ----------------------  Conversion

-- | Cut timezone.
timeCutZone :: O.Map Time
timeCutZone (TimeYmdcz d c _)  = timeFromDc d c
timeCutZone (TimeYmdc  d c)    = TimeYmdc d c
timeCutZone (TimeYmd   d)      = TimeYmd d
timeCutZone (TimeYw    d)      = TimeYw  d
timeCutZone (TimeYm    d)      = TimeYm  d

-- | Cut timezone and convert to local time,
--   i.e., addition of UTC and time zone.
timeLocalize :: O.Map Time
timeLocalize (TimeYmdcz d c z)  = timeFromDc d $ D.clockAddSec z c
timeLocalize (TimeYmdc  d c)    = TimeYmdc d c
timeLocalize (TimeYmd   d)      = TimeYmd d
timeLocalize (TimeYw    d)      = TimeYw  d
timeLocalize (TimeYm    d)      = TimeYm  d

-- | Alter time zone part.
timeAltZone :: O.Map Zone -> O.Map Time
timeAltZone f (TimeYmdcz d c z)  = timeFromDcz d c $ f z
timeAltZone _ t@(TimeYmdc  _ _)  = t
timeAltZone _ t@(TimeYmd   _)    = t
timeAltZone _ t@(TimeYw    _)    = t
timeAltZone _ t@(TimeYm    _)    = t

-- | Cut clock part.
timeCutClock :: O.Map Time
timeCutClock (TimeYmdcz d _ _)  = TimeYmd d
timeCutClock (TimeYmdc  d _)    = TimeYmd d
timeCutClock (TimeYmd   d)      = TimeYmd d
timeCutClock (TimeYw    d)      = TimeYw  d
timeCutClock (TimeYm    d)      = TimeYm  d

-- | Alter day part.
timeAltDate :: O.Map D.Date -> O.Map Time
timeAltDate f (TimeYmdcz d c z)  = TimeYmdcz (f d) c z
timeAltDate f (TimeYmdc  d c)    = TimeYmdc  (f d) c
timeAltDate f (TimeYmd   d)      = TimeYmd   (f d)
timeAltDate _ (TimeYw    d)      = TimeYw    d
timeAltDate _ (TimeYm    d)      = TimeYm    d

-- | Alter the Modified Julian Day of date part.
timeAltMjd :: O.Map D.Mjd -> O.Map Time
timeAltMjd f (TimeYmdcz d c z)   = TimeYmdcz (D.dateAltMjd f d) c z
timeAltMjd f (TimeYmdc  d c)     = TimeYmdc  (D.dateAltMjd f d) c
timeAltMjd f (TimeYmd   d)       = TimeYmd   (D.dateAltMjd f d)
timeAltMjd f (TimeYw    d)       = TimeYw    (f d)
timeAltMjd f (TimeYm    d)       = TimeYm    (f d)

-- | Alter integer content of the Modified Julian Day.
timeAltDays :: O.Map D.Days -> O.Map Time
timeAltDays f time = timeAltMjd g time where
    g (Tim.ModifiedJulianDay d) = Tim.ModifiedJulianDay $ f d

-- | Convert local time to Koshu time content.
timeFromZonedTime :: Tim.ZonedTime -> Time
timeFromZonedTime zt = time where
    Tim.UTCTime mjd s = Tim.zonedTimeToUTC zt
    s'   = fromEnum s `div` 1000000000000
    d    = D.dateFromMjd $ Tim.toModifiedJulianDay mjd
    c    = D.clockFromDhms 0 0 0 s'
    time = TimeYmdcz d c (60 * Tim.timeZoneMinutes (Tim.zonedTimeZone zt))

-- | Days and seconds of time.
--
--   >>> timeDaysSec $ timeFromMjd 55555
--   (55555,0)
--
timeDaysSec :: Time -> D.DaysSec
timeDaysSec (TimeYmdcz d c _)  = dateClockDaysSec d c
timeDaysSec (TimeYmdc d c)     = dateClockDaysSec d c
timeDaysSec (TimeYmd d)        = (D.unMjd $ D.dateMjd d, 0)
timeDaysSec (TimeYw d)         = (D.unMjd d, 0)
timeDaysSec (TimeYm d)         = (D.unMjd d, 0)

dateClockDaysSec :: D.Date -> D.Clock -> D.DaysSec
dateClockDaysSec d c = let (d', s) = D.clockDaysSec c
                       in (D.unMjd (D.dateMjd d) + d', s)
