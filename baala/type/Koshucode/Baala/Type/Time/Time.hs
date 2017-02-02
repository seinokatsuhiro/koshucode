{-# OPTIONS_GHC -Wall #-}

-- | Time

module Koshucode.Baala.Type.Time.Time
  ( -- * Data type
    Time (..), Zone,
    timeYmdTuple,
    timePrecision,

    -- * Creation
    monthlyTime, monthlyClippedTime,
    monthlyTimeDate, monthlyTimeClock,
    timeFromMjd,
    timeFromYmAb, timeFromYwAb,
    timeFromDczAb,
    timeFromYmd, timeFromYmdTuple,

    -- * Conversion
    timeCutClock, timeSetClock,
    timeAltZone, timeCutZone,
    timeLocalize,
    timeAltDate, timeAltDays,
    timeFromZonedTime,
    timeDaysSec,

    -- * Creation with I/O
    nowUtc, nowZoned, now, today,
  ) where

import qualified Data.Time.Calendar                      as Tim
import qualified Data.Time.Calendar.WeekDate             as Tim
import qualified Data.Time.Clock                         as Tim
import qualified Data.Time.LocalTime                     as Tim
import qualified Koshucode.Baala.Overture                as O
import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Type.Time.Clock         as T
import qualified Koshucode.Baala.Type.Time.ClockCalc     as T
import qualified Koshucode.Baala.Type.Time.Date          as T
import qualified Koshucode.Baala.Type.Time.Parts         as T
import qualified Koshucode.Baala.Type.Message            as Msg


-- ----------------------  Time

-- | Time is a small duration on timeline.
data Time
    = TimeYmdcz T.Date T.Clock Zone  -- ^ Date and clock with time zone
    | TimeYmdc  T.Date T.Clock       -- ^ Date and clock
    | TimeYmd   T.Date               -- ^ Year, month, and day
    | TimeYw    T.Mjd                -- ^ Year and week
    | TimeYm    T.Mjd                -- ^ Year and month
      deriving (Eq, Ord)

-- | Time zone as offset from UTC.
type Zone = T.Sec

-- | The first day of the Modified Julian Day.
instance B.Default Time where
    def = timeFromMjd (0 :: Int)

-- | Extract date part and convert to MJT.
instance T.ToMjd Time where
    toMjd (TimeYmdcz d _ _)   = T.toMjd d
    toMjd (TimeYmdc  d _)     = T.toMjd d
    toMjd (TimeYmd   d)       = T.toMjd d
    toMjd (TimeYw    mjd)     = mjd
    toMjd (TimeYm    mjd)     = mjd

-- | Convert time to year\/month\/day tuple.
--
--   >>> timeYmdTuple $ timeFromMjd 55555
--   (2010,12,25)
--
timeYmdTuple :: Time -> T.Ymd
timeYmdTuple = Tim.toGregorian . T.toMjd

-- | Get the name of time precision.
timePrecision :: Time -> String
timePrecision (TimeYmdcz _ c _)  = T.clockPrecision c ++ " zone"
timePrecision (TimeYmdc  _ c)    = T.clockPrecision c
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
      dcz               :: T.Date -> T.Clock -> Zone -> B.MixText
      dcz d c z         = let c'       = T.clockAddSec z c
                              (day, _) = T.clockDaysSec c'
                          in dc (T.dateAddDay day d) (T.clockCutDay c')

      dc                :: T.Date -> T.Clock -> B.MixText
      dc d c            = B.mixEncode d `B.mixSep` T.clockBodyToMix c

      zone z            = hm $ T.dhmsFromSec z
      szone z           = case z `compare` 0 of
                            EQ -> B.mixString "UTC"
                            LT -> B.mixString "-" O.++ zone z
                            GT -> B.mixString "+" O.++ zone z

      hm (_, h, m, _)   = B.mixJoin ":"  [T.mix02 h, T.mix02 m]
      yw (y, w, _)      = B.mixJoin "-#" [B.mixDec y, B.mixDec w]
      ym (y, m, _)      = B.mixJoin "-"  [B.mixDec y, T.mix02 m]


-- ----------------------  Creation

-- | Create monthly time.
--
--   >>> monthlyTime (T.DateYmd 2013 4 18) (Just $ T.ClockPartsMin 0 12 05)
--   Right 2013-04-18 12:05
--
--   >>> monthlyTime (T.DateYmd 2013 4 18) (Nothing :: Maybe T.ClockParts)
--   Right 2013-04-18
--
--   >>> monthlyTime (T.DateYmd 2013 4 31) (Nothing :: Maybe T.ClockParts)
--   Left AbortReason "Not monthly date" ...
--
monthlyTime :: (T.ToMjdClip day, T.ToClock clock) => day -> Maybe clock -> B.Ab Time
monthlyTime = timeOf T.monthly

-- | Create monthly clipped time.
monthlyClippedTime :: (T.ToMjdClip day, T.ToClock clock) => day -> Maybe clock -> Time
monthlyClippedTime = clippedTimeOf T.monthly

-- | Create monthly day-precision time.
monthlyTimeDate :: (T.ToMjdClip day) => day -> B.Ab Time
monthlyTimeDate day = timeOf T.monthly day (Nothing :: Maybe T.ClockParts)

-- | Create monthly clock-precision time.
monthlyTimeClock :: (T.ToMjdClip day, T.ToClock clock) => day -> clock -> B.Ab Time
monthlyTimeClock day clock = timeOf T.monthly day (Just clock)

timeOf :: (T.ToMjdClip day, T.ToClock clock) =>
          (T.Mjd -> T.Date) -> day -> Maybe clock -> B.Ab Time
timeOf ly day clock = do mjd <- T.toMjdAb day
                         Right $ createTime (ly mjd) clock

clippedTimeOf :: (T.ToMjdClip day, T.ToClock clock) =>
                 (T.Mjd -> T.Date) -> day -> Maybe clock -> Time
clippedTimeOf ly day = createTime (ly $ T.toMjdClip day)

createTime :: (T.ToClock clock) => T.Date -> Maybe clock -> Time
createTime date (Nothing) = TimeYmd  date
createTime date (Just c)  = TimeYmdc date (T.toClock c)

-- | Create time data from the Modified Julian day.
--
--   >>> timeFromMjd (55555 :: Int)
--   2010-12-25
--
timeFromMjd :: (T.ToMjd mjd) => mjd -> Time
timeFromMjd = TimeYmd . T.monthly

-- | Create time data from year and month.
timeFromYmAb :: T.Year -> T.Month -> B.Ab Time
timeFromYmAb y m =
    case Tim.fromGregorianValid y m 1 of
      Just day -> Right $ TimeYm day
      Nothing  -> Msg.notDate y m 1

-- | Create time data from year and week.
timeFromYwAb :: T.Year -> T.Week -> B.Ab Time
timeFromYwAb y w =
    case Tim.fromWeekDateValid y w 1 of
      Just day -> Right $ TimeYw day
      Nothing  -> Msg.notDate y w 1

-- | Create time data from date, clock, and time zone.
timeFromDczAb :: T.Date -> T.Clock -> Maybe Zone -> B.Ab Time
timeFromDczAb d c Nothing  = Right $ timeFromDc  d c
timeFromDczAb d c (Just z) = Right $ timeFromDcz d c z

timeFromDc :: T.Date -> T.Clock -> Time
timeFromDc d c = let (d', c') = T.clockDaysClock c
                 in TimeYmdc (T.dateAddDay d' d) c'

timeFromDcz :: T.Date -> T.Clock -> Zone -> Time
timeFromDcz d c z = let (d', c') = T.clockDaysClock c
                    in TimeYmdcz (T.dateAddDay d' d) c' z

-- | Create time from year, month, and day.
--
--   >>> timeFromYmd 2013 4 18
--   2013-04-18
--
--   >>> timeFromYmd 2013 4 0
--   2013-04-01
--
--   >>> timeFromYmd 2013 4 33
--   2013-04-30
--
timeFromYmd :: T.Year -> T.Month -> T.Day -> Time
timeFromYmd y m d = timeFromMjd $ Tim.fromGregorian y m d

-- | Craete time from tuple of year, month, and day.
--
--   >>> timeFromYmdTuple (2013, 4, 18)
--   2013-04-18
--
timeFromYmdTuple :: T.Ymd -> Time
timeFromYmdTuple = timeFromMjd . dayFromYmdTuple where
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
timeCutZone (TimeYmdcz d c _)    = timeFromDc d c
timeCutZone time                 = time

-- | Cut timezone and convert to local time,
--   i.e., addition of UTC and time zone.
timeLocalize :: O.Map Time
timeLocalize (TimeYmdcz d c z)   = timeFromDc d $ T.clockAddSec z c
timeLocalize time                = time

-- | Alter time zone part.
timeAltZone :: O.Map Zone -> O.Map Time
timeAltZone f (TimeYmdcz d c z)   = timeFromDcz d c $ f z
timeAltZone _ time                = time

-- | Cut clock part.
timeCutClock :: O.Map Time
timeCutClock (TimeYmdcz d _ _)    = TimeYmd d
timeCutClock (TimeYmdc  d _)      = TimeYmd d
timeCutClock time                 = time

-- | Set or add clock to time.
timeSetClock :: T.Clock -> O.Map Time
timeSetClock c (TimeYmdcz d _ z)  = TimeYmdcz d c z
timeSetClock c (TimeYmdc  d _)    = TimeYmdc  d c
timeSetClock c (TimeYmd   d)      = TimeYmdc  d c
timeSetClock _ time               = time

-- | Alter day part.
timeAltDate :: O.Map T.Date -> O.Map Time
timeAltDate f (TimeYmdcz d c z)  = TimeYmdcz (f d) c z
timeAltDate f (TimeYmdc  d c)    = TimeYmdc  (f d) c
timeAltDate f (TimeYmd   d)      = TimeYmd   (f d)
timeAltDate _ (TimeYw    d)      = TimeYw    d
timeAltDate _ (TimeYm    d)      = TimeYm    d

-- | Alter the Modified Julian Day of date part.
timeAltMjd :: O.Map T.Mjd -> O.Map Time
timeAltMjd f (TimeYmdcz d c z)   = TimeYmdcz (T.dateAltMjd f d) c z
timeAltMjd f (TimeYmdc  d c)     = TimeYmdc  (T.dateAltMjd f d) c
timeAltMjd f (TimeYmd   d)       = TimeYmd   (T.dateAltMjd f d)
timeAltMjd f (TimeYw    d)       = TimeYw    (f d)
timeAltMjd f (TimeYm    d)       = TimeYm    (f d)

-- | Alter integer content of the Modified Julian Day.
timeAltDays :: O.Map T.Days -> O.Map Time
timeAltDays f time = timeAltMjd g time where
    g (Tim.ModifiedJulianDay d) = Tim.ModifiedJulianDay $ f d

-- | Convert local time to Koshu time content.
timeFromZonedTime :: Tim.ZonedTime -> Time
timeFromZonedTime zt = time where
    Tim.UTCTime mjd s = Tim.zonedTimeToUTC zt
    d    = T.mjdDate $ Tim.toModifiedJulianDay mjd
    c    = T.clockFromDhms 0 0 0 $ floor s
    time = TimeYmdcz d c (60 * Tim.timeZoneMinutes (Tim.zonedTimeZone zt))

-- | Days and seconds of time.
--
--   >>> timeDaysSec $ timeFromMjd 55555
--   (55555,0)
--
timeDaysSec :: Time -> T.DaysSec
timeDaysSec (TimeYmdcz d c _)  = dateClockDaysSec d c
timeDaysSec (TimeYmdc d c)     = dateClockDaysSec d c
timeDaysSec (TimeYmd d)        = (T.mjdInteger $ T.toMjd d, 0)
timeDaysSec (TimeYw d)         = (T.mjdInteger d, 0)
timeDaysSec (TimeYm d)         = (T.mjdInteger d, 0)

dateClockDaysSec :: T.Date -> T.Clock -> T.DaysSec
dateClockDaysSec d c = let (d', s) = T.clockDaysSec c
                       in (T.mjdInteger (T.toMjd d) + d', s)
