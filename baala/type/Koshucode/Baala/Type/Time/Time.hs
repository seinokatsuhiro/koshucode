{-# OPTIONS_GHC -Wall #-}

-- | Time.

module Koshucode.Baala.Type.Time.Time
  ( -- * Time type
    Time (..), Zone,
    timePrecision,

    -- * Date-part time
    mjdTime, mjdTimeClip, mjdTimeAb,
    dTime,

    -- * Date-and-clock time
    dcTime,
    dczTime,

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
    def = mjdTime (0 :: Int)

-- | Extract date part and convert to MJT.
instance T.ToMjd Time where
    toMjd (TimeYmdcz d _ _)   = T.toMjd d
    toMjd (TimeYmdc  d _)     = T.toMjd d
    toMjd (TimeYmd   d)       = T.toMjd d
    toMjd (TimeYw    mjd)     = mjd
    toMjd (TimeYm    mjd)     = mjd

-- | Get the name of time precision.
timePrecision :: Time -> String
timePrecision (TimeYmdcz _ c _)  = T.clockPrecision c ++ " zone"
timePrecision (TimeYmdc  _ c)    = T.clockPrecision c
timePrecision (TimeYmd   d)      = T.datePrecision d
timePrecision (TimeYw    _)      = "week"
timePrecision (TimeYm    _)      = "month"


-- ----------------------  Encode

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


-- ----------------------  Date

-- | Create time data from the Modified Julian day.
--
--   >>> mjdTime (55555 :: Int)
--   2010-12-25
--
mjdTime :: (T.ToMjd mjd) => mjd -> Time
mjdTime = TimeYmd . T.monthly

-- | Craete time from tuple of year, month, and day.
--
--   >>> mjdTimeClip $ T.ymd 2013 4 18
--   2013-04-18
--
mjdTimeClip :: (T.ToMjdClip mjd) => mjd -> Time
mjdTimeClip = mjdTime . T.toMjdClip

-- | Create monthly day-precision time.
--
--   >>> mjdTimeAb $ T.ymd 2013 4 18
--   Right 2013-04-18
--
--   >>> mjdTimeAb $ T.ymd 2013 4 31
--   Left ...
--
mjdTimeAb :: (T.ToMjdClip day) => day -> B.Ab Time
mjdTimeAb day = do mjd <- T.toMjdAb day
                   Right $ mjdTime mjd

-- | Create time without clock.
--
--   >>> dTime $ T.mjdDate (55555 :: Int)
--   2010-12-25
--
--   >>> dTime $ T.ymdDateClip 2013 4 31
--   2013-04-30
--
--   >>> dTime <$> T.ymdDate 2013 4 18
--   Right 2013-04-18
--
dTime :: T.Date -> Time
dTime = TimeYmd


-- ----------------------  Date and clock

-- | Create time with clock.
--
--   >>> dcTime (T.mjdDate (55555 :: Int)) (T.ClockPartsMin 0 10 40)
--   2010-12-25 10 40
--
--   >>> dcTime (T.mjdDate (55555 :: Int)) (T.ClockPartsMin 1 10 40)
--   2010-12-26 10:40
--
dcTime :: (T.ToClock clock) => T.Date -> clock -> Time
dcTime d c = dcTime2 d $ T.toClock c

dcTime2 :: T.Date -> T.Clock -> Time
dcTime2 d c = let (d', c') = T.clockDaysClock c
              in TimeYmdc (T.dateAddDay d' d) c'

-- | Create time from date, clock, and time zone.
dczTime :: T.Date -> T.Clock -> Zone -> Time
dczTime d c z = let (d', c') = T.clockDaysClock c
                 in TimeYmdcz (T.dateAddDay d' d) c' z


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
timeCutZone (TimeYmdcz d c _)    = dcTime2 d c
timeCutZone time                 = time

-- | Cut timezone and convert to local time,
--   i.e., addition of UTC and time zone.
timeLocalize :: O.Map Time
timeLocalize (TimeYmdcz d c z)   = dcTime2 d $ T.clockAddSec z c
timeLocalize time                = time

-- | Alter time zone part.
timeAltZone :: O.Map Zone -> O.Map Time
timeAltZone f (TimeYmdcz d c z)   = dczTime d c $ f z
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
--   >>> timeDaysSec $ mjdTime 55555
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
