{-# OPTIONS_GHC -Wall #-}

-- | Time

module Koshucode.Baala.Data.Type.Time.Time
  ( -- * Data type
    Time (..),
    timeMjd, timePrecision,
    timeYmdTuple,

    -- * Construction
    timeYmd, timeYmdc, timeFromMjd,
    timeFromYmAb, timeFromYwAb,
    timeFromDczAb,
    timeFromYmd, timeFromYmdTuple,

    -- * Construction with I/O
    today, now, nowZoned,

    -- * Conversion
    timeOmitClock, timeOmitZone,
    timeMapDate, timeMapMjd,
    timeFromZonedTime,
  ) where

import qualified Data.Time.Calendar                         as Tim
import qualified Data.Time.Calendar.WeekDate                as Tim
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
    = TimeYmdcz D.Date D.Clock D.Sec  -- ^ Date and clock with time zone
    | TimeYmdc  D.Date D.Clock        -- ^ Date and clock
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

timeYmdTuple :: Time -> D.YmdTuple
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
