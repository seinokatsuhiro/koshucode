{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

-- | Date.

module Koshucode.Baala.Data.Type.Time.Date
  ( -- * Modified Julian Day
    Mjd, mjd, unMjd,

    -- * Data type
    Date (..), Ymd,
    Year, Month, Week, Day,

    -- * Construction
    dateFromMjd,
    dateFromYmd,
    dateFromYwd,
    dateFromYd, 

    -- * Conversion
    monthly, weekly, yearly,

    -- * Utility
    dateMjd, dateAltMjd, dateAdd,
    mix02,
  ) where

import qualified Data.Time.Calendar                as Tim
import qualified Data.Time.Calendar.WeekDate       as Tim
import qualified Data.Time.Calendar.OrdinalDate    as Tim
import qualified Koshucode.Baala.Overture          as O
import qualified Koshucode.Baala.Base              as B
import qualified Koshucode.Baala.Data.Type.Message as Msg


-- ----------------------  Modified Julian Day

-- | Type for the Modified Julian Day.
type Mjd = Tim.Day

-- | Create MJD.
mjd :: Integer -> Mjd
mjd = Tim.ModifiedJulianDay

-- | Extract MJD integer value.
unMjd :: Mjd -> Integer
unMjd = Tim.toModifiedJulianDay

instance Num Tim.Day where
    x + y        = mjd (unMjd x + unMjd y)
    x - y        = mjd (unMjd x - unMjd y)
    x * y        = mjd (unMjd x * unMjd y)
    abs          = mjd . abs . unMjd
    signum       = mjd . signum . unMjd
    fromInteger  = mjd


-- ----------------------  Type

-- | Date.
data Date
    = Monthly Mjd    -- ^ Date in /YYYY-MM-DD/
    | Weekly  Mjd    -- ^ Date in /YYYY-#W-D/
    | Yearly  Mjd    -- ^ Date in /YYYY-##D/

instance Eq  Date where
    a == b = dateMjd a == dateMjd b

instance Ord Date where
    a `compare` b = dateMjd a `compare` dateMjd b

instance Show Date where
    show d@(Monthly _) = "Date " ++ B.encode d
    show d = "Date " ++ B.encode d ++ " (" ++ B.encode (monthly d) ++ ")"

-- | Type for year, month, and day.
type Ymd = (Year, Month, Day)

-- | Year type.
type Year = Integer

-- | Week type.
type Week = Int

-- | Month type.
type Month = Int

-- | Day type.
type Day = Int


-- ----------------------  Encode

instance B.MixEncode Date where
    mixEncode = dateToMix

-- | Encode date.
dateToMix :: Date -> B.MixText
dateToMix date =
    case date of
      Monthly d   -> dateMonth $ Tim.toGregorian    d
      Weekly  d   -> dateWeek  $ Tim.toWeekDate     d
      Yearly  d   -> dateYear  $ Tim.toOrdinalDate  d
    where
      dateMonth (y, m, d)  = B.mixDec y `hy`    mix02 m    `hy` mix02 d
      dateWeek  (y, w, d)  = B.mixDec y `hyw`   B.mixDec w `hy` B.mixDec d
      dateYear  (y, d)     = B.mixDec y `hyord` B.mixDec d

      hy    = B.mixInfix "-"
      hyw   = B.mixInfix "-#"
      hyord = B.mixInfix "-##"

-- | Create mix text with two-width zeros.
--
--   >>> mix02 5
--   MixText "05"
--
mix02 :: Int -> B.MixText
mix02 = B.mixDecZero 2


-- ----------------------  Construction

-- | Create date from the Modified Julian Day.
--
--   >>> dateFromMjd 55555
--   Date 2010-12-25
--
dateFromMjd :: Integer -> Date
dateFromMjd = Monthly . Tim.ModifiedJulianDay

-- | Create date from year, month, and day.
--
--   >>> dateFromYmd 2013 4 18
--   Right Date 2013-04-18
--
--   >>> dateFromYmd 2013 4 31
--   Left ...
--
dateFromYmd :: Year -> Month -> Day -> B.Ab Date
dateFromYmd y m d =
    case Tim.fromGregorianValid y m d of
      Just day -> Right $ Monthly day
      Nothing  -> Msg.notDate y m d

-- | Create date from year, week, and day.
--   Day 1 for Monday, 2 for Tuesday, ..., and 7 for Sunday.
--
--   >>> dateFromYwd 2013 16 4
--   Right Date 2013-#16-4 (2013-04-18)
--
dateFromYwd :: Year -> Week -> Day -> B.Ab Date
dateFromYwd y w d =
    case Tim.fromWeekDateValid y w d of
      Just day -> Right $ Weekly day
      Nothing  -> Msg.notDate y w d

-- | Create date from year and day.
--
--   >>> dateFromYd 2013 108
--   Right Date 2013-##108 (2013-04-18)
--
dateFromYd :: Year -> Day -> B.Ab Date
dateFromYd y d =
    case Tim.fromOrdinalDateValid y d of
      Just day -> Right $ Yearly day
      Nothing  -> Msg.notDate y 0 d


-- ----------------------  Form conversion

-- | Convert date into monthly date.
--
--   >>> monthly $ dateFromMjd 55555
--   Date 2010-12-25
--
monthly :: O.Map Date
monthly = Monthly . dateMjd

-- | Convert date into weekly date.
--
--   >>> weekly $ dateFromMjd 55555
--   Date 2010-#51-6 (2010-12-25)
--
weekly :: O.Map Date
weekly  = Weekly . dateMjd

-- | Convert date into yearly date.
--
--   >>> yearly $ dateFromMjd 55555
--   Date 2010-##359 (2010-12-25)
--
yearly :: O.Map Date
yearly  = Yearly . dateMjd


-- ----------------------  Utility

-- | Get the internal Modified Julian Day.
dateMjd :: Date -> Mjd
dateMjd (Monthly day)  = day
dateMjd (Weekly  day)  = day
dateMjd (Yearly  day)  = day

-- | Alter the Modified Julian Day of date.
dateAltMjd :: O.Map Mjd -> O.Map Date
dateAltMjd f (Monthly day)  = Monthly $ f day
dateAltMjd f (Weekly  day)  = Weekly  $ f day
dateAltMjd f (Yearly  day)  = Yearly  $ f day

-- | Add days.
--
--   >>> dateAdd 7 $ dateFromMjd 55555
--   Date 2011-01-01
--
dateAdd :: (Integral n) => n -> O.Map Date
dateAdd = dateAltMjd . Tim.addDays . fromIntegral
