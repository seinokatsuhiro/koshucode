{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

-- | Date.

module Koshucode.Baala.Data.Type.Time.Date
  ( -- * Modified Julian Day
    Mjd, mjd, unMjd,

    -- * Data type
    Date (..), YmdTuple,
    Year, Month, Week, Day,

    -- * Construction
    dateFromMjd,
    dateFromYmdAb,
    dateFromYwdAb,
    dateFromYdAb, 

    -- * Conversion
    monthly, weekly, yearly,

    -- * Utility
    dateMjd, dateMap, dateAdd,
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
mjd :: Integer -> Tim.Day
mjd = Tim.ModifiedJulianDay

-- | Extract MJD integer value.
unMjd :: Tim.Day -> Integer
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
    show d = "Date " ++ B.mixToFlatString (dateToMix d)

-- | Year type.
type Year = Integer

-- | Week type.
type Week = Int

-- | Month type.
type Month = Int

-- | Day type.
type Day = Int

-- | Type for year, month, and day.
type YmdTuple = (Year, Month, Day)


-- ----------------------  Write

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
--   >>> dateFromYmdAb 2013 4 18
--   Right Date 2013-04-18
--
--   >>> dateFromYmdAb 2013 4 31
--   Left ...
--
dateFromYmdAb :: Year -> Month -> Day -> B.Ab Date
dateFromYmdAb y m d =
    case Tim.fromGregorianValid y m d of
      Just day -> Right $ Monthly day
      Nothing  -> Msg.notDate y m d

-- | Create date from year, week, and day.
dateFromYwdAb :: Year -> Week -> Day -> B.Ab Date
dateFromYwdAb y w d =
    case Tim.fromWeekDateValid y w d of
      Just day -> Right $ Weekly day
      Nothing  -> Msg.notDate y w d

-- | Create date from year and day.
dateFromYdAb :: Year -> Day -> B.Ab Date
dateFromYdAb y d =
    case Tim.fromOrdinalDateValid y d of
      Just day -> Right $ Yearly day
      Nothing  -> Msg.notDate y 0 d


-- ----------------------  Form conversion

-- | Convert into monthly date.
--
--   >>> monthly $ dateFromMjd 55555
--   Date 2010-12-25
--
monthly :: O.Map Date
monthly = Monthly . dateMjd

-- | Convert into weekly date.
--
--   >>> weekly $ dateFromMjd 55555
--   Date 2010-#51-6
--
weekly :: O.Map Date
weekly  = Weekly . dateMjd

-- | Convert into yearly date.
--
--   >>> yearly $ dateFromMjd 55555
--   Date 2010-##359
--
yearly :: O.Map Date
yearly  = Yearly . dateMjd


-- ----------------------  Utility

-- | Get the internal Modified Julian Day.
dateMjd :: Date -> Mjd
dateMjd (Monthly day)  = day
dateMjd (Weekly  day)  = day
dateMjd (Yearly  day)  = day

-- | Map the content of date.
dateMap :: O.Map Mjd -> O.Map Date
dateMap f (Monthly day)  = Monthly $ f day
dateMap f (Weekly  day)  = Weekly  $ f day
dateMap f (Yearly  day)  = Yearly  $ f day

-- | Add days.
--
--   >>> dateAdd 7 $ dateFromMjd 55555
--   Date 2011-01-01
--
dateAdd :: (Integral n) => n -> O.Map Date
dateAdd = dateMap . Tim.addDays . fromIntegral
