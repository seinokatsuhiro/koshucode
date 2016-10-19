{-# OPTIONS_GHC -Wall #-}

-- | Date.

module Koshucode.Baala.Data.Type.Time.Date
  ( -- * Data type
    Date (..), MJDay, YmdTuple,
    Year, Month, Week, Day,
    -- * Construction
    dateFromYmdAb, dateFromYwdAb,
    dateFromYdAb, dateFromMjd,
    -- * Form conversion
    monthly, weekly, yearly,
    -- * Utility
    dateDay, dateMapDay, dateAdd,
    mix02,
  ) where

import qualified Data.Time.Calendar                as Tim
import qualified Data.Time.Calendar.WeekDate       as Tim
import qualified Data.Time.Calendar.OrdinalDate    as Tim
import qualified Koshucode.Baala.Overture          as O
import qualified Koshucode.Baala.Base              as B
import qualified Koshucode.Baala.Data.Type.Message as Msg


-- ----------------------  Type

-- | Date.
data Date
    = Monthly MJDay    -- ^ Date in /YYYY-MM-DD/
    | Weekly  MJDay    -- ^ Date in /YYYY-#W-D/
    | Yearly  MJDay    -- ^ Date in /YYYY-##D/

instance Eq  Date where
    a == b = dateDay a == dateDay b

instance Ord Date where
    a `compare` b = dateDay a `compare` dateDay b

instance Show Date where
    show d = "Date " ++ B.mixToFlatString (dateToMix d)

-- | Type for the Modified Julian Day.
type MJDay = Tim.Day

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

-- | Create date from year, month, and day.
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

-- | Create date from the Modified Julian Day.
dateFromMjd :: Integer -> Date
dateFromMjd = Monthly . Tim.ModifiedJulianDay


-- ----------------------  Form conversion

-- | Convert into monthly date.
--
--   >>> monthly $ dateFromMjd 55555
--   Date 2010-12-25
--
monthly :: O.Map Date
monthly = Monthly . dateDay

-- | Convert into weekly date.
--
--   >>> weekly $ dateFromMjd 55555
--   Date 2010-#51-6
--
weekly :: O.Map Date
weekly  = Weekly . dateDay

-- | Convert into yearly date.
--
--   >>> yearly $ dateFromMjd 55555
--   Date 2010-##359
--
yearly :: O.Map Date
yearly  = Yearly . dateDay


-- ----------------------  Utility

-- | Get the internal Modified Julian Day.
dateDay :: Date -> MJDay
dateDay (Monthly day)  = day
dateDay (Weekly  day)  = day
dateDay (Yearly  day)  = day

-- | Map the content of date.
dateMapDay :: O.Map MJDay -> O.Map Date
dateMapDay f (Monthly day)  = Monthly $ f day
dateMapDay f (Weekly  day)  = Weekly  $ f day
dateMapDay f (Yearly  day)  = Yearly  $ f day

-- | Add days.
--
--   >>> dateAdd 7 $ dateFromMjd 55555
--   Date 2011-01-01
--
dateAdd :: (Integral n) => n -> O.Map Date
dateAdd = dateMapDay . Tim.addDays . fromIntegral
