{-# OPTIONS_GHC -Wall #-}

-- | Date using the Modified Julian Day (MJD).

module Koshucode.Baala.Type.Time.Date
  ( -- * Data type
    Date (..), Ymd,

    -- * Creation
    mjdDate,
    monthlyDate,
    weeklyDate,
    yearlyDate, 

    -- * Conversion
    monthly, weekly, yearly,
    dateAltMjd, dateAddDay,
    mix02,
  ) where

import qualified Data.Time.Calendar                    as Tim
import qualified Data.Time.Calendar.WeekDate           as Tim
import qualified Data.Time.Calendar.OrdinalDate        as Tim
import qualified Koshucode.Baala.Overture              as O
import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Type.Time.Parts       as T
import qualified Koshucode.Baala.Type.Message          as Msg


-- ----------------------  Type

-- | Date.
data Date
    = Monthly T.Mjd    -- ^ Date in /YYYY-MM-DD/
    | Weekly  T.Mjd    -- ^ Date in /YYYY-#WW-D/
    | Yearly  T.Mjd    -- ^ Date in /YYYY-##D/

instance Eq Date where
    (==) = O.ordEq

instance Ord Date where
    a `compare` b = T.toMjd a `compare` T.toMjd b

instance Show Date where
    show d@(Monthly _) = "Date " ++ B.encode d
    show d = "Date " ++ B.encode d ++ " (" ++ B.encode (monthly d) ++ ")"

instance T.ToMjd Date where
    toMjd = dateMjd
    
-- | Get the internal Modified Julian Day.
dateMjd :: Date -> T.Mjd
dateMjd (Monthly day)  = day
dateMjd (Weekly  day)  = day
dateMjd (Yearly  day)  = day

-- | Type for year, month, and day.
type Ymd = (T.Year, T.Month, T.Day)


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


-- ----------------------  Creation

-- | Create date from the Modified Julian Day.
--
--   >>> mjdDate (55555 :: Int)
--   Date 2010-12-25
--
mjdDate :: (T.ToMjd n) => n -> Date
mjdDate = Monthly . T.toMjd

-- | Create date from year, month, and day.
--
--   >>> monthlyDate 2013 4 18
--   Right Date 2013-04-18
--
--   >>> monthlyDate 2013 4 31
--   Left ...
--
monthlyDate :: T.Year -> T.Month -> T.Day -> B.Ab Date
monthlyDate y m d =
    case Tim.fromGregorianValid y m d of
      Just day -> Right $ Monthly day
      Nothing  -> Msg.notDate y m d

-- | Create date from year, week, and day.
--   Day 1 for Monday, 2 for Tuesday, ..., and 7 for Sunday.
--
--   >>> weeklyDate 2013 16 4
--   Right Date 2013-#16-4 (2013-04-18)
--
weeklyDate :: T.Year -> T.Week -> T.Day -> B.Ab Date
weeklyDate y w d =
    case Tim.fromWeekDateValid y w d of
      Just day -> Right $ Weekly day
      Nothing  -> Msg.notDate y w d

-- | Create date from year and day.
--
--   >>> yearlyDate 2013 108
--   Right Date 2013-##108 (2013-04-18)
--
yearlyDate :: T.Year -> T.Day -> B.Ab Date
yearlyDate y d =
    case Tim.fromOrdinalDateValid y d of
      Just day -> Right $ Yearly day
      Nothing  -> Msg.notDate y 0 d


-- ----------------------  Conversion

-- | Convert date into monthly date.
--
--   >>> monthly $ mjdDate 55555
--   Date 2010-12-25
--
monthly :: (T.ToMjd day) => day -> Date
monthly = Monthly . T.toMjd

-- | Convert date into weekly date.
--
--   >>> weekly $ mjdDate 55555
--   Date 2010-#51-6 (2010-12-25)
--
weekly :: (T.ToMjd day) => day -> Date
weekly  = Weekly . T.toMjd

-- | Convert date into yearly date.
--
--   >>> yearly $ mjdDate 55555
--   Date 2010-##359 (2010-12-25)
--
yearly :: (T.ToMjd day) => day -> Date
yearly  = Yearly . T.toMjd

-- | Alter the Modified Julian Day of date.
dateAltMjd :: O.Map T.Mjd -> O.Map Date
dateAltMjd f (Monthly day)  = Monthly $ f day
dateAltMjd f (Weekly  day)  = Weekly  $ f day
dateAltMjd f (Yearly  day)  = Yearly  $ f day

-- | Add days.
--
--   >>> dateAddDay 7 $ mjdDate 55555
--   Date 2011-01-01
--
dateAddDay :: (Integral n) => n -> O.Map Date
dateAddDay = dateAltMjd . Tim.addDays . fromIntegral
