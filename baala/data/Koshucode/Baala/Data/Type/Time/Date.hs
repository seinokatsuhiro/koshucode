{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

-- | Date using the Modified Julian Day (MJD).

module Koshucode.Baala.Data.Type.Time.Date
  ( -- * Modified Julian Day
    ToMjd (..),
    Mjd, mjdInteger,
    ToMjdClip (..),

    -- * Data type
    DateParts (..),
    Date (..), Ymd,
    Year, Month, Week, Day,

    -- * Creation
    dateFromMjd,
    dateFromYmd,
    dateFromYwd,
    dateFromYd, 

    -- * Conversion
    monthly, weekly, yearly,

    -- * Utility
    dateAltMjd, dateAdd,
    mix02,
  ) where

import qualified Data.Time.Calendar                as Tim
import qualified Data.Time.Calendar.WeekDate       as Tim
import qualified Data.Time.Calendar.OrdinalDate    as Tim
import qualified Koshucode.Baala.Overture          as O
import qualified Koshucode.Baala.Base              as B
import qualified Koshucode.Baala.Data.Type.Message as Msg


-- ----------------------  Modified Julian Day

-- | Convert to the Modified Julian Day.
class ToMjd a where
    toMjd :: a -> Mjd

instance ToMjd Tim.Day where
    toMjd = id

-- | Synonym for MJD type.
type Mjd = Tim.Day

-- | >>> toMjd (55555 :: Int)
--   2010-12-25
instance ToMjd Int where
    toMjd = Tim.ModifiedJulianDay . fromIntegral

-- | >>> toMjd (55555 :: Integer)
--   2010-12-25
instance ToMjd Integer where
    toMjd = Tim.ModifiedJulianDay

instance Num Tim.Day where
    x + y        = toMjd (mjdInteger x + mjdInteger y)
    x - y        = toMjd (mjdInteger x - mjdInteger y)
    x * y        = toMjd (mjdInteger x * mjdInteger y)
    abs          = toMjd . abs . mjdInteger
    signum       = toMjd . signum . mjdInteger
    fromInteger  = toMjd

-- | Extract MJD integer value.
mjdInteger :: Mjd -> Integer
mjdInteger = Tim.toModifiedJulianDay

-- | Convert to the Modified Just Day but abortable.
class ToMjdClip a where
    toMjdClip :: a -> Mjd

    toMjdAb :: a -> B.Ab Mjd
    toMjdAb = Right . toMjdClip

-- ----------------------  Type

-- | Decomposed date.
data DateParts
    = DateYmd Year Month Day  -- ^ Monthly date
    | DateYwd Year Week Day   -- ^ Weekly date
    | DateYd  Year Day        -- ^ Yearly date
    | DateMjd Mjd             -- ^ MJD date
      deriving (Show, Eq, Ord)

instance ToMjdClip DateParts where
    toMjdClip (DateYmd y m d)  = Tim.fromGregorian   y m d
    toMjdClip (DateYwd y w d)  = Tim.fromWeekDate    y w d
    toMjdClip (DateYd  y d)    = Tim.fromOrdinalDate y d
    toMjdClip (DateMjd mjd)    = mjd

    toMjdAb (DateYmd y m d)  = abMjd (Msg.notMonthlyDate y m d) $ Tim.fromGregorianValid   y m d
    toMjdAb (DateYwd y w d)  = abMjd (Msg.notWeeklyDate y w d)  $ Tim.fromWeekDateValid    y w d
    toMjdAb (DateYd  y d)    = abMjd (Msg.notYearlyDate y d)    $ Tim.fromOrdinalDateValid y d
    toMjdAb (DateMjd mjd)    = Right mjd

abMjd :: B.Ab Mjd -> Maybe Mjd -> B.Ab Mjd
abMjd _ (Just mjd) = Right mjd
abMjd a (Nothing)  = a

-- | Date.
data Date
    = Monthly Mjd    -- ^ Date in /YYYY-MM-DD/
    | Weekly  Mjd    -- ^ Date in /YYYY-#WW-D/
    | Yearly  Mjd    -- ^ Date in /YYYY-##D/

instance Eq Date where
    (==) = ordEq

-- | Test equality using 'Ord' method.
ordEq :: (Ord a) => a -> a -> Bool
ordEq a b = (a `compare` b) == EQ

instance Ord Date where
    a `compare` b = toMjd a `compare` toMjd b

instance Show Date where
    show d@(Monthly _) = "Date " ++ B.encode d
    show d = "Date " ++ B.encode d ++ " (" ++ B.encode (monthly d) ++ ")"

instance ToMjd Date where
    toMjd = dateMjd
    
-- | Get the internal Modified Julian Day.
dateMjd :: Date -> Mjd
dateMjd (Monthly day)  = day
dateMjd (Weekly  day)  = day
dateMjd (Yearly  day)  = day

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


-- ----------------------  Creation

-- | Create date from the Modified Julian Day.
--
--   >>> dateFromMjd (55555 :: Int)
--   Date 2010-12-25
--
dateFromMjd :: (ToMjd n) => n -> Date
dateFromMjd = Monthly . toMjd

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
monthly :: (ToMjd day) => day -> Date
monthly = Monthly . toMjd

-- | Convert date into weekly date.
--
--   >>> weekly $ dateFromMjd 55555
--   Date 2010-#51-6 (2010-12-25)
--
weekly :: (ToMjd day) => day -> Date
weekly  = Weekly . toMjd

-- | Convert date into yearly date.
--
--   >>> yearly $ dateFromMjd 55555
--   Date 2010-##359 (2010-12-25)
--
yearly :: (ToMjd day) => day -> Date
yearly  = Yearly . toMjd


-- ----------------------  Utility

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
