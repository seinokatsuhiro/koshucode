{-# OPTIONS_GHC -Wall #-}

-- | Date using the Modified Julian Day (MJD).

module Koshucode.Baala.Type.Time.Date
  ( -- * Data type
    Date (..),
    datePrecision,

    -- * MJD to Date
    mjdDate,
    mjdDateClip,
    mjdDateAb,

    -- * Creating Date
    ymdDate,
    ywdDate,
    ydDate,
    ywDate,
    ymDate,

    -- * Clipped date
    ymdDateClip,
    ywdDateClip,
    ydDateClip,
    ywDateClip,
    ymDateClip,

    -- * Conversion
    monthly, weekly, yearly,
    byWeek, byMonth,
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

-- | Date with format.
data Date
    = Monthly T.Mjd    -- ^ Date in /YYYY-MM-DD/
    | Weekly  T.Mjd    -- ^ Date in /YYYY-#WW-D/
    | Yearly  T.Mjd    -- ^ Date in /YYYY-##D/
    | ByWeek  T.Mjd    -- ^ Date in /YYYY-#WW/
    | ByMonth T.Mjd    -- ^ Date in /YYYY-MM/

instance Eq Date where
    (==) = O.ordEq

instance Ord Date where
    a `compare` b = dateMjd a `compare` dateMjd b

instance Show Date where
    show d@(Monthly _) = "Date " ++ B.encode d
    show d = "Date " ++ B.encode d ++ " (" ++ B.encode (monthly d) ++ ")"

instance T.ToMjd Date where
    toMjd = dateMjd

-- | Get the name of date precision.
datePrecision :: Date -> String
datePrecision (Monthly  _)  = "day"
datePrecision (Weekly   _)  = "day"
datePrecision (Yearly   _)  = "day"
datePrecision (ByWeek   _)  = "week"
datePrecision (ByMonth  _)  = "month"

-- | Get the internal Modified Julian Day.
dateMjd :: Date -> T.Mjd
dateMjd (Monthly mjd)  = mjd
dateMjd (Weekly  mjd)  = mjd
dateMjd (Yearly  mjd)  = mjd
dateMjd (ByWeek  mjd)  = mjd
dateMjd (ByMonth mjd)  = mjd


-- ----------------------  Encode

instance B.MixEncode Date where
    mixEncode = dateToMix

-- | Encode date.
dateToMix :: Date -> B.MixText
dateToMix date =
    case date of
      Monthly mjd  -> encodeMonthly $ Tim.toGregorian    mjd
      Weekly  mjd  -> encodeWeekly  $ Tim.toWeekDate     mjd
      Yearly  mjd  -> encodeYearly  $ Tim.toOrdinalDate  mjd
      ByWeek  mjd  -> encodeByWeek  $ Tim.toWeekDate     mjd
      ByMonth mjd  -> encodeByMonth $ Tim.toGregorian    mjd
    where
      encodeMonthly (y, m, d)  = B.mixDec y &-   mix02 m    &- mix02 d
      encodeWeekly  (y, w, d)  = B.mixDec y &-#  B.mixDec w &- B.mixDec d
      encodeYearly  (y, d)     = B.mixDec y &-## B.mixDec d
      encodeByWeek  (y, w, _)  = B.mixDec y &-#  B.mixDec w
      encodeByMonth (y, m, _)  = B.mixDec y &-   mix02 m

      (&-)   = B.mixInfix "-"
      (&-#)  = B.mixInfix "-#"
      (&-##) = B.mixInfix "-##"

-- | Create mix text with two-width zeros.
--
--   >>> mix02 5
--   MixText "05"
--
mix02 :: Int -> B.MixText
mix02 = B.mixDecZero 2


-- ----------------------  MJD

-- | Create date from the Modified Julian Day.
--
--   >>> mjdDate (55555 :: Int)
--   Date 2010-12-25
--
mjdDate :: (T.ToMjd n) => n -> Date
mjdDate = Monthly . T.toMjd

-- | Create date from the Modified Julian Day.
--
--   >>> mjdDateClip $ T.ymd 2013 4 31
--   Date 2013-04-30
--
mjdDateClip :: (T.ToMjdClip n) => n -> Date
mjdDateClip = Monthly . T.toMjdClip

-- | Create date from the Modified Julian Day.
--
--   >>> mjdDateAb $ T.ymd 2013 4 18
--   Right Date 2013-04-18
--
mjdDateAb :: (T.ToMjdClip n) => n -> B.Ab Date
mjdDateAb = fmap Monthly . T.toMjdAb


-- ----------------------  Creation

-- | Create date from year, month, and day.
--
--   >>> ymdDate 2013 4 18
--   Right Date 2013-04-18
--
--   >>> ymdDate 2013 4 31
--   Left ...
--
ymdDate :: T.Year -> T.Month -> T.Day -> B.Ab Date
ymdDate y m d =
    case Tim.fromGregorianValid y m d of
      Just mjd -> Right $ Monthly mjd
      Nothing  -> Msg.notDate y m d

-- | Create date from year, week, and day.
--   Day 1 for Monday, 2 for Tuesday, ..., and 7 for Sunday.
--
--   >>> ywdDate 2013 16 4
--   Right Date 2013-#16-4 (2013-04-18)
--
ywdDate :: T.Year -> T.Week -> T.Day -> B.Ab Date
ywdDate y w d =
    case Tim.fromWeekDateValid y w d of
      Just mjd -> Right $ Weekly mjd
      Nothing  -> Msg.notDate y w d

-- | Create date from year and day.
--
--   >>> ydDate 2013 108
--   Right Date 2013-##108 (2013-04-18)
--
ydDate :: T.Year -> T.Day -> B.Ab Date
ydDate y d =
    case Tim.fromOrdinalDateValid y d of
      Just mjd -> Right $ Yearly mjd
      Nothing  -> Msg.notDate y 0 d

-- | Create date from year and week.
--
--   >>> ywDate 2013 16
--   Right Date 2013-#16 (2013-04-15)
--
ywDate :: T.Year -> T.Week -> B.Ab Date
ywDate y w =
    case Tim.fromWeekDateValid y w 1 of
      Just mjd -> Right $ ByWeek mjd
      Nothing  -> Msg.notDate y w 1

-- | Create date from year and month.
--
--   >>> ymDate 2013 4
--   Right Date 2013-04 (2013-04-01)
--
ymDate :: T.Year -> T.Month -> B.Ab Date
ymDate y m =
    case Tim.fromGregorianValid y m 1 of
      Just mjd -> Right $ ByMonth mjd
      Nothing  -> Msg.notDate y m 1


-- ----------------------  Clipped date

-- | Create date from year, month, and day.
--
--   >>> ymdDateClip 2013 4 18
--   Date 2013-04-18
--
ymdDateClip :: T.Year -> T.Month -> T.Day -> Date
ymdDateClip y m d = Monthly $ Tim.fromGregorian y m d

-- | Create date from year, week, and day.
--
--   >>> ywdDateClip 2013 16 10
--   Date 2013-04-21
--
ywdDateClip :: T.Year -> T.Week -> T.Day -> Date
ywdDateClip y w d = Monthly $ Tim.fromWeekDate y w d

-- | Create date from year and day.
--
--   >>> ydDateClip 2013 400
--   Date 2013-##365 (2013-12-31)
--
ydDateClip :: T.Year -> T.Day -> Date
ydDateClip y d = Yearly $ Tim.fromOrdinalDate y d

-- | Create date from year and week.
--
--   >>> ywDateClip 2013 60
--   Date 2013-#52 (2013-12-23)
--
ywDateClip :: T.Year -> T.Week -> Date
ywDateClip y w = ByWeek $ Tim.fromWeekDate y w 1

-- | Create date from year and month.
--
--   >>> ymDateClip 2013 13
--   Date 2013-12 (2013-12-01)
--
ymDateClip :: T.Year -> T.Month -> Date
ymDateClip y m = ByMonth $ Tim.fromGregorian y m 1


-- ----------------------  Conversion

-- | Convert date into monthly date.
--
--   >>> monthly $ mjdDate (55555 :: Int)
--   Date 2010-12-25
--
monthly :: (T.ToMjd day) => day -> Date
monthly = Monthly . T.toMjd

-- | Convert date into weekly date.
--
--   >>> weekly $ mjdDate (55555 :: Int)
--   Date 2010-#51-6 (2010-12-25)
--
weekly :: (T.ToMjd day) => day -> Date
weekly  = Weekly . T.toMjd

-- | Convert date into yearly date.
--
--   >>> yearly $ mjdDate (55555 :: Int)
--   Date 2010-##359 (2010-12-25)
--
yearly :: (T.ToMjd day) => day -> Date
yearly  = Yearly . T.toMjd

-- | Convert date into by-week date.
--
--   >>> byWeek $ mjdDate (55555 :: Int)
--   Date 2010-#51 (2010-12-20)
--
byWeek :: (T.ToMjd day) => day -> Date
byWeek = week . Tim.toWeekDate . T.toMjd where
    week (y, w, _) = ywDateClip y w

-- | Convert date into by-month date.
--
--   >>> byMonth $ mjdDate (55555 :: Int)
--   Date 2010-12 (2010-12-01)
--
byMonth :: (T.ToMjd day) => day -> Date
byMonth = month . Tim.toGregorian . T.toMjd where
    month (y, m, _) = ymDateClip y m

-- | Alter the Modified Julian Day of date.
dateAltMjd :: O.Map T.Mjd -> O.Map Date
dateAltMjd f (Monthly mjd)  = Monthly $ f mjd
dateAltMjd f (Weekly  mjd)  = Weekly  $ f mjd
dateAltMjd f (Yearly  mjd)  = Yearly  $ f mjd
dateAltMjd f (ByWeek  mjd)  = ByWeek  $ f mjd
dateAltMjd f (ByMonth mjd)  = ByMonth $ f mjd

-- | Add days.
--
--   >>> dateAddDay 7 $ mjdDate (55555 :: Int)
--   Date 2011-01-01
--
dateAddDay :: (Integral n) => n -> O.Map Date
dateAddDay = dateAltMjd . Tim.addDays . fromIntegral
