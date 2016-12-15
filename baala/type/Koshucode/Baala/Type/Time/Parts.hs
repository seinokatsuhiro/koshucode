{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

-- | Parts of clock and time.

module Koshucode.Baala.Type.Time.Parts
  ( -- * Modified Julian Day
    Mjd,
    mjdInteger,
    ToMjd (..),
    ToMjdClip (..),

    -- * Day of week
    Dayw (..),
    mjdDayw,
    daywDiff,

    -- * Date parts
    Year, Month, Week, Day,
    DateParts (..),
    dateYmd, dateYwd, dateYd, dateEom, dateYmnd,

    -- * Clock parts
    Days, Hour, Min, Sec,
    ClockParts (..),
    clockDhms, clockDhm, clockDh, clockD,
  ) where

import qualified Data.Time.Calendar                 as Tim
import qualified Data.Time.Calendar.WeekDate        as Tim
import qualified Data.Time.Calendar.OrdinalDate     as Tim
import qualified Koshucode.Baala.Base               as B
import qualified Koshucode.Baala.Type.Message       as Msg


-- ----------------------  Modified Julian Day

-- | Synonym for MJD type.
type Mjd = Tim.Day

-- | Extract MJD integer value.
mjdInteger :: Mjd -> Integer
mjdInteger = Tim.toModifiedJulianDay

-- | Convert to the Modified Julian Day.
class ToMjd a where
    toMjd :: a -> Mjd

instance ToMjd Tim.Day where
    toMjd = id

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

-- | Convert to the Modified Just Day but abortable.
class ToMjdClip a where
    toMjdClip :: a -> Mjd

    toMjdAb :: a -> B.Ab Mjd
    toMjdAb = Right . toMjdClip


-- ---------------------------------  Day of wekk

-- | Symbol of day of week.
data Dayw
    = Monday      -- ^ __1.__ Day of the moon.
    | Tuesday     -- ^ __2.__ Day of Mars
    | Wednesday   -- ^ __3.__ Day between Tuesday and Thursday.
    | Thursday    -- ^ __4.__ Day of thunder.
    | Friday      -- ^ __5.__ Day of Venus.
    | Saturday    -- ^ __6.__ Day of Saturn.
    | Sunday      -- ^ __7.__ Day of the sun.
      deriving (Show, Eq, Ord)

instance Enum Dayw where
    fromEnum Monday     = 1
    fromEnum Tuesday    = 2
    fromEnum Wednesday  = 3
    fromEnum Thursday   = 4
    fromEnum Friday     = 5
    fromEnum Saturday   = 6
    fromEnum Sunday     = 7

    toEnum 0 = Sunday
    toEnum 1 = Monday
    toEnum 2 = Tuesday
    toEnum 3 = Wednesday
    toEnum 4 = Thursday
    toEnum 5 = Friday
    toEnum 6 = Saturday
    toEnum 7 = Sunday
    toEnum n = toEnum $ mod n 7

-- | Day of week.
--
--   >>> mjdDayw (55555 :: Int)
--   Saturday
--
mjdDayw :: (ToMjd day) => day -> Dayw
mjdDayw day = let (_, _, d) = Tim.toWeekDate $ toMjd day
              in toEnum d

-- | Interval of weekday to next weekday.
--
--   >>> daywDiff Wednesday Saturday
--   3
--
--   >>> daywDiff Saturday Wednesday
--   4
--
daywDiff :: Dayw -> Dayw -> Int
daywDiff from to
    | from > to  = fromEnum to - fromEnum from + 7
    | otherwise  = fromEnum to - fromEnum from


-- ---------------------------------  Date

-- | Year type.
type Year = Integer

-- | Week type.
type Week = Int

-- | Month type.
type Month = Int

-- | Day type.
type Day = Int

-- | Decomposed date.
data DateParts
    = DateMjd  Mjd                  -- ^ MJD date
    | DateYmd  Year Month Day       -- ^ Monthly date
    | DateYwd  Year Week Dayw       -- ^ Weekly date
    | DateYd   Year Day             -- ^ Yearly date
    | DateEom  Year Month           -- ^ End of month
    | DateYmnd Year Month Int Dayw  -- ^ /N/\'th day of week in a month
      deriving (Show, Eq, Ord)

-- | Monthly date parts.
dateYmd :: Year -> Month -> Day -> DateParts
dateYmd = DateYmd

-- | Weekly date parts.
dateYwd :: Year -> Week -> Dayw -> DateParts
dateYwd = DateYwd

-- | Yearly date parts.
dateYd :: Year -> Day -> DateParts
dateYd = DateYd

-- | End of month.
dateEom :: Year -> Month -> DateParts
dateEom = DateEom

-- | /N/\'th day of week in a month
--
--   >>> toMjdClip $ dateYmnd 2013 4 3 Thursday
--   2013-04-18
--
dateYmnd :: Year -> Month -> Int -> Dayw -> DateParts
dateYmnd = DateYmnd

instance ToMjdClip DateParts where
    toMjdClip (DateMjd mjd)       = mjd
    toMjdClip (DateYmd  y m d)    = Tim.fromGregorian   y m d
    toMjdClip (DateYwd  y w d)    = Tim.fromWeekDate    y w (fromEnum d)
    toMjdClip (DateYd   y d)      = Tim.fromOrdinalDate y d
    toMjdClip (DateEom  y m)      = Tim.fromGregorian   y m 31
    toMjdClip (DateYmnd y m n d)  = toMjdClip $ monthlyYmnd y m n d

    toMjdAb (DateMjd   mjd)      = Right mjd
    toMjdAb (DateYmd   y m d)    = abMjd (Msg.notMonthlyDate y m d) $ Tim.fromGregorianValid   y m d
    toMjdAb (DateYwd   y w d)    = let d' = fromEnum d
                                   in abMjd (Msg.notWeeklyDate y w d') $ Tim.fromWeekDateValid y w d'
    toMjdAb (DateYd    y d)      = abMjd (Msg.notYearlyDate  y d)   $ Tim.fromOrdinalDateValid y d
    toMjdAb d@(DateEom _ _)      = Right $ toMjdClip d
    toMjdAb (DateYmnd  y m n d)  = toMjdAb $ monthlyYmnd y m n d

abMjd :: B.Ab Mjd -> Maybe Mjd -> B.Ab Mjd
abMjd _ (Just mjd) = Right mjd
abMjd a (Nothing)  = a

monthlyYmnd :: Year -> Month -> Int -> Dayw -> DateParts
monthlyYmnd y m n d = let diff = daywDiff (bomDayw y m) d
                      in DateYmd y m (diff + 1 + 7 * (n - 1))

-- | Day of week of the beginning of month.
bomDayw :: Year -> Month -> Dayw
bomDayw y m = mjdDayw $ Tim.fromGregorian y m 1


-- ---------------------------------  Clock

-- | Integer type for the Modified Julian Day.
type Days = Integer

-- | Hour type.
type Hour = Int

-- | Minute type.
type Min = Int

-- | Second type.
type Sec = Int

-- | Decomposed clock.
data ClockParts
    = ClockPartsSec  Days Hour Min Sec
    | ClockPartsMin  Days Hour Min
    | ClockPartsHour Days Hour
    | ClockPartsDays Days
      deriving (Show, Eq, Ord)

-- | Clock parts of days, hour, minute, and second.
clockDhms :: Days -> Hour -> Min -> Sec -> ClockParts
clockDhms = ClockPartsSec

-- | Clock parts of days, hour, and minute.
clockDhm :: Days -> Hour -> Min -> ClockParts
clockDhm = ClockPartsMin

-- | Clock parts of days and hour.
clockDh :: Days -> Hour -> ClockParts
clockDh = ClockPartsHour

-- | Clock parts of days only.
clockD :: Days -> ClockParts
clockD = ClockPartsDays
