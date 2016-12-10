{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

-- | Parts of clock and time.

module Koshucode.Baala.Data.Type.Time.Parts
  ( -- * Modified Julian Day
    Mjd,
    mjdInteger,
    ToMjd (..),
    ToMjdClip (..),

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
import qualified Koshucode.Baala.Data.Type.Message  as Msg


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
    = DateMjd  Mjd                 -- ^ MJD date
    | DateYmd  Year Month Day      -- ^ Monthly date
    | DateYwd  Year Week Day       -- ^ Weekly date
    | DateYd   Year Day            -- ^ Yearly date
    | DateEom  Year Month          -- ^ End of month
    | DateYmnd Year Month Int Day  -- ^ /N/\'th day of week in a month
      deriving (Show, Eq, Ord)

-- | Monthly date parts.
dateYmd :: Year -> Month -> Day -> DateParts
dateYmd = DateYmd

-- | Weekly date parts.
dateYwd :: Year -> Week -> Day -> DateParts
dateYwd = DateYwd

-- | Yearly date parts.
dateYd :: Year -> Day -> DateParts
dateYd = DateYd

-- | End of month.
dateEom :: Year -> Month -> DateParts
dateEom = DateEom

-- | /N/\'th day of week in a month
--
--   >>> toMjdClip (dateYmnd 2013 4 3 4)
--   2013-04-18
--
dateYmnd :: Year -> Month -> Int -> Day -> DateParts
dateYmnd = DateYmnd

instance ToMjdClip DateParts where
    toMjdClip (DateMjd mjd)       = mjd
    toMjdClip (DateYmd  y m d)    = Tim.fromGregorian   y m d
    toMjdClip (DateYwd  y w d)    = Tim.fromWeekDate    y w d
    toMjdClip (DateYd   y d)      = Tim.fromOrdinalDate y d
    toMjdClip (DateEom  y m)      = Tim.fromGregorian   y m 31
    toMjdClip (DateYmnd y m n d)  = toMjdClip $ monthlyYmnd y m n d

    toMjdAb (DateMjd   mjd)      = Right mjd
    toMjdAb (DateYmd   y m d)    = abMjd (Msg.notMonthlyDate y m d) $ Tim.fromGregorianValid   y m d
    toMjdAb (DateYwd   y w d)    = abMjd (Msg.notWeeklyDate  y w d) $ Tim.fromWeekDateValid    y w d
    toMjdAb (DateYd    y d)      = abMjd (Msg.notYearlyDate  y d)   $ Tim.fromOrdinalDateValid y d
    toMjdAb d@(DateEom _ _)      = Right $ toMjdClip d
    toMjdAb (DateYmnd  y m n d)  = toMjdAb $ monthlyYmnd y m n d

abMjd :: B.Ab Mjd -> Maybe Mjd -> B.Ab Mjd
abMjd _ (Just mjd) = Right mjd
abMjd a (Nothing)  = a

monthlyYmnd :: Year -> Month -> Int -> Day -> DateParts
monthlyYmnd y m n d = let diff = diffDayW (bomDayW y m) d
                      in DateYmd y m (diff + 1 + 7 * (n - 1))

-- | Day of week of the beginning of month.
bomDayW :: Year -> Month -> Day
bomDayW y m = d where
    mjd = Tim.fromGregorian y m 1
    (_, _, d) = Tim.toWeekDate mjd

-- | Interval of weekday to next weekday.
--
--   >>> diffDayW (fromEnum Wednesday) (fromEnum Saturday)
--   3
--
--   >>> diffDayW (fromEnum Saturday) (fromEnum Wednesday)
--   4
--
diffDayW :: Day -> Day -> Int
diffDayW from to
    | from > to  = to + 7 - from
    | otherwise  = to - from


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
