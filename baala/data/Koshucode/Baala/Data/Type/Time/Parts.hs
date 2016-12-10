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

    -- * Clock parts
    Days, Hour, Min, Sec,
    ClockParts (..),
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
