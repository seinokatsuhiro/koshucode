{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Data.Type.Time.Date
  ( -- * Data type
    Date (..), MJDay, YmdTuple,
    Year, Month, Week, Day,

    -- * Construction
    dateFromYmdAb, dateFromYwdAb, dateFromYdAb,

    -- * Utility
    dateDay, dateMapDay, dateAdd,
    monthly, weekly, yearly,
    mix02,
  ) where

import qualified Data.Time.Calendar                as T
import qualified Data.Time.Calendar.WeekDate       as T
import qualified Data.Time.Calendar.OrdinalDate    as T
import qualified Koshucode.Baala.Overture          as O
import qualified Koshucode.Baala.Base              as B
import qualified Koshucode.Baala.Data.Type.Message as Msg


-- ----------------------  Type

data Date
    = Monthly MJDay    -- ^ Date in /YYYY-MM-DD/
    | Weekly  MJDay    -- ^ Date in /YYYY-#W-D/
    | Yearly  MJDay    -- ^ Date in /YYYY-##D/
      deriving (Show)

instance Eq  Date where  a == b         = dateDay a == dateDay b
instance Ord Date where  a `compare` b  = dateDay a `compare` dateDay b

type MJDay    = T.Day     -- ^ The Modified Julian Day
type Year     = Integer
type Week     = Int
type Month    = Int
type Day      = Int

type YmdTuple = (Year, Month, Day)


-- ----------------------  Write

instance B.MixEncode Date where
    mixEncode = dateToMix

dateToMix :: Date -> B.MixText
dateToMix date =
    case date of
      Monthly d   -> dateMonth $ T.toGregorian    d
      Weekly  d   -> dateWeek  $ T.toWeekDate     d
      Yearly  d   -> dateYear  $ T.toOrdinalDate  d
    where
      dateMonth (y, m, d)  = B.mixDec y `hyMix`   mix02 m `hyMix` mix02 d
      dateWeek  (y, w, d)  = B.mixDec y `hywMix`  B.mixDec w   `hyMix` B.mixDec d
      dateYear  (y, d)     = B.mixDec y `hywwMix` B.mixDec d

mix02 :: Int -> B.MixText
mix02 = B.mixDecZero 2

hyMix, hywMix, hywwMix :: B.Bin B.MixText
hyMix    = B.mixInfix "-"
hywMix   = B.mixInfix "-#"
hywwMix  = B.mixInfix "-##"


-- ----------------------  Construction

-- | Create date from year, month, and day.
dateFromYmdAb :: Year -> Month -> Day -> B.Ab Date
dateFromYmdAb y m d =
    case T.fromGregorianValid y m d of
      Just day -> Right $ Monthly day
      Nothing  -> Msg.notDate y m d

-- | Create date from year, week, and day.
dateFromYwdAb :: Year -> Week -> Day -> B.Ab Date
dateFromYwdAb y w d =
    case T.fromWeekDateValid y w d of
      Just day -> Right $ Weekly day
      Nothing  -> Msg.notDate y w d

-- | Create date from year and day.
dateFromYdAb :: Year -> Day -> B.Ab Date
dateFromYdAb y d =
    case T.fromOrdinalDateValid y d of
      Just day -> Right $ Yearly day
      Nothing  -> Msg.notDate y 0 d


-- ----------------------  Utility

-- | Get the internal Modified Julian Day.
dateDay :: Date -> MJDay
dateDay (Monthly day)  = day
dateDay (Weekly  day)  = day
dateDay (Yearly  day)  = day

dateMapDay :: O.Map MJDay -> O.Map Date
dateMapDay f (Monthly day)  = Monthly $ f day
dateMapDay f (Weekly  day)  = Weekly  $ f day
dateMapDay f (Yearly  day)  = Yearly  $ f day

dateAdd :: (Integral n) => n -> O.Map Date
dateAdd d = dateMapDay $ T.addDays (fromIntegral d)

-- | Convert into monthly date.
monthly :: O.Map Date
monthly = Monthly . dateDay

-- | Convert into weekly date.
weekly :: O.Map Date
weekly  = Weekly . dateDay

-- | Convert into yearly date.
yearly :: O.Map Date
yearly  = Yearly . dateDay

