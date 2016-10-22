{-# OPTIONS_GHC -Wall #-}

-- | Clock: distance between two times.

module Koshucode.Baala.Data.Type.Time.Clock
  ( -- * Data type
    Clock (..), DayCount, Hour, Min, Sec,
    clockBodyToMix,

    -- * Accessor
    clockFromDhms, clockFromDhm, clockFromDh, clockFromD,
    clockFromHms,
    dhmsFromSec, secFromHms,

    -- * Property
    clockSign, clockDayCount, clockSec,
    clockPrecision,
    clockDhms, clockAlter,

    -- * Conversion
    clockMap,
    clockMap2,
    daySeconds,
  ) where

import qualified Koshucode.Baala.Overture                as O
import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Data.Type.Time.Date     as D
import qualified Koshucode.Baala.Base.Message            as Msg


-- ----------------------  Data type

-- | Clock as distance between two times.
data Clock
    = ClockDhms DayCount Sec    -- ^ Clock represented by multiple of second
    | ClockDhm  DayCount Sec    -- ^ Clock represented by multiple of minute
    | ClockDh   DayCount Sec    -- ^ Clock represented by multiple of hour
    | ClockD    DayCount        -- ^ Clock represented by multiple of day
      deriving (Eq, Ord)

-- | Integer type for the Modified Julian Day.
type DayCount = Integer

-- | Hour type.
type Hour = Int

-- | Minute type.
type Min = Int

-- | Second type.
type Sec = Int

-- | Create clock from MJD, hour, minute, and second.
--
--   >>> clockFromDhms 1 9 40 20
--   |1'09:40:20|
--
clockFromDhms :: DayCount -> Hour -> Min -> Sec -> Clock
clockFromDhms d h m s = ClockDhms d $ secFromHms (h, m, s)

-- | Create clock from MJD, hour, and minute.
clockFromDhm :: DayCount -> Hour -> Min -> Clock
clockFromDhm d h m = ClockDhm d $ secFromHms (h, m, 0)

-- | Create clock from MJD and hour.
clockFromDh :: DayCount -> Hour -> Clock
clockFromDh d h = ClockDh d $ secFromHms (h, 0, 0)

-- | Create clock from MJD.
clockFromD :: DayCount -> Clock
clockFromD = ClockD

-- | Create clock from hour, minute, and optional second.
--
--   >>> clockFromHms 9 40 Nothing
--   |09:40|
--
--   >>> clockFromHms 9 40 (Just 20)
--   |09:40:20|
--
clockFromHms :: Hour -> Min -> Maybe Sec -> Clock
clockFromHms h m (Nothing)  = clockFromDhm  0 h m
clockFromHms h m (Just s)   = clockFromDhms 0 h m s

-- | Aggregate hour, minute, and second into single second.
secFromHms :: (Hour, Min, Sec) -> Sec
secFromHms (h, m, s) = s + 60 * (m + 60 * h)

-- | Decompose second into MJD, hour, minute and second parts.
--
--   >>> dhmsFromSec 333333
--   (3,9,15,33)
--
dhmsFromSec :: Sec -> (DayCount, Hour, Min, Sec)
dhmsFromSec sec =
    let (m', s)   = sec `quotRem` 60
        (h', m)   = m'  `quotRem` 60
        (d,  h)   = h'  `quotRem` 24
    in (toInteger d, h, m, s)


-- ----------------------  Property

-- | Sign of clock as @-1@, @0@, @1@.
clockSign :: Clock -> Int
clockSign c | day == 0 && sec == 0  = 0
            | day >= 0 && sec >= 0  = 1
            | day <= 0 && sec <= 0  = -1
            | otherwise             = B.bug $ "inconsistent " ++ show c
            where day = clockDayCount c
                  sec = clockSec      c

-- | MJD part of clock.
clockDayCount :: Clock -> DayCount
clockDayCount (ClockDhms day _)  = day
clockDayCount (ClockDhm  day _)  = day
clockDayCount (ClockDh   day _)  = day
clockDayCount (ClockD    day)    = day

-- | Second part of clock.
clockSec :: Clock -> Sec
clockSec (ClockDhms _ sec)       = sec
clockSec (ClockDhm  _ sec)       = sec
clockSec (ClockDh   _ sec)       = sec
clockSec (ClockD    _)           = 0

-- | Precision string of clock.
clockPrecision :: Clock -> String
clockPrecision (ClockDhms _ _)   = "sec"
clockPrecision (ClockDhm  _ _)   = "min"
clockPrecision (ClockDh   _ _)   = "hour"
clockPrecision (ClockD    _)     = "day"

-- | Extract elements of clock.
clockDhms :: Clock -> (DayCount, Maybe Hour, Maybe Min, Maybe Sec)
clockDhms clock =
    case clock of
      ClockDhms _ _  -> (day + d, Just h, Just m, Just s)
      ClockDhm  _ _  -> (day + d, Just h, Just m, Nothing)
      ClockDh   _ _  -> (day + d, Just h, Nothing, Nothing)
      ClockD    _    -> (day + d, Nothing, Nothing, Nothing)
    where
      day             = abs $ clockDayCount clock
      sec             = abs $ clockSec clock
      (d, h, m, s)    = dhmsFromSec $ abs sec

-- | Replace elements of clock.
clockAlter :: Maybe DayCount -> Maybe Hour -> Maybe Min -> Maybe Sec -> O.Map Clock
clockAlter d' h' m' s' clock =
    case clockDhms clock of
      (d, Just h,  Just m,  Just s)  -> clockFromDhms (d' ! d) (h' ! h) (m' ! m) (s' ! s)
      (d, Just h,  Just m,  Nothing) -> clockFromDhm  (d' ! d) (h' ! h) (m' ! m)
      (d, Just h,  Nothing, Nothing) -> clockFromDh   (d' ! d) (h' ! h)
      (d, Nothing, Nothing, Nothing) -> clockFromD    (d' ! d)
      _                              -> B.bug "clockAlter"
    where
      Just x' ! _  = x'
      Nothing ! x  = x


-- ----------------------  Writer

-- | @|D'HH:MM:SS|@
instance Show Clock where
    show = B.mixToFlatString . B.mixEncode

-- | @|D'HH:MM:SS|@
instance B.MixEncode Clock where
    mixEncode = clockToMix

-- | Convert clock to mix text with vertical bars.
clockToMix :: Clock -> B.MixText
clockToMix = B.mixBracket "|" "|" . clockBodyToMix

-- | Convert clock to mix text.
--
--   >>> clockBodyToMix $ clockFromDhm 0 9 40
--   MixText "09:40"
--
--   >>> B.mixEncode $ clockFromDhm 0 9 40
--   MixText "|09:40|"
--
clockBodyToMix :: Clock -> B.MixText
clockBodyToMix c = body $ clockPos c where
    sign = signToMix $ clockSign c
    body (ClockDhms day sec)  = sign $ daySecToMix dhmsToMix day sec
    body (ClockDhm  day sec)  = sign $ daySecToMix dhmToMix  day sec
    body (ClockDh   day sec)  = sign $ daySecToMix dhToMix   day sec
    body (ClockD    day)      = sign $ dayToMix day

-- | Convert clock to positive clock.
clockPos :: O.Map Clock
clockPos = clockMap abs abs

signToMix :: Int -> O.Map B.MixText
signToMix (-1) m = B.mixString "-" B.<> m
signToMix _    m = m

daySecToMix :: (Sec -> (DayCount, B.MixText)) -> DayCount -> Sec -> B.MixText
daySecToMix secMix day sec =
    let (d, mx) = secMix sec
    in case day + d of
         0  -> mx
         d2 -> dayToMix d2 B.<> mx

mixColon :: B.Bin B.MixText
mixColon = B.mixInfix ":"

dhmsToMix :: Sec -> (DayCount, B.MixText)
dhmsToMix sec = (d, hms) where
    hms            = D.mix02 h `mixColon` D.mix02 m `mixColon` D.mix02 s
    (d, h, m, s)   = dhmsFromSec $ abs sec

dhmToMix :: Sec -> (DayCount, B.MixText)
dhmToMix sec = (d, hm) where
    hm             = D.mix02 h `mixColon` D.mix02 m
    (d, h, m, _)   = dhmsFromSec sec

dhToMix :: Sec -> (DayCount, B.MixText)
dhToMix sec = (d, hm) where
    hm             = D.mix02 h
    (d, h, _, _)   = dhmsFromSec sec

dayToMix :: DayCount -> B.MixText
dayToMix d = B.mixDec d B.<> B.mixString "'"


-- ----------------------  Map

-- | Map MDJ and second of clock.
clockMap :: O.Map DayCount -> O.Map Sec -> O.Map Clock
clockMap f g (ClockDhms d s)  = adjust ClockDhms (f d) (g s)
clockMap f g (ClockDhm  d s)  = adjust ClockDhm  (f d) (g s)
clockMap f g (ClockDh   d s)  = adjust ClockDh   (f d) (g s)
clockMap f _ (ClockD    d)    =        ClockD    (f d)

-- | Combine MDJ and second of two clocks.
clockMap2 :: B.Bin DayCount -> B.Bin Sec -> B.BinAb Clock
clockMap2 f g (ClockDhms d s) (ClockDhms e t)  = adjustAb ClockDhms (f d e) (g s t)
clockMap2 f g (ClockDhm  d s) (ClockDhm  e t)  = adjustAb ClockDhm  (f d e) (g s t)
clockMap2 f g (ClockDh   d s) (ClockDh   e t)  = adjustAb ClockDh   (f d e) (g s t)
clockMap2 f _ (ClockD    d)   (ClockD    e)    = Right  $ ClockD    (f d e)
clockMap2 _ _ _ _ = Msg.adlib "clock"

-- | Seconds in a day, i.e., 86400 seconds.
daySeconds :: (Num a) => a
daySeconds = 86400   -- 24 * 60 * 60

adjustAb :: (DayCount -> Sec -> Clock) -> DayCount -> Sec -> B.Ab Clock
adjustAb k d s = Right $ adjust k d s

adjust :: (DayCount -> Sec -> Clock) -> DayCount -> Sec -> Clock
adjust k d s = let (d', s')  = s `divMod` daySeconds
                   d2        = d + toInteger d'
               in if d2 < 0 && s' > 0
                  then k (d2 + 1) (s' - daySeconds)
                  else k d2 s'

