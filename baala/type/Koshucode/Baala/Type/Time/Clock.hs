{-# OPTIONS_GHC -Wall #-}

-- | Clock: distance between two times.

module Koshucode.Baala.Type.Time.Clock
  ( -- * Clock
    Clock (..),
    ToClock (..),
    clockBodyToMix,

    -- * Constructor
    clockFromDhms, clockFromDhm,
    clockFromDh, clockFromD,
    dhmsFromSec, secFromHms,

    -- * Property
    clockDaysSec,
    clockDaysClock,
    clockSign, 
    clockPrecision,
    clockAtts, clockAlter,

    -- * Conversion
    clockMap,
    clockMap2,

    -- * Days and seconds
    DaysSec,
    daysSec,
    daySeconds,
  ) where

import qualified Koshucode.Baala.Overture                as O
import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Type.Time.Date          as T
import qualified Koshucode.Baala.Type.Time.Parts         as T
import qualified Koshucode.Baala.Base.Message            as Msg


-- ----------------------  Data type

-- | Clock as distance between two times.
data Clock
    = ClockDhms T.Days T.Sec    -- ^ Clock represented by multiple of second
    | ClockDhm  T.Days T.Sec    -- ^ Clock represented by multiple of minute
    | ClockDh   T.Days T.Sec    -- ^ Clock represented by multiple of hour
    | ClockD    T.Days          -- ^ Clock represented by multiple of day

instance Eq Clock where
    (==) = O.ordEq

instance Ord Clock where
    compare = O.compareOn clockDaysSec

-- | Convertible to clock.
class ToClock a where
    toClock :: a -> Clock

instance ToClock Clock where
    toClock = id

instance ToClock T.ClockParts where
    toClock (T.ClockPartsSec  d h m s)  = clockFromDhms d h m s
    toClock (T.ClockPartsMin  d h m)    = clockFromDhm  d h m
    toClock (T.ClockPartsHour d h)      = clockFromDh   d h
    toClock (T.ClockPartsDays d)        = clockFromD    d

-- | Create clock from days, hour, minute, and second.
--
--   >>> clockFromDhms 1 9 40 20
--   |1'09:40:20|
--
clockFromDhms :: T.Days -> T.Hour -> T.Min -> T.Sec -> Clock
clockFromDhms d h m s = ClockDhms d $ secFromHms (h, m, s)

-- | Create clock from days, hour, and minute.
clockFromDhm :: T.Days -> T.Hour -> T.Min -> Clock
clockFromDhm d h m = ClockDhm d $ secFromHms (h, m, 0)

-- | Create clock from days and hour.
clockFromDh :: T.Days -> T.Hour -> Clock
clockFromDh d h = ClockDh d $ secFromHms (h, 0, 0)

-- | Create clock from days.
clockFromD :: T.Days -> Clock
clockFromD = ClockD

-- | Aggregate hour, minute, and second into single second.
secFromHms :: (T.Hour, T.Min, T.Sec) -> T.Sec
secFromHms (h, m, s) = s + 60 * (m + 60 * h)

-- | Decompose second into days, hour, minute and second parts.
--
--   >>> dhmsFromSec 333333
--   (3,9,15,33)
--
dhmsFromSec :: T.Sec -> (T.Days, T.Hour, T.Min, T.Sec)
dhmsFromSec sec =
    let (m', s)   = sec `quotRem` 60
        (h', m)   = m'  `quotRem` 60
        (d,  h)   = h'  `quotRem` 24
    in (toInteger d, h, m, s)


-- ----------------------  Property

-- | Days and second of clock.
--
--   >>> clockDaysSec $ clockFromDhms 1 9 15 33
--   (1, 33333)
--
clockDaysSec :: Clock -> DaysSec
clockDaysSec (ClockDhms d s)  = (d, s)
clockDaysSec (ClockDhm  d s)  = (d, s)
clockDaysSec (ClockDh   d s)  = (d, s)
clockDaysSec (ClockD    d)    = (d, 0)

-- | Split days part from clock.
--
--   >>> clockDaysClock $ clockFromDhms 1 9 15 33
--   (1, |09:15:33|)
--
clockDaysClock :: Clock -> (T.Days, Clock)
clockDaysClock (ClockDhms d s)  = (d, ClockDhms 0 s)
clockDaysClock (ClockDhm  d s)  = (d, ClockDhm  0 s)
clockDaysClock (ClockDh   d s)  = (d, ClockDh   0 s)
clockDaysClock (ClockD    d)    = (d, ClockD    0)

-- | Sign of clock as @-1@, @0@, @1@.
--
--   >>> clockSign $ clockFromDhms 1 9 15 33
--   1
--
--   >>> clockSign $ clockFromDhms 0 0 0 0
--   0
--
clockSign :: Clock -> Int
clockSign c | d == 0 && s == 0  = 0
            | d >= 0 && s >= 0  = 1
            | d <= 0 && s <= 0  = -1
            | otherwise         = B.bug $ "inconsistent " ++ show c
            where (d, s) = clockDaysSec c

-- | Precision string of clock.
clockPrecision :: Clock -> String
clockPrecision (ClockDhms _ _)   = "sec"
clockPrecision (ClockDhm  _ _)   = "min"
clockPrecision (ClockDh   _ _)   = "hour"
clockPrecision (ClockD    _)     = "day"

-- | All attributes of clock.
clockAtts :: Clock -> (T.Days, Maybe T.Hour, Maybe T.Min, Maybe T.Sec)
clockAtts clock =
    case clock of
      ClockDhms _ _  -> (day + d, Just h, Just m, Just s)
      ClockDhm  _ _  -> (day + d, Just h, Just m, Nothing)
      ClockDh   _ _  -> (day + d, Just h, Nothing, Nothing)
      ClockD    _    -> (day + d, Nothing, Nothing, Nothing)
    where
      (day, sec)      = abs2 $ clockDaysSec clock
      (d, h, m, s)    = dhmsFromSec sec

abs2 :: (Num a, Num b) => O.Map (a, b)
abs2 = map2 abs abs

map2 :: (a -> a') -> (b -> b') -> (a, b) -> (a', b')
map2 f g (x, y) = (f x, g y)

-- | Replace elements of clock.
clockAlter :: Maybe T.Days -> Maybe T.Hour -> Maybe T.Min -> Maybe T.Sec -> O.Map Clock
clockAlter d' h' m' s' clock =
    case clockAtts clock of
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
clockToMix = B.mixBracket ("|", "|") . clockBodyToMix

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
signToMix (-1) m = B.mixString "-" O.++ m
signToMix _    m = m

daySecToMix :: (T.Sec -> (T.Days, B.MixText)) -> T.Days -> T.Sec -> B.MixText
daySecToMix secMix day sec =
    let (d, mx) = secMix sec
    in case day + d of
         0  -> mx
         d2 -> dayToMix d2 O.++ mx

mixColon :: O.Bin B.MixText
mixColon = B.mixInfix ":"

dhmsToMix :: T.Sec -> (T.Days, B.MixText)
dhmsToMix sec = (d, hms) where
    hms            = T.mix02 h `mixColon` T.mix02 m `mixColon` T.mix02 s
    (d, h, m, s)   = dhmsFromSec $ abs sec

dhmToMix :: T.Sec -> (T.Days, B.MixText)
dhmToMix sec = (d, hm) where
    hm             = T.mix02 h `mixColon` T.mix02 m
    (d, h, m, _)   = dhmsFromSec sec

dhToMix :: T.Sec -> (T.Days, B.MixText)
dhToMix sec = (d, hm) where
    hm             = T.mix02 h
    (d, h, _, _)   = dhmsFromSec sec

dayToMix :: T.Days -> B.MixText
dayToMix d = B.mixDec d O.++ B.mixString "'"


-- ----------------------  Map

-- | Map MDJ and second of clock.
clockMap :: O.Map T.Days -> O.Map T.Sec -> O.Map Clock
clockMap f g (ClockDhms d s)  = adjust ClockDhms (f d) (g s)
clockMap f g (ClockDhm  d s)  = adjust ClockDhm  (f d) (g s)
clockMap f g (ClockDh   d s)  = adjust ClockDh   (f d) (g s)
clockMap f _ (ClockD    d)    =        ClockD    (f d)

-- | Combine MDJ and second of two clocks.
clockMap2 :: O.Bin T.Days -> O.Bin T.Sec -> B.BinAb Clock
clockMap2 f g (ClockDhms d s) (ClockDhms e t)  = adjustAb ClockDhms (f d e) (g s t)
clockMap2 f g (ClockDhm  d s) (ClockDhm  e t)  = adjustAb ClockDhm  (f d e) (g s t)
clockMap2 f g (ClockDh   d s) (ClockDh   e t)  = adjustAb ClockDh   (f d e) (g s t)
clockMap2 f _ (ClockD    d)   (ClockD    e)    = Right  $ ClockD    (f d e)
clockMap2 _ _ _ _ = Msg.adlib "clock"

adjustAb :: (T.Days -> T.Sec -> Clock) -> T.Days -> T.Sec -> B.Ab Clock
adjustAb k d s = Right $ adjust k d s

adjust :: (T.Days -> T.Sec -> Clock) -> T.Days -> T.Sec -> Clock
adjust k d s = let (d', s')  = daysSec s
                   d2        = d + d'
               in if d2 < 0 && s' > 0
                  then k (d2 + 1) (s' - daySeconds)
                  else k d2 s'


-- ----------------------  Days and seconds

-- | Days and seconds.
type DaysSec = (T.Days, T.Sec)

-- | Seconds in a day, i.e., 86400 seconds.
--
--   >>> daySeconds :: Int
--   86400
--
daySeconds :: (Num a) => a
daySeconds = 86400   -- 24 * 60 * 60

-- | Calculate days and seconds.
--
--   >>> daysSec 777777
--   (9, 177)
--
--   >>> daysSec (-777777)
--   (-10, 86223)
--
daysSec :: T.Sec -> DaysSec
daysSec s = let (d', s') = s `divMod` daySeconds
            in (toInteger d', s')

