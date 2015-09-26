{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Data.Type.Clock
  ( -- * Data type
    Clock (..), DayCount, Hour, Min, Sec,
    AbBin,
    writeClock, writeClockBody,

    -- * Accessor
    clockFromDhms, clockFromDhm, clockFromDh, clockFromD,
    clockFromHms,
    dhmsFromSec, secFromHms,

    -- * Property
    clockSign, clockDayCount, clockSec,
    clockPrecision,
    clockDhms, clockAlter,

    -- * Calculation
    clockPos, clockNeg,
    clockCutDay, clockAddDay, clockAddSec,
    clockAdd, clockSub, clockTimes,
    clockRangeBy, clockStep,
  ) where

import qualified Koshucode.Baala.Base.Abort    as B
import qualified Koshucode.Baala.Base.Prelude  as B
import qualified Koshucode.Baala.Base.Text     as B
import qualified Koshucode.Baala.Base.Message  as Msg


-- ----------------------  Data type

data Clock
    = ClockDhms DayCount Sec    -- ^ Clock represented by multiple of second
    | ClockDhm  DayCount Sec    -- ^ Clock represented by multiple of minute
    | ClockDh   DayCount Sec    -- ^ Clock represented by multiple of hour
    | ClockD    DayCount        -- ^ Clock represented by multiple of day
      deriving (Show, Eq, Ord)

type DayCount = Integer
type Hour     = Int
type Min      = Int
type Sec      = Int

clockFromDhms :: DayCount -> Hour -> Min -> Sec -> Clock
clockFromDhms d h m s = ClockDhms d $ secFromHms (h, m, s)

clockFromDhm :: DayCount -> Hour -> Min -> Clock
clockFromDhm d h m = ClockDhm d $ secFromHms (h, m, 0)

clockFromDh :: DayCount -> Hour -> Clock
clockFromDh d h = ClockDh d $ secFromHms (h, 0, 0)

clockFromD :: DayCount -> Clock
clockFromD = ClockD

clockFromHms :: Hour -> Min -> Maybe Sec -> Clock
clockFromHms h m (Nothing)  = clockFromDhm  0 h m
clockFromHms h m (Just s)   = clockFromDhms 0 h m s

-- | Aggregate hour, minute, and second into single second.
secFromHms :: (Hour, Min, Sec) -> Sec
secFromHms (h, m, s) = s + 60 * (m + 60 * h)

-- | Decompose second into day-count, hour, minute and second parts.
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

-- | Day-count part of clock.
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

clockPrecision :: Clock -> String
clockPrecision (ClockDhms _ _)   = "sec"
clockPrecision (ClockDhm  _ _)   = "min"
clockPrecision (ClockDh   _ _)   = "hour"
clockPrecision (ClockD    _)     = "day"

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

clockAlter :: Maybe DayCount -> Maybe Hour -> Maybe Min -> Maybe Sec -> B.Map Clock
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

instance B.Write Clock where
    writeDocWith _ = writeClock

writeClock :: Clock -> B.Doc
writeClock = B.docWrap "|" "|" . writeClockBody

writeClockBody :: Clock -> B.Doc
writeClockBody c = body $ clockPos c where
    sign = signDoc $ clockSign c
    body (ClockDhms day sec)  = sign $ clockDoc dhmsDoc day sec
    body (ClockDhm  day sec)  = sign $ clockDoc dhmDoc  day sec
    body (ClockDh   day sec)  = sign $ clockDoc dhDoc   day sec
    body (ClockD    day)      = sign $ dayDoc day

signDoc :: Int -> B.Map B.Doc
signDoc (-1) doc = B.doc "-" B.<> doc
signDoc _    doc = doc

clockDoc :: (Sec -> (DayCount, B.Doc)) -> DayCount -> Sec -> B.Doc
clockDoc secDoc day sec =
    let (d, doc) = secDoc sec
    in case day + d of
         0  -> doc
         d2 -> dayDoc d2 B.<> doc

colon :: B.Bin B.Doc
colon = B.docConcat ":"

dhmsDoc :: Sec -> (DayCount, B.Doc)
dhmsDoc sec = (d, hms) where
    hms            = B.doc02 h `colon` B.doc02 m `colon` B.doc02 s
    (d, h, m, s)   = dhmsFromSec $ abs sec

dhmDoc :: Sec -> (DayCount, B.Doc)
dhmDoc sec = (d, hm) where
    hm             = B.doc02 h `colon` B.doc02 m
    (d, h, m, _)   = dhmsFromSec sec

dhDoc :: Sec -> (DayCount, B.Doc)
dhDoc sec = (d, hm) where
    hm             = B.doc02 h
    (d, h, _, _)   = dhmsFromSec sec

dayDoc :: DayCount -> B.Doc
dayDoc d = B.doc d B.<> B.doc "'"


-- ----------------------  Map

-- | Type for abortable binary operator.
type AbBin a  = a -> a -> B.Ab a

clockMap :: B.Map DayCount -> B.Map Sec -> B.Map Clock
clockMap f g (ClockDhms d s)  = adjust ClockDhms (f d) (g s)
clockMap f g (ClockDhm  d s)  = adjust ClockDhm  (f d) (g s)
clockMap f g (ClockDh   d s)  = adjust ClockDh   (f d) (g s)
clockMap f _ (ClockD    d)    =        ClockD    (f d)

clockMap2 :: B.Bin DayCount -> B.Bin Sec -> AbBin Clock
clockMap2 f g (ClockDhms d s) (ClockDhms e t)  = adjustAb ClockDhms (f d e) (g s t)
clockMap2 f g (ClockDhm  d s) (ClockDhm  e t)  = adjustAb ClockDhm  (f d e) (g s t)
clockMap2 f g (ClockDh   d s) (ClockDh   e t)  = adjustAb ClockDh   (f d e) (g s t)
clockMap2 f _ (ClockD    d)   (ClockD    e)    = Right  $ ClockD    (f d e)
clockMap2 _ _ _ _ = Msg.adlib "clock"

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


-- ----------------------  Calculation

-- | Convert clock to positive clock.
clockPos :: B.Map Clock
clockPos = clockMap abs abs

-- | Convert clock to negative clock.
clockNeg :: B.Map Clock
clockNeg = clockMap neg neg

neg :: (Ord a, Num a) => a -> a
neg a | a > 0      = - a
      | otherwise  = a

-- | Set day-count to zero.
clockCutDay :: B.Map Clock
clockCutDay = clockMap (const 0) id

-- | Add day-count.
clockAddDay :: DayCount -> B.Map Clock
clockAddDay d = clockMap (+ d) id

-- | Add second.
clockAddSec :: Sec -> B.Map Clock
clockAddSec s = clockMap id (+ s)

-- | Calculation of clock plus clock.
clockAdd :: AbBin Clock
clockAdd = clockMap2 (+) (+)

-- | Calculation of clock minus clock.
clockSub :: AbBin Clock
clockSub = clockMap2 (-) (-)

clockTimes :: Int -> B.Map Clock
clockTimes m = clockMap (* (toInteger m)) (* m)


-- ----------------------  Range

clockRangeBy :: B.Map (DayCount, Sec) -> B.RangeBy Clock
clockRangeBy step from to = clocks where
    from'  =  clockTuple from
    to'    =  clockTuple to
    clocks =  map fromClockTuple $ B.rangeBy step from' to'

clockStep :: Sec -> B.Map (DayCount, Sec)
clockStep sec (d, s) = let (d', s') = (sec + s) `quotRem` daySeconds
                       in (d + toInteger d', s')

clockTuple :: Clock -> (DayCount, Sec)
clockTuple (ClockDhms d s)  =  (d, s)
clockTuple (ClockDhm  d s)  =  (d, s)
clockTuple (ClockDh   d s)  =  (d, s)
clockTuple (ClockD    d)    =  (d, 0)

fromClockTuple :: (DayCount, Sec) -> Clock
fromClockTuple (d, s) = ClockDhms d s
