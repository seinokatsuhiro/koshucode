{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Data.Clock
  ( -- * Data type
    Clock (..), DayCount, Hour, Min, Sec,
    writeClock, writeClockBody,

    -- * Accessor
    clockDayCount, clockSec, clockSign,
    dhmsFromSec, secFromHms,
    clockFromDhms, clockFromDhm, clockFromDh,

    -- * Calculation
    clockPos, clockNeg,
    clockAdd, clockSub,
    clockRangeBy, clockStep,
  ) where

import qualified Koshucode.Baala.Base.Abort    as B
import qualified Koshucode.Baala.Base.Prelude  as B
import qualified Koshucode.Baala.Base.Text     as B
import qualified Koshucode.Baala.Base.Message  as Msg

data Clock
    = ClockDhms DayCount Sec
    | ClockDhm  DayCount Sec
    | ClockDh   DayCount Sec
    | ClockD    DayCount
      deriving (Show, Eq, Ord)

type DayCount = Integer
type Hour     = Int
type Min      = Int
type Sec      = Int

-- | Select day-count part from clock.
clockDayCount :: Clock -> DayCount
clockDayCount (ClockDhms day _) = day
clockDayCount (ClockDhm  day _) = day
clockDayCount (ClockDh   day _) = day
clockDayCount (ClockD    day)   = day

-- | Select second part from clock.
clockSec :: Clock -> Sec
clockSec (ClockDhms _ sec) = sec
clockSec (ClockDhm  _ sec) = sec
clockSec (ClockDh   _ sec) = sec
clockSec (ClockD    _)     = 0

clockSign :: Clock -> Int
clockSign c | day  > 0 || sec  > 0  = 1
            | day  < 0 || sec  < 0  = (-1)
            | day == 0 || sec == 0  = 1
            | otherwise             = B.bug "inconsistent clock"
            where day = clockDayCount c
                  sec = clockSec      c

clockFromDhms :: DayCount -> Hour -> Min -> Sec -> Clock
clockFromDhms d h m s = ClockDhms d $ secFromHms (h, m, s)

clockFromDhm :: DayCount -> Hour -> Min -> Clock
clockFromDhm d h m = ClockDhm d $ secFromHms (h, m, 0)

clockFromDh :: DayCount -> Hour -> Clock
clockFromDh d h = ClockDh d $ secFromHms (h, 0, 0)

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


-- ----------------------  Writer

instance B.Write Clock where
    write _ = writeClock

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

clockDoc :: (Sec -> (DayCount, B.Doc)) -> DayCount -> Int -> B.Doc
clockDoc secDoc day sec =
    let (d, doc) = secDoc sec
    in case day + d of
         0  -> doc
         d2 -> dayDoc d2 B.<> doc

colon :: B.Doc
colon = B.doc ":"

dhmsDoc :: Sec -> (DayCount, B.Doc)
dhmsDoc sec = (d, hms) where
    hms            = dd h B.<> colon B.<> dd m B.<> colon B.<> dd s
    (d, h, m, s)   = dhmsFromSec $ abs sec

dhmDoc :: Sec -> (DayCount, B.Doc)
dhmDoc sec = (d, hm) where
    hm             = dd h B.<> colon B.<> dd m
    (d, h, m, _)   = dhmsFromSec sec

dhDoc :: Sec -> (DayCount, B.Doc)
dhDoc sec = (d, hm) where
    hm             = dd h
    (d, h, _, _)   = dhmsFromSec sec

dayDoc :: DayCount -> B.Doc
dayDoc d = B.doc d B.<> B.doc "'"

dd :: Int -> B.Doc
dd n | n < 10    = B.doc $ '0' : show n
     | otherwise = B.doc n


-- ----------------------  Calculation

clockMap :: B.Map DayCount -> B.Map Sec -> B.Map Clock
clockMap f g (ClockDhms day sec)  = ClockDhms (f day) (g sec)
clockMap f g (ClockDhm  day sec)  = ClockDhm  (f day) (g sec)
clockMap f g (ClockDh   day sec)  = ClockDh   (f day) (g sec)
clockMap f _ (ClockD    day)      = ClockD    (f day)

-- | Convert clock to positive clock.
clockPos :: B.Map Clock
clockPos = clockMap abs abs

-- | Convert clock to negative clock.
clockNeg :: B.Map Clock
clockNeg = clockMap neg neg

neg :: (Ord a, Num a) => a -> a
neg a | a > 0      = - a
      | otherwise  = a

clockAdd :: Clock -> Clock -> B.Ab Clock
clockAdd (ClockD    d1)    (ClockD    d2)      =  clockD    (d1 + d2)
clockAdd (ClockDh   d1 s1) (ClockDh   d2 s2)   =  clockDh   (d1 + d2) (s1 + s2)
clockAdd (ClockDhm  d1 s1) (ClockDhm  d2 s2)   =  clockDhm  (d1 + d2) (s1 + s2)
clockAdd (ClockDhms d1 s1) (ClockDhms d2 s2)   =  clockDhms (d1 + d2) (s1 + s2)
clockAdd _ _ = Msg.adlib "clock"

clockSub :: Clock -> Clock -> B.Ab Clock
clockSub (ClockD    d1)    (ClockD    d2)     =  clockD    (d1 - d2)
clockSub (ClockDh   d1 s1) (ClockDh   d2 s2)  =  clockDh   (d1 - d2) (s1 - s2)
clockSub (ClockDhm  d1 s1) (ClockDhm  d2 s2)  =  clockDhm  (d1 - d2) (s1 - s2)
clockSub (ClockDhms d1 s1) (ClockDhms d2 s2)  =  clockDhms (d1 - d2) (s1 - s2)
clockSub _ _ = Msg.adlib "clock"

clockD :: DayCount -> B.Ab Clock
clockD = Right . ClockD

clockDh :: DayCount -> Sec -> B.Ab Clock
clockDh = make ClockDh

clockDhm :: DayCount -> Sec -> B.Ab Clock
clockDhm = make ClockDhm

clockDhms :: DayCount -> Sec -> B.Ab Clock
clockDhms = make ClockDhms

make :: (DayCount -> Sec -> Clock) -> DayCount -> Sec -> B.Ab Clock
make k d s = let (d', s') = s `quotRem` 86400
             in Right $ k (d + toInteger d') s'



-- ----------------------  Range

clockRangeBy :: B.Map (DayCount, Sec) -> B.RangeBy Clock
clockRangeBy step from to = clocks where
    from'  =  clockTuple from
    to'    =  clockTuple to
    clocks =  map fromClockTuple $ B.rangeBy step from' to'

clockStep :: Int -> B.Map (DayCount, Sec)
clockStep sec (d, s) = let (d', s') = (sec + s) `quotRem` 86400
                       in (d + toInteger d', s')

clockTuple :: Clock -> (DayCount, Sec)
clockTuple (ClockDhms d s)  =  (d, s)
clockTuple (ClockDhm  d s)  =  (d, s)
clockTuple (ClockDh   d s)  =  (d, s)
clockTuple (ClockD    d)    =  (d, 0)

fromClockTuple :: (DayCount, Sec) -> Clock
fromClockTuple (d, s) = ClockDhms d s
