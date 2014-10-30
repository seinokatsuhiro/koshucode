{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Data.Clock
  ( Clock (..), DayCount, Hour, Min, Sec,
    hmsFromSec, secFromHms,
    clockAdd, clockSub,
    clockRangeHour,
    clockRangeMinute,
    clockRangeSecond,
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
type Hour = Int
type Min  = Int
type Sec  = Int

secFromHms :: (Hour, Min, Sec) -> Sec
secFromHms (h, m, s) = (h * 60 + m) * 60 + s

hmsFromSec :: Sec -> (DayCount, Hour, Min, Sec)
hmsFromSec sec =
    let (m', s)   =  sec `divMod` 60
        (h', m)   =  m'  `divMod` 60
        (d,  h)   =  h'  `divMod` 24
    in (toInteger d, h, m, s)


-- ----------------------  Writer

instance B.Write Clock where
    write _ (ClockDhms day sec)  =  clockDoc dhmsDoc day sec
    write _ (ClockDhm  day sec)  =  clockDoc dhmDoc  day sec
    write _ (ClockDh   day sec)  =  clockDoc dhDoc   day sec
    write _ (ClockD    day)      =  bars $ dayDoc day

clockDoc :: (Sec -> (DayCount, B.Doc)) -> DayCount -> Int -> B.Doc
clockDoc secDoc day sec =
    let (d, doc) = secDoc sec
    in bars$ case day + d of
               0  -> doc
               d2 -> dayDoc d2 B.<> doc

bars :: B.Map B.Doc
bars = B.docWrap "|" "|"

colon :: B.Doc
colon = B.doc ":"

dhmsDoc :: Sec -> (DayCount, B.Doc)
dhmsDoc sec = (d, hms) where
    hms            = dd h B.<> colon B.<> dd m B.<> colon B.<> dd s
    (d, h, m, s)   = hmsFromSec sec

dhmDoc :: Sec -> (DayCount, B.Doc)
dhmDoc sec = (d, hm) where
    hm             = dd h B.<> colon B.<> dd m
    (d, h, m, _)   = hmsFromSec sec

dhDoc :: Sec -> (DayCount, B.Doc)
dhDoc sec = (d, hm) where
    hm             = dd h
    (d, h, _, _)   = hmsFromSec sec

dayDoc :: DayCount -> B.Doc
dayDoc d = B.doc d B.<> B.doc "'"

dd :: Int -> B.Doc
dd n | n < 10    = B.doc $ '0' : show n
     | otherwise = B.doc n


-- ----------------------  Arithmetic

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
make k d s = let (d', s') = s `divMod` 86400
             in Right $ k (d + toInteger d') s'



-- ----------------------  Range

clockRangeSecond :: B.RangeBy Clock
clockRangeSecond = clockRangeBy secondStep

clockRangeMinute :: B.RangeBy Clock
clockRangeMinute = clockRangeBy minuteStep

clockRangeHour :: B.RangeBy Clock
clockRangeHour = clockRangeBy hourStep

clockRangeBy :: B.Map (DayCount, Sec) -> B.RangeBy Clock
clockRangeBy step from to = clocks where
    from'  =  clockTuple from
    to'    =  clockTuple to
    clocks =  map fromClockTuple $ B.rangeBy step from' to'

clockTuple :: Clock -> (DayCount, Sec)
clockTuple (ClockDhms d s)  =  (d, s)
clockTuple (ClockDhm  d s)  =  (d, s)
clockTuple (ClockDh   d s)  =  (d, s)
clockTuple (ClockD    d)    =  (d, 0)

fromClockTuple :: (DayCount, Sec) -> Clock
fromClockTuple (d, s) = ClockDhms d s

hourStep :: B.Map (DayCount, Sec)
hourStep (d, s)   | s < 82800 = (d, s')
                  | otherwise = (d + 1, s' `mod` 86400)
                  where s' = s + 3600

minuteStep :: B.Map (DayCount, Sec)
minuteStep (d, s) | s < 86340 = (d, s')
                  | otherwise = (d + 1, s' `mod` 86400)
                  where s' = s + 60

secondStep :: B.Map (DayCount, Sec)
secondStep (d, s) | s < 86400 = (d, s + 1)
                  | otherwise = (d + 1, 0)
