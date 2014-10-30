{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Data.Clock
  ( Clock (..), DayCount, Hour, Min, Sec,
    hmsFromSec, secFromHms,
    clockRangeHour,
    clockRangeMinute,
    clockRangeSecond,
  ) where

import qualified Koshucode.Baala.Base.Prelude  as B
import qualified Koshucode.Baala.Base.Text     as B

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
