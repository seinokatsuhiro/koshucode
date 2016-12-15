{-# OPTIONS_GHC -Wall #-}

-- | Calculation on clock.

module Koshucode.Baala.Type.Time.ClockCalc
  ( clockPos, clockNeg,
    clockCutDay, clockAddDay, clockAddSec,
    clockAdd, clockSub, clockTimes,
    clockRangeBy, clockStep,
  ) where

import qualified Koshucode.Baala.Overture                as O
import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Type.Time.Clock         as D
import qualified Koshucode.Baala.Type.Time.Parts         as D


-- ----------------------  Calculation

-- | Convert clock to positive clock.
clockPos :: O.Map D.Clock
clockPos = D.clockMap abs abs

-- | Convert clock to negative clock.
clockNeg :: O.Map D.Clock
clockNeg = D.clockMap neg neg

neg :: (Ord a, Num a) => a -> a
neg a | a > 0      = - a
      | otherwise  = a

-- | Set MJD to zero.
clockCutDay :: O.Map D.Clock
clockCutDay = D.clockMap (const 0) id

-- | Add MJD.
clockAddDay :: D.Days -> O.Map D.Clock
clockAddDay d = D.clockMap (+ d) id

-- | Add second.
clockAddSec :: D.Sec -> O.Map D.Clock
clockAddSec s = D.clockMap id (+ s)

-- | Calculation of clock plus clock.
clockAdd :: B.BinAb D.Clock
clockAdd = D.clockMap2 (+) (+)

-- | Calculation of clock minus clock.
clockSub :: B.BinAb D.Clock
clockSub = D.clockMap2 (-) (-)

-- | Multiplication of clock.
clockTimes :: Int -> O.Map D.Clock
clockTimes m = D.clockMap (* (toInteger m)) (* m)


-- ----------------------  Range

-- | Create sequence between two clocks.
--
--   >>> clockRangeBy (clockStep 120) (clockFromDhm 0 0 0) (clockFromDhm 0 0 5)
--   [|00:00:00|, |00:02:00|, |00:04:00|]
--
clockRangeBy :: O.Map (D.Days, D.Sec) -> B.RangeBy D.Clock
clockRangeBy step from to = clocks where
    from'  =  clockTuple from
    to'    =  clockTuple to
    clocks =  map fromClockTuple $ B.rangeBy step from' to'

-- | Create clock step of given second.
clockStep :: D.Sec -> O.Map (D.Days, D.Sec)
clockStep sec (d, s) = let (d', s') = (sec + s) `quotRem` D.daySeconds
                       in (d + toInteger d', s')

clockTuple :: D.Clock -> (D.Days, D.Sec)
clockTuple (D.ClockDhms d s)  =  (d, s)
clockTuple (D.ClockDhm  d s)  =  (d, s)
clockTuple (D.ClockDh   d s)  =  (d, s)
clockTuple (D.ClockD    d)    =  (d, 0)

fromClockTuple :: (D.Days, D.Sec) -> D.Clock
fromClockTuple (d, s) = D.ClockDhms d s
