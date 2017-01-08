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
import qualified Koshucode.Baala.Type.Time.Clock         as T
import qualified Koshucode.Baala.Type.Time.Parts         as T


-- ----------------------  Calculation

-- | Convert clock to positive clock.
clockPos :: O.Map T.Clock
clockPos = T.clockMap abs abs

-- | Convert clock to negative clock.
clockNeg :: O.Map T.Clock
clockNeg = T.clockMap neg neg

neg :: (Ord a, Num a) => a -> a
neg a | a > 0      = - a
      | otherwise  = a

-- | Set MJD to zero.
clockCutDay :: O.Map T.Clock
clockCutDay = T.clockMap (const 0) id

-- | Add MJT.
clockAddDay :: T.Days -> O.Map T.Clock
clockAddDay d = T.clockMap (+ d) id

-- | Add second.
clockAddSec :: T.Sec -> O.Map T.Clock
clockAddSec s = T.clockMap id (+ s)

-- | Calculation of clock plus clock.
clockAdd :: B.BinAb T.Clock
clockAdd = T.clockMap2 (+) (+)

-- | Calculation of clock minus clock.
clockSub :: B.BinAb T.Clock
clockSub = T.clockMap2 (-) (-)

-- | Multiplication of clock.
clockTimes :: Int -> O.Map T.Clock
clockTimes m = T.clockMap (* (toInteger m)) (* m)


-- ----------------------  Range

-- | Create sequence between two clocks.
--
--   >>> clockRangeBy (clockStep 120) (clockFromDhm 0 0 0) (clockFromDhm 0 0 5)
--   [|00:00:00|, |00:02:00|, |00:04:00|]
--
clockRangeBy :: O.Map (T.Days, T.Sec) -> B.RangeBy T.Clock
clockRangeBy step from to = clocks where
    from'  =  clockTuple from
    to'    =  clockTuple to
    clocks =  map fromClockTuple $ B.rangeBy step from' to'

-- | Create clock step of given second.
clockStep :: T.Sec -> O.Map (T.Days, T.Sec)
clockStep sec (d, s) = let (d', s') = (sec + s) `quotRem` T.daySeconds
                       in (d + toInteger d', s')

clockTuple :: T.Clock -> (T.Days, T.Sec)
clockTuple (T.ClockDhms d s)  =  (d, s)
clockTuple (T.ClockDhm  d s)  =  (d, s)
clockTuple (T.ClockDh   d s)  =  (d, s)
clockTuple (T.ClockD    d)    =  (d, 0)

fromClockTuple :: (T.Days, T.Sec) -> T.Clock
fromClockTuple (d, s) = T.ClockDhms d s
