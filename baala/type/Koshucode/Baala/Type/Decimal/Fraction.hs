{-# OPTIONS_GHC -Wall #-}

-- | Decimal functions.

module Koshucode.Baala.Type.Decimal.Fraction
  ( -- * Part of decimals
    -- ** Integer and fraction
    decimalIntPart, decimalFracPart,
    decimalIntFrac,
    -- ** Numerator and denominator
    decimalNum, decimalDenom,

    -- * Conver to integer
    -- ** Round
    decimalRound, decimalRoundAt, decimalRoundPer,
    -- ** Round to even
    decimalRoundEven, decimalRoundEvenAt, decimalRoundEvenPer,
    -- ** Truncate
    decimalTrunc, decimalTruncAt, decimalTruncPer,
    decimalTruncError,
    -- ** Round out
    decimalRoundOut, decimalRoundOutAt, decimalRoundOutPer,
    -- ** Floor
    decimalFloor, decimalFloorAt, decimalFloorPer,
    -- ** Ceil
    decimalCeil, decimalCeilAt, decimalCeilPer,
    -- ** Other
    chopDigitsTrancate,
    chopDigitsRound,
    roundLastDigit,
  ) where

import qualified Data.Ratio                                as R
import qualified Koshucode.Baala.Overture                  as O
import qualified Koshucode.Baala.Type.Decimal.Decimal      as D
import qualified Koshucode.Baala.Type.Decimal.Rational     as D


-- --------------------------------------------  Part

-- ----------------------  Integer and fraction

-- | Integer part of decimal.
decimalIntPart :: O.Map D.Decimal
decimalIntPart = fst . decimalIntFrac

-- | Fractional part of decimal.
decimalFracPart :: O.Map D.Decimal
decimalFracPart = snd . decimalIntFrac

-- | Integer and fractional parts of decimal.
--
--   >>> decimalIntFrac $ D.realDecimal 2 (4.5 :: Double)
--   (Decimal (2) 4, Decimal (2) 0 + 1 % 2)
--
decimalIntFrac :: D.Decimal -> (D.Decimal, D.Decimal)
decimalIntFrac d = (dec $ i D.%% 1, dec f) where
    (i, f) = properFraction $ D.decimalRatio d
    dec r  = D.decimalRatioMap (const r) d

-- ----------------------  Numerator and denominator

-- | Numerator part of decimal number.
decimalNum :: D.Decimal -> D.DecimalInteger
decimalNum = R.numerator . D.decimalRatio

-- | Denominator part of decimal number.
decimalDenom :: D.Decimal -> D.DecimalInteger
decimalDenom = R.denominator . D.decimalRatio


-- --------------------------------------------  Round

updateDecimal :: D.Decimal -> D.DecimalFracle -> D.DecimalRatio -> D.Decimal
updateDecimal d l r = d { D.decimalFracle = l
                        , D.decimalRatio  = r }

-- ----------------------  Round

-- | Round decimal per self fractional length.
decimalRound :: O.Map D.Decimal
decimalRound d@D.Decimal { D.decimalFracle = l, D.decimalRatio = r } =
    updateDecimal d l $ D.ratioRoundAt l r

-- | Round decimal per fractional length.
decimalRoundAt :: O.Bin D.Decimal
decimalRoundAt D.Decimal { D.decimalRatio = l }
             d@D.Decimal { D.decimalRatio = r } =
    let l' = fst $ properFraction l
    in updateDecimal d l' $ D.ratioRoundAt l' r

-- | Round decimal per unit decimal.
decimalRoundPer :: O.Bin D.Decimal
decimalRoundPer D.Decimal { D.decimalFracle = l, D.decimalRatio = per }
              d@D.Decimal { D.decimalRatio = r } =
    updateDecimal d l $ D.ratioRoundPer per r

-- ----------------------  Round to even

-- | Round decimal to even per self fractional length.
decimalRoundEven :: O.Map D.Decimal
decimalRoundEven d@D.Decimal { D.decimalFracle = l, D.decimalRatio = r } =
    updateDecimal d l $ D.ratioRoundEvenAt l r

-- | Round decimal to even per fractional length.
decimalRoundEvenAt :: O.Bin D.Decimal
decimalRoundEvenAt D.Decimal { D.decimalRatio = l }
                 d@D.Decimal { D.decimalRatio = r } =
    let l' = fst $ properFraction l
    in updateDecimal d l' $ D.ratioRoundEvenAt l' r

-- | Round decimal to even per unit decimal.
decimalRoundEvenPer :: O.Bin D.Decimal
decimalRoundEvenPer D.Decimal { D.decimalFracle = l, D.decimalRatio = per }
                  d@D.Decimal { D.decimalRatio = r } =
    updateDecimal d l $ D.ratioRoundEvenPer per r

-- ----------------------  Truncate

-- | Truncate decimal per self fractional length.
decimalTrunc :: O.Map D.Decimal
decimalTrunc d@D.Decimal { D.decimalFracle = l, D.decimalRatio = r } =
    updateDecimal d l $ D.ratioTruncAt l r

-- | Truncate decimal per fractional length.
decimalTruncAt :: O.Bin D.Decimal
decimalTruncAt D.Decimal { D.decimalRatio = l }
             d@D.Decimal { D.decimalRatio = r } =
    let l' = fst $ properFraction l
    in updateDecimal d l' $ D.ratioTruncAt l' r

-- | Truncate decimal per unit decimal.
decimalTruncPer :: O.Bin D.Decimal
decimalTruncPer D.Decimal { D.decimalFracle = l, D.decimalRatio = per }
              d@D.Decimal { D.decimalRatio = r } =
    updateDecimal d l $ D.ratioTruncPer per r

-- | Truncation error of decimal.
decimalTruncError :: O.Map D.Decimal
decimalTruncError d@D.Decimal { D.decimalFracle = l, D.decimalRatio = r } =
    updateDecimal d (l + 1) (r `D.ratioRem` D.ratioFracle l)

-- ----------------------  Round out

-- | Round out (toward infinity) decimal to even per self fractional length.
decimalRoundOut :: O.Map D.Decimal
decimalRoundOut d@D.Decimal { D.decimalFracle = l, D.decimalRatio = r } =
    updateDecimal d l $ D.ratioRoundOutAt l r

-- | Round out (toward infinity) decimal to even per fractional length.
decimalRoundOutAt :: O.Bin D.Decimal
decimalRoundOutAt D.Decimal { D.decimalRatio = l }
                 d@D.Decimal { D.decimalRatio = r } =
    let l' = fst $ properFraction l
    in updateDecimal d l' $ D.ratioRoundOutAt l' r

-- | Round out (toward infinity) decimal to even per unit decimal.
decimalRoundOutPer :: O.Bin D.Decimal
decimalRoundOutPer D.Decimal { D.decimalFracle = l, D.decimalRatio = per }
                  d@D.Decimal { D.decimalRatio = r } =
    updateDecimal d l $ D.ratioRoundOutPer per r

-- ----------------------  Floor

-- | Floor decimal per self fractional length.
decimalFloor :: O.Map D.Decimal
decimalFloor d@D.Decimal { D.decimalFracle = l, D.decimalRatio = r } =
    updateDecimal d l $ D.ratioFloorAt l r

-- | Floor decimal per fractional length.
decimalFloorAt :: O.Bin D.Decimal
decimalFloorAt D.Decimal { D.decimalRatio = l }
             d@D.Decimal { D.decimalRatio = r } =
    let l' = fst $ properFraction l
    in updateDecimal d l' $ D.ratioFloorAt l' r

-- | Floor decimal per unit decimal.
decimalFloorPer :: O.Bin D.Decimal
decimalFloorPer D.Decimal { D.decimalFracle = l, D.decimalRatio = per }
              d@D.Decimal { D.decimalRatio = r } =
    updateDecimal d l $ D.ratioFloorPer per r

-- ----------------------  Ceil

-- | Ceiling decimal per self fractional length.
decimalCeil :: O.Map D.Decimal
decimalCeil d@D.Decimal { D.decimalFracle = l, D.decimalRatio = r } =
    updateDecimal d l $ D.ratioCeilAt l r

-- | Ceiling decimal per fractional length.
decimalCeilAt :: O.Bin D.Decimal
decimalCeilAt D.Decimal { D.decimalRatio = l }
             d@D.Decimal { D.decimalRatio = r } =
    let l' = fst $ properFraction l
    in updateDecimal d l' $ D.ratioCeilAt l' r

-- | Ceiling decimal per unit decimal.
decimalCeilPer :: O.Bin D.Decimal
decimalCeilPer D.Decimal { D.decimalFracle = l, D.decimalRatio = per }
              d@D.Decimal { D.decimalRatio = r } =
    updateDecimal d l $ D.ratioCeilPer per r

-- ----------------------  Other

-- | @chopDigitsTrancate@ /d/ /n/ returns a number
--   which does not have the tailing /d/ digits.
--   If /d/ is zero or negative, it returns just /n/.
chopDigitsTrancate :: (Integral d, Integral n) => d -> n -> n
chopDigitsTrancate d n = fst $ chopDigits d n

-- | @chopDigitsRound@ is similar to 'chopDigitsTrancate',
--   but rounds chopped digit.
chopDigitsRound :: (Integral d, Integral n) => d -> n -> n
chopDigitsRound d n =
  case chopDigits d n of
   (n', True)   -> n' + 1
   (n', False)  -> n'

chopDigits :: (Integral d, Integral n) => d -> n -> (n, Bool)
chopDigits d n
  | d <= 0     = (n, False)
  | otherwise  = (sig * q, r >= half)
  where
    sig        = signum n
    divisor    = 10 ^ d
    half       = divisor `quot` 2
    (q, r)     = abs n `quotRem` divisor

-- | Round the last (least significant) digit.
roundLastDigit :: (Integral n) => n -> n
roundLastDigit n = sig * n' where
  sig = signum n
  a   = abs n
  n'  = case a `rem` 10 of
         r | r >= 5     -> a - r + 10
           | otherwise  -> a - r
