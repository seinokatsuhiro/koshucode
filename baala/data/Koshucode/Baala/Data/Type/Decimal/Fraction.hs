{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

-- | Decimal number.

module Koshucode.Baala.Data.Type.Decimal.Fraction
  ( -- * Part of decimals
    decimalIntPart, decimalFracPart,
    decimalIntFrac,
    decimalNum, decimalDenom,

    -- * Round decimals
    decimalRound, decimalRoundAt, decimalRoundPer,
    decimalRoundEven, decimalRoundEvenAt, decimalRoundEvenPer,
    decimalTrunc, decimalTruncAt, decimalTruncPer,
    chopDigitsTrancate,
    chopDigitsRound,
    roundLastDigit,
  ) where

import qualified Data.Ratio                                  as R
import qualified Koshucode.Baala.Base                        as B
import qualified Koshucode.Baala.Data.Type.Decimal.Decimal   as D
import qualified Koshucode.Baala.Data.Type.Decimal.Rational  as D


-- --------------------------------------------  Part

-- | Integer part of decimals.
decimalIntPart :: B.Map D.Decimal
decimalIntPart = fst . decimalIntFrac

-- | Fractional part of decimals.
decimalFracPart :: B.Map D.Decimal
decimalFracPart = snd . decimalIntFrac

decimalIntFrac :: D.Decimal -> (D.Decimal, D.Decimal)
decimalIntFrac d = (dec $ i D.%% 1, dec f) where
    (i, f) = properFraction $ D.decimalRatio d
    dec r  = D.decimalRatioMap (const r) d

-- | Numerator part of decimal number.
decimalNum :: D.Decimal -> D.DecimalInteger
decimalNum = R.numerator . D.decimalRatio

-- | Denominator part of decimal number.
decimalDenom :: D.Decimal -> D.DecimalInteger
decimalDenom = R.denominator . D.decimalRatio


-- --------------------------------------------  Round

updateDecimal :: D.Decimal -> D.DecimalFracl -> D.DecimalRatio -> D.Decimal
updateDecimal d l r = d { D.decimalFracl = l
                        , D.decimalRatio = r }

-- ----------------------  Round

-- | Round decimal per self fractional length.
decimalRound :: B.Map D.Decimal
decimalRound d@D.Decimal { D.decimalFracl = l, D.decimalRatio = r } =
    updateDecimal d l $ D.ratioRoundAt l r

-- | Round decimal per fractional length.
decimalRoundAt :: B.Bin D.Decimal
decimalRoundAt D.Decimal { D.decimalRatio = l }
             d@D.Decimal { D.decimalRatio = r } =
    let l' = fst $ properFraction l
    in updateDecimal d l' $ D.ratioRoundAt l' r

-- | Round decimal per unit decimal.
decimalRoundPer :: B.Bin D.Decimal
decimalRoundPer D.Decimal { D.decimalFracl = l, D.decimalRatio = per }
              d@D.Decimal { D.decimalRatio = r } =
    updateDecimal d l $ D.ratioRoundPer per r

-- ----------------------  Round to even

-- | Round decimal to even per self fractional length.
decimalRoundEven :: B.Map D.Decimal
decimalRoundEven d@D.Decimal { D.decimalFracl = l, D.decimalRatio = r } =
    updateDecimal d l $ D.ratioRoundEvenAt l r

-- | Round decimal to even per fractional length.
decimalRoundEvenAt :: B.Bin D.Decimal
decimalRoundEvenAt D.Decimal { D.decimalRatio = l }
                 d@D.Decimal { D.decimalRatio = r } =
    let l' = fst $ properFraction l
    in updateDecimal d l' $ D.ratioRoundEvenAt l' r

-- | Round decimal to even per unit decimal.
decimalRoundEvenPer :: B.Bin D.Decimal
decimalRoundEvenPer D.Decimal { D.decimalFracl = l, D.decimalRatio = per }
                  d@D.Decimal { D.decimalRatio = r } =
    updateDecimal d l $ D.ratioRoundEvenPer per r

-- ----------------------  Truncate

-- | Truncate decimal per fractional length.
decimalTruncAt :: B.Bin D.Decimal
decimalTruncAt D.Decimal { D.decimalRatio = l }
             d@D.Decimal { D.decimalRatio = r } =
    let l' = fst $ properFraction l
    in updateDecimal d l' $ D.ratioTruncAt l' r

-- | Truncate decimal per self fractional length.
decimalTrunc :: B.Map D.Decimal
decimalTrunc d@D.Decimal { D.decimalFracl = l, D.decimalRatio = r } =
    updateDecimal d l $ D.ratioTruncAt l r

-- | Truncate decimal per unit decimal.
decimalTruncPer :: B.Bin D.Decimal
decimalTruncPer D.Decimal { D.decimalFracl = l, D.decimalRatio = per }
              d@D.Decimal { D.decimalRatio = r } =
    updateDecimal d l $ D.ratioTruncPer per r

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

