{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

-- | Decimal number.

module Koshucode.Baala.Data.Type.Decimal.Fraction
  ( -- * Part of decimals
    decimalIntPart, decimalFracPart,
    decimalIntFrac,
    decimalNum, decimalDenom,

    -- * Round decimals
    decimalRound,
    decimalRoundAt,
    decimalRoundPer,
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

-- | Round decimal per self fractional length.
decimalRound :: B.Map D.Decimal
decimalRound d@D.Decimal { D.decimalFracl = l, D.decimalRatio = r } =
    d { D.decimalFracl = l
      , D.decimalRatio = D.ratioRoundAt l r }

-- | Round decimal per fractional length.
decimalRoundAt :: B.Bin D.Decimal
decimalRoundAt D.Decimal { D.decimalRatio = l }
             d@D.Decimal { D.decimalRatio = r } =
    let l' = fst $ properFraction l
    in d { D.decimalFracl = l'
         , D.decimalRatio = D.ratioRoundAt l' r }

-- | Round decimal per unit decimal.
decimalRoundPer :: B.Bin D.Decimal
decimalRoundPer D.Decimal { D.decimalFracl = l, D.decimalRatio = per }
              d@D.Decimal { D.decimalRatio = r } =
    d { D.decimalFracl = l
      , D.decimalRatio = D.ratioRoundPer per r }

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

