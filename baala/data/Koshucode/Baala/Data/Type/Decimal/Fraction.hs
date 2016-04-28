{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

-- | Decimal number.

module Koshucode.Baala.Data.Type.Decimal.Fraction
  ( decimalIntPart,
    decimalFracPart,
    decimalProperFraction,
    chopDigitsTrancate,
    chopDigitsRound,
    roundLastDigit,
  ) where

import qualified Koshucode.Baala.Base                        as B
import qualified Koshucode.Baala.Data.Type.Decimal.Decimal   as D

-- | Integer part of decimals.
decimalIntPart :: B.Map D.Decimal
decimalIntPart = fst . decimalProperFraction

-- | Fractional part of decimals.
decimalFracPart :: B.Map D.Decimal
decimalFracPart = snd . decimalProperFraction

decimalProperFraction :: D.Decimal -> (D.Decimal, D.Decimal)
decimalProperFraction D.Decimal {..} = (dec $ i D.%% 1, dec f) where
    (i, f) = properFraction decimalRatio
    dec r  = D.Decimal { D.decimalRatio  = r
                       , D.decimalFracl  = decimalFracl
                       , D.decimalApprox = decimalApprox }

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

