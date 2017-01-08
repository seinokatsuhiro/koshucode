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
import qualified Koshucode.Baala.Type.Decimal.Decimal      as T
import qualified Koshucode.Baala.Type.Decimal.Rational     as T


-- --------------------------------------------  Part

-- ----------------------  Integer and fraction

-- | Integer part of decimal.
decimalIntPart :: O.Map T.Decimal
decimalIntPart = fst . decimalIntFrac

-- | Fractional part of decimal.
decimalFracPart :: O.Map T.Decimal
decimalFracPart = snd . decimalIntFrac

-- | Integer and fractional parts of decimal.
--
--   >>> decimalIntFrac $ T.realDecimal 2 (4.5 :: Double)
--   (Decimal (2) 4, Decimal (2) 0 + 1 % 2)
--
decimalIntFrac :: T.Decimal -> (T.Decimal, T.Decimal)
decimalIntFrac d = (dec $ i T.%% 1, dec f) where
    (i, f) = properFraction $ T.decimalRatio d
    dec r  = T.decimalRatioMap (const r) d

-- ----------------------  Numerator and denominator

-- | Numerator part of decimal number.
decimalNum :: T.Decimal -> T.DecimalInteger
decimalNum = R.numerator . T.decimalRatio

-- | Denominator part of decimal number.
decimalDenom :: T.Decimal -> T.DecimalInteger
decimalDenom = R.denominator . T.decimalRatio


-- --------------------------------------------  Round

updateDecimal :: T.Decimal -> T.DecimalFracle -> T.DecimalRatio -> T.Decimal
updateDecimal d l r = d { T.decimalFracle = l
                        , T.decimalRatio  = r }

-- ----------------------  Round

-- | Round decimal per self fractional length.
decimalRound :: O.Map T.Decimal
decimalRound d@T.Decimal { T.decimalFracle = l, T.decimalRatio = r } =
    updateDecimal d l $ T.ratioRoundAt l r

-- | Round decimal per fractional length.
decimalRoundAt :: O.Bin T.Decimal
decimalRoundAt T.Decimal { T.decimalRatio = l }
             d@T.Decimal { T.decimalRatio = r } =
    let l' = fst $ properFraction l
    in updateDecimal d l' $ T.ratioRoundAt l' r

-- | Round decimal per unit decimal.
decimalRoundPer :: O.Bin T.Decimal
decimalRoundPer T.Decimal { T.decimalFracle = l, T.decimalRatio = per }
              d@T.Decimal { T.decimalRatio = r } =
    updateDecimal d l $ T.ratioRoundPer per r

-- ----------------------  Round to even

-- | Round decimal to even per self fractional length.
decimalRoundEven :: O.Map T.Decimal
decimalRoundEven d@T.Decimal { T.decimalFracle = l, T.decimalRatio = r } =
    updateDecimal d l $ T.ratioRoundEvenAt l r

-- | Round decimal to even per fractional length.
decimalRoundEvenAt :: O.Bin T.Decimal
decimalRoundEvenAt T.Decimal { T.decimalRatio = l }
                 d@T.Decimal { T.decimalRatio = r } =
    let l' = fst $ properFraction l
    in updateDecimal d l' $ T.ratioRoundEvenAt l' r

-- | Round decimal to even per unit decimal.
decimalRoundEvenPer :: O.Bin T.Decimal
decimalRoundEvenPer T.Decimal { T.decimalFracle = l, T.decimalRatio = per }
                  d@T.Decimal { T.decimalRatio = r } =
    updateDecimal d l $ T.ratioRoundEvenPer per r

-- ----------------------  Truncate

-- | Truncate decimal per self fractional length.
decimalTrunc :: O.Map T.Decimal
decimalTrunc d@T.Decimal { T.decimalFracle = l, T.decimalRatio = r } =
    updateDecimal d l $ T.ratioTruncAt l r

-- | Truncate decimal per fractional length.
decimalTruncAt :: O.Bin T.Decimal
decimalTruncAt T.Decimal { T.decimalRatio = l }
             d@T.Decimal { T.decimalRatio = r } =
    let l' = fst $ properFraction l
    in updateDecimal d l' $ T.ratioTruncAt l' r

-- | Truncate decimal per unit decimal.
decimalTruncPer :: O.Bin T.Decimal
decimalTruncPer T.Decimal { T.decimalFracle = l, T.decimalRatio = per }
              d@T.Decimal { T.decimalRatio = r } =
    updateDecimal d l $ T.ratioTruncPer per r

-- | Truncation error of decimal.
decimalTruncError :: O.Map T.Decimal
decimalTruncError d@T.Decimal { T.decimalFracle = l, T.decimalRatio = r } =
    updateDecimal d (l + 1) (r `T.ratioRem` T.ratioFracle l)

-- ----------------------  Round out

-- | Round out (toward infinity) decimal to even per self fractional length.
decimalRoundOut :: O.Map T.Decimal
decimalRoundOut d@T.Decimal { T.decimalFracle = l, T.decimalRatio = r } =
    updateDecimal d l $ T.ratioRoundOutAt l r

-- | Round out (toward infinity) decimal to even per fractional length.
decimalRoundOutAt :: O.Bin T.Decimal
decimalRoundOutAt T.Decimal { T.decimalRatio = l }
                 d@T.Decimal { T.decimalRatio = r } =
    let l' = fst $ properFraction l
    in updateDecimal d l' $ T.ratioRoundOutAt l' r

-- | Round out (toward infinity) decimal to even per unit decimal.
decimalRoundOutPer :: O.Bin T.Decimal
decimalRoundOutPer T.Decimal { T.decimalFracle = l, T.decimalRatio = per }
                  d@T.Decimal { T.decimalRatio = r } =
    updateDecimal d l $ T.ratioRoundOutPer per r

-- ----------------------  Floor

-- | Floor decimal per self fractional length.
decimalFloor :: O.Map T.Decimal
decimalFloor d@T.Decimal { T.decimalFracle = l, T.decimalRatio = r } =
    updateDecimal d l $ T.ratioFloorAt l r

-- | Floor decimal per fractional length.
decimalFloorAt :: O.Bin T.Decimal
decimalFloorAt T.Decimal { T.decimalRatio = l }
             d@T.Decimal { T.decimalRatio = r } =
    let l' = fst $ properFraction l
    in updateDecimal d l' $ T.ratioFloorAt l' r

-- | Floor decimal per unit decimal.
decimalFloorPer :: O.Bin T.Decimal
decimalFloorPer T.Decimal { T.decimalFracle = l, T.decimalRatio = per }
              d@T.Decimal { T.decimalRatio = r } =
    updateDecimal d l $ T.ratioFloorPer per r

-- ----------------------  Ceil

-- | Ceiling decimal per self fractional length.
decimalCeil :: O.Map T.Decimal
decimalCeil d@T.Decimal { T.decimalFracle = l, T.decimalRatio = r } =
    updateDecimal d l $ T.ratioCeilAt l r

-- | Ceiling decimal per fractional length.
decimalCeilAt :: O.Bin T.Decimal
decimalCeilAt T.Decimal { T.decimalRatio = l }
             d@T.Decimal { T.decimalRatio = r } =
    let l' = fst $ properFraction l
    in updateDecimal d l' $ T.ratioCeilAt l' r

-- | Ceiling decimal per unit decimal.
decimalCeilPer :: O.Bin T.Decimal
decimalCeilPer T.Decimal { T.decimalFracle = l, T.decimalRatio = per }
              d@T.Decimal { T.decimalRatio = r } =
    updateDecimal d l $ T.ratioCeilPer per r

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

