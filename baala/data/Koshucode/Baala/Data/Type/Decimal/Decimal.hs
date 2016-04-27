{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Data.Type.Decimal.Decimal
  ( -- * Type
    DecimalInteger, DecimalPoint, Decimal (..),
    isDecimalZero,
    decimalNum, decimalDenom,
    decimalPointSet,
    reduceDecimal,

    -- * Convert
    integralDecimal, realDecimal,
    decimalFractional,
    decimal0, decimal1,

    -- * Chop
    chopDigitsTrancate,
    chopDigitsRound,
    roundLastDigit,
  ) where

import qualified Data.Ratio                        as R
import qualified Koshucode.Baala.Base              as B



-- ----------------------  Type

type DecimalInteger = Integer
type DecimalPoint   = Int

data Decimal = Decimal 
    { decimalRatio   :: (DecimalInteger, DecimalInteger)
    , decimalPoint   :: DecimalPoint
    , decimalApprox  :: Bool
    } deriving (Show, Eq, Ord)

-- | Test decimal is zero.
isDecimalZero :: Decimal -> Bool
isDecimalZero (Decimal (n, _) _ _)  = n == 0

-- | Numerator part of decimal number.
decimalNum :: Decimal -> DecimalInteger
decimalNum = fst . decimalRatio

-- | Denominator part of decimal number.
decimalDenom :: Decimal -> DecimalInteger
decimalDenom = snd . decimalRatio

-- | Change decimal point.
decimalPointSet :: DecimalPoint -> B.AbMap Decimal
decimalPointSet p (Decimal r _ a) = Right $ Decimal r p a

reduceDecimal :: (DecimalInteger, DecimalInteger) -> DecimalPoint -> Bool -> Decimal
reduceDecimal (n, den) = Decimal (n `div` g, den `div` g) where
    g = gcd n den


-- ----------------------  Convert

-- | Convert integral number to decimal number.
integralDecimal :: (Integral n) => n -> Decimal
integralDecimal = realDecimal 0

-- | Convert real number to decimal number.
realDecimal :: (Real n) => DecimalPoint -> n -> Decimal
realDecimal p n = Decimal (R.numerator r, R.denominator r) p False where
    r = toRational n

-- | Convert decimal number to fractional number.
decimalFractional :: (Fractional n) => Decimal -> n
decimalFractional (Decimal { decimalRatio = (num, den)}) =
    fromRational $ toRational $ (num R.% den)

decimal0 :: Decimal
decimal0 = integralDecimal (0 :: DecimalInteger)

decimal1 :: Decimal
decimal1 = integralDecimal (1 :: DecimalInteger)


-- ----------------------  Chop

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

