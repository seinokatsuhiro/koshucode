{-# OPTIONS_GHC -Wall #-}

-- | Decimal number.
--
--   A decimal number in Koshucode is internally represented
--   using a rational number ('decimalRatio')
--   with a length of fractional part ('decimalFracl') .
--   For example, the decimal number 11.250 correnponds to
--   the rational number 45/4 with the fraction length 3.
--
--   [Decimal number]
--     Number represented using 10 digits and the decimal separator.
--   [Decimal separator]
--     Symbol between integer part and fractional part.
--   [Fractional part]
--     The right part of decimal numbers, e.g., 250 for 11.250.
--   [Integer part]
--     The left part of decimal numbers, e.g., 11 for 11.250.
--   [Integral decimal]
--     Decimal number without fractional part.
--     

module Koshucode.Baala.Data.Type.Decimal.Decimal
  ( -- * Type
    DecimalInteger, DecimalFracl, Decimal (..),
    isDecimalZero,
    decimalNum, decimalDenom,
    decimalFraclSet,
    reduceDecimal,

    -- * Conversion
    decimal0, decimal1,
    integralDecimal, realDecimal,
    decimalFractional,

    -- * Chop
    chopDigitsTrancate,
    chopDigitsRound,
    roundLastDigit,
  ) where

import qualified Data.Ratio                        as R
import qualified Koshucode.Baala.Base              as B



-- ----------------------  Type

type DecimalInteger = Integer

-- | Length of fractional part.
type DecimalFracl   = Int

-- | Decimal number.
data Decimal = Decimal 
    { decimalRatio   :: (DecimalInteger, DecimalInteger)
                                       -- ^ Rational number for the decimal number
    , decimalFracl   :: DecimalFracl   -- ^ Length of the fractional part
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

-- | Change decimal fracl.
decimalFraclSet :: DecimalFracl -> B.AbMap Decimal
decimalFraclSet f (Decimal r _ a) = Right $ Decimal r f a

reduceDecimal :: (DecimalInteger, DecimalInteger) -> DecimalFracl -> Bool -> Decimal
reduceDecimal (n, den) = Decimal (n `div` g, den `div` g) where
    g = gcd n den


-- ----------------------  Conversion

-- | The integral decimal 0, i.e., 'integralDecimal 0'.
decimal0 :: Decimal
decimal0 = integralDecimal (0 :: DecimalInteger)

-- | The integral decimal 1, i.e., 'integralDecimal 1'.
decimal1 :: Decimal
decimal1 = integralDecimal (1 :: DecimalInteger)

-- | Convert integral number to integral decimal number.
integralDecimal :: (Integral n) => n -> Decimal
integralDecimal = realDecimal 0

-- | Convert real number to decimal number.
realDecimal :: (Real n) => DecimalFracl -> n -> Decimal
realDecimal p n = Decimal (R.numerator r, R.denominator r) p False where
    r = toRational n

-- | Convert decimal number to fractional number.
decimalFractional :: (Fractional n) => Decimal -> n
decimalFractional (Decimal { decimalRatio = (num, den)}) =
    fromRational $ toRational $ (num R.% den)


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

