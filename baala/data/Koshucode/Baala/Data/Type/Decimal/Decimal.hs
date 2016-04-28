{-# LANGUAGE RecordWildCards #-}
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
    (%%),
    DecimalRatio, DecimalInteger, DecimalFracl, Decimal (..),
    decimalRatioEq, decimalRatioCompare,
    isDecimalZero,
    decimalNum, decimalDenom,
    decimalFraclSet,

    -- * Conversion
    decimal0, decimal1,
    integralDecimal, intDecimal, realDecimal,
    decimalFractional,
  ) where

import qualified Data.Ratio                        as R
import qualified Koshucode.Baala.Base              as B



-- ----------------------  Type

(%%) :: DecimalInteger -> DecimalInteger -> DecimalRatio
x %% y = x R.% y

type DecimalRatio = R.Ratio DecimalInteger

type DecimalInteger = Integer

-- | Length of fractional part,
--   e.g., the length of the fractional part of 4.050 is 3.
type DecimalFracl = Int

-- | Decimal number.
data Decimal = Decimal 
    { decimalApprox  :: Bool
    , decimalFracl   :: DecimalFracl   -- ^ Length of the fractional part
    , decimalRatio   :: DecimalRatio   -- ^ Rational number for the decimal number
    } deriving (Show)

instance Eq Decimal where
    (==) = decimalRatioEq

instance Ord Decimal where
    compare = decimalRatioCompare

-- | Test numerical equality of two decimal numbers.
decimalRatioEq :: Decimal -> Decimal -> Bool
decimalRatioEq x y = decimalRatio x == decimalRatio y

-- | Test numerical order of two decimal numbers.
decimalRatioCompare :: Decimal -> Decimal -> Ordering
decimalRatioCompare x y = decimalRatio x `compare` decimalRatio y

-- | Test decimal is zero.
isDecimalZero :: Decimal -> Bool
isDecimalZero Decimal {..} = decimalRatio == 0

-- | Numerator part of decimal number.
decimalNum :: Decimal -> DecimalInteger
decimalNum = R.numerator . decimalRatio

-- | Denominator part of decimal number.
decimalDenom :: Decimal -> DecimalInteger
decimalDenom = R.denominator . decimalRatio

-- | Change decimal fracl.
decimalFraclSet :: DecimalFracl -> B.AbMap Decimal
decimalFraclSet f d@Decimal {..} = Right $ d { decimalFracl = f }


-- ----------------------  Conversion

-- | The integral decimal 0, i.e., 'integralDecimal 0'.
decimal0 :: Decimal
decimal0 = intDecimal 0

-- | The integral decimal 1, i.e., 'integralDecimal 1'.
decimal1 :: Decimal
decimal1 = intDecimal 1

-- | Convert integral number to integral decimal number.
integralDecimal :: (Integral n) => n -> Decimal
integralDecimal = realDecimal 0

intDecimal :: Int -> Decimal
intDecimal = realDecimal 0

-- | Convert real number to decimal number.
realDecimal :: (Real n) => DecimalFracl -> n -> Decimal
realDecimal p n = Decimal { decimalRatio  = toRational n
                          , decimalFracl  = p
                          , decimalApprox = False }

-- | Convert decimal number to fractional number.
decimalFractional :: (Fractional n) => Decimal -> n
decimalFractional (Decimal { decimalRatio = r }) =
    fromRational $ toRational (R.numerator r %% R.denominator r)

