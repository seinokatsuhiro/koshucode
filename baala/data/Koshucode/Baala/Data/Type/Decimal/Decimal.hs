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
  ( -- * Rational
    DecimalInteger, DecimalRatio, (%%),
    -- * Decimal
    DecimalFracl, Decimal (..),
    isDecimalZero,
    decimalNum, decimalDenom,
    decimalFraclSet,
    decimalRatioMap,

    -- * Binary
    Bin, BinAb,
    BinDecimal, BinAbDecimal,
    BinFracl, BinRatio,
    decimalBin, decimalBinAb,
  ) where

import qualified Data.Ratio                        as R
import qualified Koshucode.Baala.Base              as B


-- ----------------------  Rational

-- | Type for numerator or denominator for decimal numbers.
type DecimalInteger = Integer

-- | Rational number of decimal type.
type DecimalRatio = R.Ratio DecimalInteger

-- | Make rational numbers.
(%%) :: DecimalInteger -> DecimalInteger -> DecimalRatio
n %% den = n R.% den


-- ----------------------  Decimal

-- | Length of fractional part.
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

decimalRatioMap :: B.Map DecimalRatio -> B.Map Decimal
decimalRatioMap f d@Decimal {..} = d { decimalRatio = f decimalRatio }


-- --------------------------------------------  Binary

-- | Type for binary operators.
type Bin a = a -> a -> a

-- | Type for abortable binary operators.
type BinAb a = a -> a -> B.Ab a

-- | Binary operation for two decimals.
type BinDecimal = Bin Decimal

-- | Abortable binary operation for two decimals.
type BinAbDecimal = BinAb Decimal

-- | Combinate fracl.
type BinFracl = Bin DecimalFracl

-- | Combinate rational number.
type BinRatio = Bin DecimalRatio

-- | Binary operation for two decimals.
decimalBin :: BinFracl -> BinRatio -> BinDecimal
decimalBin fracl bin
      Decimal { decimalRatio = r1, decimalFracl = f1, decimalApprox = a1 }
      Decimal { decimalRatio = r2, decimalFracl = f2, decimalApprox = a2 }
    = Decimal { decimalFracl  = fracl f1 f2
              , decimalRatio  = bin r1 r2
              , decimalApprox = a1 || a2 }

-- | Abortable binary operation for two decimals.
decimalBinAb :: BinFracl -> Bin DecimalRatio -> BinAbDecimal
decimalBinAb fracl bin x y = Right $ decimalBin fracl bin x y

