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
  ( -- * Decimal
    DecimalInteger, DecimalRatio,
    DecimalFracl, Decimal (..),
    decimalFraclSet,
    decimalRatioMap,

    -- * Binary
    BinDecimal, BinAbDecimal,
    BinFracl, BinRatio,
    decimalBin, decimalBinAb,

    -- * Conversion
    integralDecimal,
    realDecimal,
    decimalFractional,
  ) where

import qualified Data.Ratio                        as R
import qualified Koshucode.Baala.Base              as B


-- --------------------------------------------  Decimal

-- | Type for numerator or denominator for decimal numbers.
type DecimalInteger = Integer

-- | Rational number of decimal type.
type DecimalRatio = R.Ratio DecimalInteger

-- | Length of fractional part.
type DecimalFracl = Int

-- | Decimal number.
data Decimal = Decimal 
    { decimalFracl   :: DecimalFracl   -- ^ Length of the fractional part
    , decimalRatio   :: DecimalRatio   -- ^ Rational number for the decimal number
    } deriving (Show)

-- | Change decimal fracl.
decimalFraclSet :: DecimalFracl -> B.AbMap Decimal
decimalFraclSet f d@Decimal {..} = Right $ d { decimalFracl = f }

-- | Map function to rational number in decimal.
decimalRatioMap :: B.Map DecimalRatio -> B.Map Decimal
decimalRatioMap f d@Decimal {..} = d { decimalRatio = f decimalRatio }


-- --------------------------------------------  Binary

-- | Binary operation for two decimals.
type BinDecimal = B.Bin Decimal

-- | Abortable binary operation for two decimals.
type BinAbDecimal = B.BinAb Decimal

-- | Combinate fracl.
type BinFracl = B.Bin DecimalFracl

-- | Combinate rational number.
type BinRatio = B.Bin DecimalRatio

-- | Binary operation for two decimals.
decimalBin :: BinFracl -> BinRatio -> BinDecimal
decimalBin fracl bin
      Decimal { decimalFracl = f1, decimalRatio = r1 }
      Decimal { decimalFracl = f2, decimalRatio = r2 }
    = Decimal { decimalFracl  = fracl f1 f2
              , decimalRatio  = bin r1 r2 }

-- | Abortable binary operation for two decimals.
decimalBinAb :: BinFracl -> BinRatio -> BinAbDecimal
decimalBinAb fracl bin x y = Right $ decimalBin fracl bin x y


-- --------------------------------------------  Conversion

-- | Convert integral number to integral decimal number.
integralDecimal :: (Integral n) => n -> Decimal
integralDecimal = realDecimal 0

-- | Convert real number to decimal number.
realDecimal :: (Real n) => DecimalFracl -> n -> Decimal
realDecimal f n = Decimal { decimalFracl  = f
                          , decimalRatio  = toRational n }

-- | Convert decimal number to fractional number.
decimalFractional :: (Fractional n) => Decimal -> n
decimalFractional = fromRational . decimalRatio


-- --------------------------------------------  Instance

-- | Test numerical equality of two decimal numbers.
decimalRatioEq :: Decimal -> Decimal -> Bool
decimalRatioEq x y = decimalRatio x == decimalRatio y

-- | Test numerical order of two decimal numbers.
decimalRatioCompare :: Decimal -> Decimal -> Ordering
decimalRatioCompare x y = decimalRatio x `compare` decimalRatio y

-- | Decompose decimal into integer part and fraction part.
decimalIntFrac :: (Integral n) => Decimal -> (n, Decimal)
decimalIntFrac d = (i, dec f) where
    (i, f) = properFraction $ decimalRatio d
    dec r  = decimalRatioMap (const r) d

-- Basic

instance Eq Decimal where
    (==)           = decimalRatioEq

instance Ord Decimal where
    compare        = decimalRatioCompare

instance Enum Decimal where
    toEnum         = fromIntegral
    fromEnum       = fromInteger . truncate
    succ d         = d + 1
    pred d         = d - 1

-- Numeric

instance Num Decimal where
    (+)            = decimalBin max (+)
    (-)            = decimalBin max (-)
    (*)            = decimalBin (+) (*)
    negate         = decimalRatioMap negate
    abs            = decimalRatioMap abs
    signum         = realDecimal 0 . signum . decimalRatio
    fromInteger    = realDecimal 0

instance Real Decimal where
    toRational = decimalRatio

instance Fractional Decimal where
    (/)            = decimalBin (+) (/)
    recip          = decimalRatioMap recip
    fromRational   = realDecimal 0

instance RealFrac Decimal where
    properFraction = decimalIntFrac
