{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

-- | Decimal number.
--
--   A decimal number in Koshucode is internally represented
--   using a rational number ('decimalRatio')
--   with a length of fractional part ('decimalFracle') .
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

module Koshucode.Baala.Type.Decimal.Decimal
  ( -- * Decimal
    DecimalInteger, DecimalRatio,
    DecimalFracle, Decimal (..),
    decimalFracleSet,
    decimalRatioMap,

    -- * Binary
    BinDecimal, BinAbDecimal,
    BinFracle, BinRatio,
    decimalBin, decimalBinAb,

    -- * Conversion
    integralDecimal,
    realDecimal,
    decimalFractional,
    decimalDouble,
  ) where

import qualified Data.Ratio                        as R
import qualified Koshucode.Baala.Overture          as O
import qualified Koshucode.Baala.Base              as B


-- --------------------------------------------  Decimal

-- | Type for numerator or denominator for decimal numbers.
type DecimalInteger = Integer

-- | Rational number of decimal type.
type DecimalRatio = R.Ratio DecimalInteger

-- | Length of fractional part.
type DecimalFracle = Int

-- | Decimal number.
data Decimal = Decimal 
    { decimalFracle  :: DecimalFracle  -- ^ Length of the fractional part
    , decimalRatio   :: DecimalRatio   -- ^ Rational number for the decimal number
    }

instance Show Decimal where
    show Decimal {..} = "Decimal (" ++ fracle ++ ") " ++ ratio
        where fracle = show decimalFracle
              ratio  = show i ++ showF
              showF  = if f == 0 then "" else " + " ++ show f
              (i, f) = properFraction decimalRatio :: (DecimalInteger, DecimalRatio)

-- | Change decimal fracle.
decimalFracleSet :: DecimalFracle -> B.AbMap Decimal
decimalFracleSet f d@Decimal {..} = Right $ d { decimalFracle = f }

-- | Map function to rational number in decimal.
decimalRatioMap :: O.Map DecimalRatio -> O.Map Decimal
decimalRatioMap f d@Decimal {..} = d { decimalRatio = f decimalRatio }


-- --------------------------------------------  Binary

-- | Binary operation for two decimals.
type BinDecimal = O.Bin Decimal

-- | Abortable binary operation for two decimals.
type BinAbDecimal = B.BinAb Decimal

-- | Combinate fracle.
type BinFracle = O.Bin DecimalFracle

-- | Combinate rational number.
type BinRatio = O.Bin DecimalRatio

-- | Binary operation for two decimals.
decimalBin :: BinFracle -> BinRatio -> BinDecimal
decimalBin frac bin
      Decimal { decimalFracle = f1, decimalRatio = r1 }
      Decimal { decimalFracle = f2, decimalRatio = r2 }
    = Decimal { decimalFracle = frac f1 f2
              , decimalRatio  = bin r1 r2 }

-- | Abortable binary operation for two decimals.
decimalBinAb :: BinFracle -> BinRatio -> BinAbDecimal
decimalBinAb frac bin x y = Right $ decimalBin frac bin x y


-- --------------------------------------------  Conversion

-- | Convert integral number to integral decimal number.
integralDecimal :: (Integral n) => n -> Decimal
integralDecimal = realDecimal 0

-- | Convert real number to decimal number.
realDecimal :: (Real n) => DecimalFracle -> n -> Decimal
realDecimal f n = Decimal { decimalFracle = f
                          , decimalRatio  = toRational n }

-- | Convert decimal number to fractional number.
decimalFractional :: (Fractional n) => Decimal -> n
decimalFractional = fromRational . decimalRatio

-- | Convert decimal number to double.
decimalDouble :: Decimal -> Double
decimalDouble = decimalFractional


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

