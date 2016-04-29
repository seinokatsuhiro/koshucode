{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

-- | Instance implementation of decimals.

module Koshucode.Baala.Data.Type.Decimal.Instance
  ( integralDecimal,
    realDecimal,
    decimalFractional,
  ) where

import qualified Koshucode.Baala.Data.Type.Decimal.Decimal     as D
import qualified Koshucode.Baala.Data.Type.Decimal.Fraction    as D


-- ----------------------  Conversion

-- | Convert integral number to integral decimal number.
integralDecimal :: (Integral n) => n -> D.Decimal
integralDecimal = realDecimal 0

-- | Convert real number to decimal number.
realDecimal :: (Real n) => D.DecimalFracl -> n -> D.Decimal
realDecimal f n = D.Decimal { D.decimalRatio  = toRational n
                            , D.decimalFracl  = f
                            , D.decimalApprox = False }

-- | Convert decimal number to fractional number.
decimalFractional :: (Fractional n) => D.Decimal -> n
decimalFractional = fromRational . D.decimalRatio


-- ----------------------  Instance

instance Num D.Decimal where
    (+)          = D.decimalBin max (+)
    (-)          = D.decimalBin max (-)
    (*)          = D.decimalBin (+) (*)
    negate       = D.decimalRatioMap negate
    abs          = D.decimalRatioMap abs
    signum       = realDecimal 0 . signum . D.decimalRatio
    fromInteger  = integralDecimal

instance Real D.Decimal where
    toRational = D.decimalRatio

instance Fractional D.Decimal where
    (/)           = D.decimalBin (+) (/)
    recip         = D.decimalRatioMap recip
    fromRational  = realDecimal 0

instance RealFrac D.Decimal where
    properFraction = D.decimalProperFractionSimple

