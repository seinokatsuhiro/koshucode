{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

-- | Instance implementation of decimals.

module Koshucode.Baala.Data.Type.Decimal.Instance
  ( integralDecimal,
    realDecimal,
    decimalFractional,
  ) where

import qualified Data.Ratio                                    as R
import qualified Koshucode.Baala.Data.Type.Decimal.Decimal     as D
import qualified Koshucode.Baala.Data.Type.Decimal.Arithmetic  as D

instance Num D.Decimal where
    (+)          = D.decimalAddSimple
    (-)          = D.decimalSubSimple
    (*)          = D.decimalMulSimple
    negate       = D.decimalInvert
    abs          = D.decimalAbs
    signum       = decimalSignum
    fromInteger  = integralDecimal

decimalSignum :: D.Decimal -> D.Decimal
decimalSignum D.Decimal {..} = realDecimal 0 $ signum decimalRatio

-- | Convert integral number to integral decimal number.
integralDecimal :: (Integral n) => n -> D.Decimal
integralDecimal = realDecimal 0

-- | Convert real number to decimal number.
realDecimal :: (Real n) => D.DecimalFracl -> n -> D.Decimal
realDecimal p n = D.Decimal { D.decimalRatio  = toRational n
                            , D.decimalFracl  = p
                            , D.decimalApprox = False }

-- | Convert decimal number to fractional number.
decimalFractional :: (Fractional n) => D.Decimal -> n
decimalFractional (D.Decimal { D.decimalRatio = r }) =
    fromRational $ toRational (R.numerator r D.%% R.denominator r)

instance Real D.Decimal where
    toRational = D.decimalRatio

instance Fractional D.Decimal where
    (/)           = D.decimalDivSimple
    recip         = D.decimalRecip
    fromRational  = realDecimal 0

