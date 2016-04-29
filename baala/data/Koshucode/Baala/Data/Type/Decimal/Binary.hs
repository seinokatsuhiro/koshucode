{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

-- | Binary type.

module Koshucode.Baala.Data.Type.Decimal.Binary
  ( Bin, BinAb,
    BinDecimal, BinAbDecimal,
    BinFracl, BinRatio,
    decimalBin, decimalBinAb,
  ) where

import qualified Koshucode.Baala.Base                        as B
import qualified Koshucode.Baala.Data.Type.Decimal.Decimal   as D

-- | Type for binary operators.
type Bin a = a -> a -> a

-- | Type for abortable binary operators.
type BinAb a = a -> a -> B.Ab a

-- | Binary operation for two decimals.
type BinDecimal = Bin D.Decimal

-- | Abortable binary operation for two decimals.
type BinAbDecimal = BinAb D.Decimal

-- | Combinate fracl.
type BinFracl = Bin D.DecimalFracl

-- | Combinate rational number.
type BinRatio = Bin D.DecimalRatio

-- | Binary operation for two decimals.
decimalBin :: BinFracl -> BinRatio -> BinDecimal
decimalBin fracl bin
      D.Decimal { D.decimalRatio = r1, D.decimalFracl = f1, D.decimalApprox = a1 }
      D.Decimal { D.decimalRatio = r2, D.decimalFracl = f2, D.decimalApprox = a2 }
    = D.Decimal { D.decimalFracl  = fracl f1 f2
                , D.decimalRatio  = bin r1 r2
                , D.decimalApprox = a1 || a2 }

-- | Abortable binary operation for two decimals.
decimalBinAb :: BinFracl -> Bin D.DecimalRatio -> BinAbDecimal
decimalBinAb fracl bin x y = Right $ decimalBin fracl bin x y

