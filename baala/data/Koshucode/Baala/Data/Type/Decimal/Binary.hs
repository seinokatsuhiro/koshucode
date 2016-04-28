{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

-- | Binary operator for decimals.

module Koshucode.Baala.Data.Type.Decimal.Binary
  ( -- * Type
    Bin, BinAb,
    DecimalBin, DecimalBinAb,
    PrecisionCombine,

    -- * Binary operator
    decimalBin, decimalBinAb,
    decimalAddSimple,
    decimalSubSimple,
    decimalMulSimple,
    decimalDivSimple,
  ) where

import qualified Koshucode.Baala.Base                        as B
import qualified Koshucode.Baala.Data.Type.Decimal.Decimal   as D


-- --------------------------------------------  Type

-- | Type for binary operators.
type Bin a = a -> a -> a

-- | Type for abortable binary operators.
type BinAb a = a -> a -> B.Ab a

type DecimalBin = Bin D.Decimal

-- | Binary operation for two decimals
type DecimalBinAb = BinAb D.Decimal

-- | Combinate precisions.
type PrecisionCombine = Bin D.DecimalFracl


-- --------------------------------------------  Binary operator

decimalBinAb :: PrecisionCombine -> Bin D.DecimalRatio -> DecimalBinAb
decimalBinAb pre bin x y = Right $ decimalBin pre bin x y

decimalBin :: PrecisionCombine -> Bin D.DecimalRatio -> DecimalBin
decimalBin pre bin
      D.Decimal { D.decimalRatio = r1, D.decimalFracl = p1, D.decimalApprox = a1 }
      D.Decimal { D.decimalRatio = r2, D.decimalFracl = p2, D.decimalApprox = a2 }
    = D.Decimal { D.decimalRatio  = bin r1 r2
                , D.decimalFracl  = pre p1 p2
                , D.decimalApprox = a1 || a2 }

decimalAddSimple :: DecimalBin
decimalAddSimple = decimalBin max (+)

decimalSubSimple :: DecimalBin
decimalSubSimple = decimalBin max (-)

decimalMulSimple :: DecimalBin
decimalMulSimple = decimalBin (+) (*)

decimalDivSimple :: DecimalBin
decimalDivSimple = decimalBin (+) (/)

