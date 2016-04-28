{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

-- | Arithmetics on decimals.

module Koshucode.Baala.Data.Type.Decimal.Arithmetic
  ( -- * Precision
    PrecisionSide (..),
    PrecisionCombine,

    -- * Binary operator
    DecimalBin, DecimalBinAb,
    Bin, BinAb,
    decimalAdd, decimalAddHigh, decimalAddSimple,
    decimalSub, decimalSubSimple,
    decimalMul, decimalMulSimple,
    decimalDiv, decimalDivSimple,
    decimalQuo,
    decimalRem,

    -- * Unary operator
    decimalInvert,
    decimalRecip,
    decimalAbs,
    decimalSum,
  ) where

import qualified Control.Monad                               as M
import qualified Koshucode.Baala.Base                        as B
import qualified Koshucode.Baala.Data.Type.Decimal.Coder     as D
import qualified Koshucode.Baala.Data.Type.Decimal.Decimal   as D
import qualified Koshucode.Baala.Data.Type.Decimal.Fraction  as D
import qualified Koshucode.Baala.Data.Type.Message           as Msg

data PrecisionSide
    = PrecisionHigh       -- ^ Select high precision
    | PrecisionLeft       -- ^ Select left precision
    | PrecisionRight      -- ^ Select right precision
    | PrecisionStrict     -- ^ Check same precision
      deriving (Show, Ord, Eq)

-- | Combinate precisions.
type PrecisionCombine = Bin D.DecimalFracl

constLeft :: a -> b -> a
constLeft = const

constRight :: a -> b -> b
constRight _ y = y


-- ----------------------  Type and Higher function

type DecimalBin = Bin D.Decimal

-- | Binary operation for two decimals
type DecimalBinAb = BinAb D.Decimal

-- | Type for binary operators.
type Bin a = a -> a -> a

-- | Type for abortable binary operators.
type BinAb a = a -> a -> B.Ab a

decimalBinAbMax :: Bin D.DecimalRatio -> DecimalBinAb
decimalBinAbMax = decimalBinAb max

decimalBinAbPlus :: Bin D.DecimalRatio -> DecimalBinAb
decimalBinAbPlus = decimalBinAb (+)

decimalBinAbLeft :: Bin D.DecimalRatio -> DecimalBinAb
decimalBinAbLeft = decimalBinAb constLeft

decimalBinAbRight :: Bin D.DecimalRatio -> DecimalBinAb
decimalBinAbRight = decimalBinAb constRight

decimalBinAb :: PrecisionCombine -> Bin D.DecimalRatio -> DecimalBinAb
decimalBinAb pre bin x y = Right $ decimalBin pre bin x y

decimalBin :: PrecisionCombine -> Bin D.DecimalRatio -> DecimalBin
decimalBin pre bin
      D.Decimal { D.decimalRatio = r1, D.decimalFracl = p1, D.decimalApprox = a1 }
      D.Decimal { D.decimalRatio = r2, D.decimalFracl = p2, D.decimalApprox = a2 }
    = D.Decimal { D.decimalRatio  = bin r1 r2
                , D.decimalFracl  = pre p1 p2
                , D.decimalApprox = a1 || a2 }


-- ----------------------  Binary operator

-- | Addition: /x/ + /y/
decimalAdd :: PrecisionSide -> DecimalBinAb
decimalAdd PrecisionHigh    = decimalBinAbMax   (+)
decimalAdd PrecisionLeft    = decimalBinAbLeft  (+)
decimalAdd PrecisionRight   = decimalBinAbRight (+)
decimalAdd PrecisionStrict  = decimalAddStrict

-- | Addition with 'PrecisionHigh'.
decimalAddHigh :: DecimalBinAb
decimalAddHigh = decimalBinAbMax (+)

decimalAddSimple :: DecimalBin
decimalAddSimple = decimalBin max (+)

decimalAddStrict :: DecimalBinAb
decimalAddStrict d1@D.Decimal { D.decimalFracl = p1 }
                 d2@D.Decimal { D.decimalFracl = p2 }
    | p1 == p2   = decimalAddHigh d1 d2
    | otherwise  = Msg.heteroDecimal txt1 txt2
    where txt1   = D.encodeDecimal d1
          txt2   = D.encodeDecimal d2

-- | Subtruction: /x/ - /y/
decimalSub :: PrecisionSide -> DecimalBinAb
decimalSub pr x y = decimalAdd pr x $ decimalInvert y

decimalSubSimple :: DecimalBin
decimalSubSimple = decimalBin max (-)

-- | Multiplication: /x/ × /y/
decimalMul :: DecimalBinAb
decimalMul = decimalBinAbPlus (*)

decimalMulSimple :: DecimalBin
decimalMulSimple = decimalBin (+) (*)

-- | Division: /x/ ÷ /y/
decimalDiv :: DecimalBinAb
decimalDiv = decimalBinAbPlus (/)

decimalDivSimple :: DecimalBin
decimalDivSimple = decimalBin (+) (/)

-- | Quotient: integral part of /x/ ÷ /y/
decimalQuo :: DecimalBinAb
decimalQuo x y = do z <- decimalDiv x y
                    Right $ D.decimalIntPart z

-- | Remainder.
decimalRem :: DecimalBinAb
decimalRem x y = do z <- decimalDiv x y
                    r <- y `decimalMul` D.decimalFracPart z
                    Right r


-- ----------------------  Unary operator

decimalRatioMap :: B.Map D.DecimalRatio -> B.Map D.Decimal
decimalRatioMap f d@D.Decimal {..} = d { D.decimalRatio = f decimalRatio }

-- | Invert sign of decimal
decimalInvert :: B.Map D.Decimal
decimalInvert = decimalRatioMap negate

-- | Exchange numerator and denominator
decimalRecip :: B.Map D.Decimal
decimalRecip = decimalRatioMap recip

-- | Absolute value
decimalAbs :: B.Map D.Decimal
decimalAbs = decimalRatioMap abs

-- | Add all decimals.
decimalSum :: [D.Decimal] -> B.Ab D.Decimal
decimalSum = M.foldM decimalAddHigh D.decimal0

