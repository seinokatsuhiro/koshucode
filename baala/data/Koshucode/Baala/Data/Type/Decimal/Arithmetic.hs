{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

-- | Arithmetics on decimals.

module Koshucode.Baala.Data.Type.Decimal.Arithmetic
  ( -- * Precision
    PrecisionSide (..),
    PrecisionSelector,

    -- * Binary operator
    DecimalBinary,
    Bin, BinAb,
    decimalAdd, decimalAddHigh,
    decimalSub,
    decimalMul,
    decimalDiv,
    decimalQuo,
    decimalRem,

    -- * Unary operator
    decimalInvert,
    decimalRecip,
    decimalAbs,
    decimalSum,
  ) where

import qualified Control.Monad                             as M
import qualified Data.Ratio                                as R
import qualified Koshucode.Baala.Base                      as B
import qualified Koshucode.Baala.Data.Type.Decimal.Coder   as D
import qualified Koshucode.Baala.Data.Type.Decimal.Decimal as D
import qualified Koshucode.Baala.Data.Type.Message         as Msg

data PrecisionSide
    = PrecisionHigh       -- ^ Select high precision
    | PrecisionLeft       -- ^ Select left precision
    | PrecisionRight      -- ^ Select right precision
    | PrecisionStrict     -- ^ Check same precision
      deriving (Show, Ord, Eq)

type PrecisionSelector = D.DecimalFracl -> D.DecimalFracl -> D.DecimalFracl

constLeft :: a -> b -> a
constLeft = const

constRight :: a -> b -> b
constRight _ y = y


-- ----------------------  Binary operator

-- | Binary operation for two decimals
type DecimalBinary = BinAb D.Decimal

-- | Type for binary operators.
type Bin a = a -> a -> a

-- | Type for abortable binary operators.
type BinAb a = a -> a -> B.Ab a

-- | Addition: /x/ + /y/
decimalAdd :: PrecisionSide -> DecimalBinary
decimalAdd PrecisionHigh    = decimalAddBy max
decimalAdd PrecisionLeft    = decimalAddBy constLeft
decimalAdd PrecisionRight   = decimalAddBy constRight
decimalAdd PrecisionStrict  = decimalAddStrict

decimalAddBy :: PrecisionSelector -> DecimalBinary
decimalAddBy sel
             D.Decimal { D.decimalRatio = r1, D.decimalFracl = p1, D.decimalApprox = a1 }
             D.Decimal { D.decimalRatio = r2, D.decimalFracl = p2, D.decimalApprox = a2 }
    = Right $ D.Decimal { D.decimalRatio  = r1 + r2
                        , D.decimalFracl  = sel p1 p2
                        , D.decimalApprox = a1 || a2 }

-- | Addition with 'PrecisionHigh'.
decimalAddHigh :: DecimalBinary
decimalAddHigh = decimalAddBy max

decimalAddStrict :: DecimalBinary
decimalAddStrict d1@(D.Decimal _ p1 _)
                 d2@(D.Decimal _ p2 _)
    | p1 == p2   = decimalAddHigh d1 d2
    | otherwise  = Msg.heteroDecimal txt1 txt2
    where txt1   = D.encodeDecimal d1
          txt2   = D.encodeDecimal d2

-- | Subtruction: /x/ - /y/
decimalSub :: PrecisionSide -> DecimalBinary
decimalSub pr d1 d2 = decimalAdd pr d1 $ decimalInvert d2

-- | Multiplication: /x/ × /y/
decimalMul :: DecimalBinary
decimalMul D.Decimal { D.decimalRatio = r1, D.decimalFracl = p1, D.decimalApprox = a1 }
           D.Decimal { D.decimalRatio = r2, D.decimalFracl = p2, D.decimalApprox = a2 }
    = Right $ D.Decimal { D.decimalRatio  = r1 * r2
                        , D.decimalFracl  = max p1 p2
                        , D.decimalApprox = a1 || a2 }

-- | Division: /x/ ÷ /y/
decimalDiv :: DecimalBinary
decimalDiv dec1 dec2 = decimalMul dec1 $ decimalRecip dec2

-- | Quotient
decimalQuo :: DecimalBinary
decimalQuo = decimalQR quot

-- | Remainder
decimalRem :: DecimalBinary
decimalRem = decimalQR rem

decimalQR :: Bin D.DecimalInteger -> DecimalBinary
decimalQR qr
          d1@(D.Decimal r1 p1 a1)
          d2@(D.Decimal r2 p2 a2)
    | p1 /= p2     = Msg.heteroDecimal txt1 txt2
    | n2 == 0      = Msg.divideByZero
    | otherwise    = Right $ D.Decimal { D.decimalRatio  = n3 D.%% 1
                                       , D.decimalFracl  = p1
                                       , D.decimalApprox = a3 }
    where n1    = R.numerator   r1
          den1  = R.denominator r1
          n2    = R.numerator   r2
          den2  = R.denominator r2
          n3    = (n1 * den2) `qr` (n2 * den1)
          a3    = a1 || a2
          txt1  = D.encodeDecimal d1
          txt2  = D.encodeDecimal d2


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

