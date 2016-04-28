{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Data.Type.Decimal.Arithmetic
  ( -- * Precision
    PrecisionSide (..),
    PrecisionSelector,

    -- * Binary operator
    DecimalBinary,
    decimalAdd, decimalAddHigh,
    decimalSub,
    decimalMul,
    decimalDiv,
    decimalQuo,
    decimalRem,

    -- * Unary operator
    decimalRevsign,
    decimalRevratio,
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
type DecimalBinary =
    D.Decimal -> D.Decimal -> B.Ab D.Decimal

-- | Addition: /x/ + /y/
decimalAdd :: PrecisionSide -> DecimalBinary
decimalAdd PrecisionHigh    = decimalAddBy max
decimalAdd PrecisionLeft    = decimalAddBy constLeft
decimalAdd PrecisionRight   = decimalAddBy constRight
decimalAdd PrecisionStrict  = decimalAddStrict

decimalAddBy :: PrecisionSelector -> DecimalBinary
decimalAddBy signi (D.Decimal r1 p1 a1)
                   (D.Decimal r2 p2 a2)
    | den1 == den2 = Right $ D.Decimal ((n1 + n2) D.%% den1) p3 a3
    | otherwise    = Right $ D.Decimal (n3        D.%% den3) p3 a3
    where n1     = R.numerator   r1
          den1   = R.denominator r1
          n2     = R.numerator   r2
          den2   = R.denominator r2
          n3     = (n1 * den2) + (n2 * den1)
          den3   = den1 * den2
          p3     = signi p1 p2
          a3     = a1 || a2

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
decimalSub pr d1 d2 = decimalAdd pr d1 $ decimalRevsign d2

-- | Multiplication: /x/ × /y/
decimalMul :: DecimalBinary
decimalMul (D.Decimal r1 p1 a1) (D.Decimal r2 p2 a2)
    = Right $ D.Decimal (n3 D.%% den3) p3 a3
    where n1    = R.numerator   r1
          den1  = R.denominator r1
          n2    = R.numerator   r2
          den2  = R.denominator r2
          n3    = (n1   `div` g1) * (n2   `div` g2)
          den3  = (den2 `div` g1) * (den1 `div` g2)
          g1    = gcd n1 den2
          g2    = gcd n2 den1
          p3    = max p1 p2
          a3    = a1 || a2

-- | Division: /x/ ÷ /y/
decimalDiv :: DecimalBinary
decimalDiv dec1 dec2
    = decimalMul dec1 $ decimalRevratio dec2

decimalQuo :: DecimalBinary
decimalQuo = decimalQR quot

decimalRem :: DecimalBinary
decimalRem = decimalQR rem

decimalQR :: (D.DecimalInteger -> D.DecimalInteger -> D.DecimalInteger) -> DecimalBinary
decimalQR qr
          d1@(D.Decimal r1 p1 a1)
          d2@(D.Decimal r2 p2 a2)
    | p1 /= p2     = Msg.heteroDecimal txt1 txt2
    | n2 == 0      = Msg.divideByZero
    | otherwise    = Right $ D.Decimal (n3 D.%% 1) p1 a3
    where n1    = R.numerator   r1
          den1  = R.denominator r1
          n2    = R.numerator   r2
          den2  = R.denominator r2
          n3    = (n1 * den2) `qr` (n2 * den1)
          a3    = a1 || a2
          txt1  = D.encodeDecimal d1
          txt2  = D.encodeDecimal d2


-- ----------------------  Unary operator

-- | Invert sign of decimal
decimalRevsign :: B.Map D.Decimal
decimalRevsign (D.Decimal r p a) = D.Decimal r' p a where
    r' = - (R.numerator r) D.%% (R.denominator r)

-- | Exchange numerator and denominator
decimalRevratio :: B.Map D.Decimal
decimalRevratio (D.Decimal r p a) = D.Decimal r' p a where
    r' = R.denominator r D.%% R.numerator r

-- | Absolute value
decimalAbs :: B.Map D.Decimal
decimalAbs (D.Decimal r p a) = D.Decimal r' p a where
    r' = abs (R.numerator r) D.%% (R.denominator r)

decimalSum :: [D.Decimal] -> B.Ab D.Decimal
decimalSum = M.foldM decimalAddHigh $ D.integralDecimal (0 :: Int)

