{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

-- | Arithmetics on decimals.

module Koshucode.Baala.Type.Decimal.BinaryAb
  ( -- * Fracle
    FracleSide (..),

    -- * Binary operator
    decimalAdd, decimalSum, decimalSub,
    decimalMul, decimalDiv,
    decimalQuo, decimalRem,
  ) where

import qualified Koshucode.Baala.Base                   as B
import qualified Koshucode.Baala.Type.Decimal.Coder     as D
import qualified Koshucode.Baala.Type.Decimal.Decimal   as D
import qualified Koshucode.Baala.Type.Decimal.Fraction  as D
import qualified Koshucode.Baala.Type.Message           as Msg


-- ----------------------  Fracle

-- | Fracle-selection mode in composing decimals.
data FracleSide
    = FracleLong       -- ^ Select longer fracle (@+@)
    | FracleLeft       -- ^ Select left fracle (@.+@)
    | FracleRight      -- ^ Select right fracle (@+.@)
    | FracleStrict     -- ^ Check same fracles (@.+.@)
      deriving (Show, Ord, Eq)

constLeft :: a -> b -> a
constLeft = const

constRight :: a -> b -> b
constRight _ y = y


-- ----------------------  Type and Higher function

decimalBinAbPlus :: D.BinRatio -> D.BinAbDecimal
decimalBinAbPlus = D.decimalBinAb (+)

decimalBinAbLeft :: D.BinRatio -> D.BinAbDecimal
decimalBinAbLeft = D.decimalBinAb constLeft

decimalBinAbRight :: D.BinRatio -> D.BinAbDecimal
decimalBinAbRight = D.decimalBinAb constRight


-- ----------------------  Binary operator

-- | Addition: /x/ + /y/
decimalAdd :: FracleSide -> D.BinAbDecimal
decimalAdd FracleLong    = decimalAddLong
decimalAdd FracleLeft    = decimalBinAbLeft  (+)
decimalAdd FracleRight   = decimalBinAbRight (+)
decimalAdd FracleStrict  = decimalAddStrict

-- | Addition with 'FracleLong'.
decimalAddLong :: D.BinAbDecimal
decimalAddLong x y = Right $ x + y

decimalAddStrict :: D.BinAbDecimal
decimalAddStrict d1@D.Decimal { D.decimalFracle = f1 }
                 d2@D.Decimal { D.decimalFracle = f2 }
    | f1 == f2   = decimalAddLong d1 d2
    | otherwise  = Msg.heteroDecimal d1 d2

-- | Add all decimals.
decimalSum :: [D.Decimal] -> B.Ab D.Decimal
decimalSum = Right . foldr (+) 0

-- | Subtruction: /x/ - /y/
decimalSub :: FracleSide -> D.BinAbDecimal
decimalSub f x y = decimalAdd f x (- y)

-- | Multiplication: /x/ × /y/
decimalMul :: D.BinAbDecimal
decimalMul = decimalBinAbPlus (*)

-- | Division: /x/ ÷ /y/
decimalDiv :: D.BinAbDecimal
decimalDiv = decimalBinAbPlus (/)

-- | Quotient: integral part of /x/ ÷ /y/
decimalQuo :: D.BinAbDecimal
decimalQuo x y = Right $ D.decimalIntPart $ x / y

-- | Remainder: /y/ × fractional part of /x/ ÷ /y/
decimalRem :: D.BinAbDecimal
decimalRem x y = Right $ y * D.decimalFracPart (x / y)

