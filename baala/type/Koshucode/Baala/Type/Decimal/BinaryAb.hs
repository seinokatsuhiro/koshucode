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
import qualified Koshucode.Baala.Type.Decimal.Coder     as T
import qualified Koshucode.Baala.Type.Decimal.Decimal   as T
import qualified Koshucode.Baala.Type.Decimal.Fraction  as T
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

decimalBinAbPlus :: T.BinRatio -> T.BinAbDecimal
decimalBinAbPlus = T.decimalBinAb (+)

decimalBinAbLeft :: T.BinRatio -> T.BinAbDecimal
decimalBinAbLeft = T.decimalBinAb constLeft

decimalBinAbRight :: T.BinRatio -> T.BinAbDecimal
decimalBinAbRight = T.decimalBinAb constRight


-- ----------------------  Binary operator

-- | Addition: /x/ + /y/
decimalAdd :: FracleSide -> T.BinAbDecimal
decimalAdd FracleLong    = decimalAddLong
decimalAdd FracleLeft    = decimalBinAbLeft  (+)
decimalAdd FracleRight   = decimalBinAbRight (+)
decimalAdd FracleStrict  = decimalAddStrict

-- | Addition with 'FracleLong'.
decimalAddLong :: T.BinAbDecimal
decimalAddLong x y = Right $ x + y

decimalAddStrict :: T.BinAbDecimal
decimalAddStrict d1@T.Decimal { T.decimalFracle = f1 }
                 d2@T.Decimal { T.decimalFracle = f2 }
    | f1 == f2   = decimalAddLong d1 d2
    | otherwise  = Msg.heteroDecimal d1 d2

-- | Add all decimals.
decimalSum :: [T.Decimal] -> B.Ab T.Decimal
decimalSum = Right . foldr (+) 0

-- | Subtruction: /x/ - /y/
decimalSub :: FracleSide -> T.BinAbDecimal
decimalSub f x y = decimalAdd f x (- y)

-- | Multiplication: /x/ × /y/
decimalMul :: T.BinAbDecimal
decimalMul = decimalBinAbPlus (*)

-- | Division: /x/ ÷ /y/
decimalDiv :: T.BinAbDecimal
decimalDiv = decimalBinAbPlus (/)

-- | Quotient: integral part of /x/ ÷ /y/
decimalQuo :: T.BinAbDecimal
decimalQuo x y = Right $ T.decimalIntPart $ x / y

-- | Remainder: /y/ × fractional part of /x/ ÷ /y/
decimalRem :: T.BinAbDecimal
decimalRem x y = Right $ y * T.decimalFracPart (x / y)

