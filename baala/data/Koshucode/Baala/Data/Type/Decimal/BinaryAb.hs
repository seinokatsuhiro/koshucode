{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

-- | Arithmetics on decimals.

module Koshucode.Baala.Data.Type.Decimal.BinaryAb
  ( -- * Fracl
    FraclSide (..),

    -- * Binary operator
    decimalAdd, decimalSum, decimalSub,
    decimalMul, decimalDiv,
    decimalQuo, decimalRem,
  ) where

import qualified Koshucode.Baala.Base                        as B
import qualified Koshucode.Baala.Data.Type.Decimal.Binary    as D
import qualified Koshucode.Baala.Data.Type.Decimal.Coder     as D
import qualified Koshucode.Baala.Data.Type.Decimal.Decimal   as D
import qualified Koshucode.Baala.Data.Type.Decimal.Fraction  as D
import qualified Koshucode.Baala.Data.Type.Decimal.Instance  ()
import qualified Koshucode.Baala.Data.Type.Message           as Msg


-- ----------------------  Fracl

data FraclSide
    = FraclLong       -- ^ Select longer fracl
    | FraclLeft       -- ^ Select left fracl
    | FraclRight      -- ^ Select right fracl
    | FraclStrict     -- ^ Check same fracls
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
decimalAdd :: FraclSide -> D.BinAbDecimal
decimalAdd FraclLong    = decimalAddLong
decimalAdd FraclLeft    = decimalBinAbLeft  (+)
decimalAdd FraclRight   = decimalBinAbRight (+)
decimalAdd FraclStrict  = decimalAddStrict

-- | Addition with 'FraclLong'.
decimalAddLong :: D.BinAbDecimal
decimalAddLong x y = Right $ x + y

decimalAddStrict :: D.BinAbDecimal
decimalAddStrict d1@D.Decimal { D.decimalFracl = f1 }
                 d2@D.Decimal { D.decimalFracl = f2 }
    | f1 == f2   = decimalAddLong d1 d2
    | otherwise  = Msg.heteroDecimal txt1 txt2
    where txt1   = D.encodeDecimal d1
          txt2   = D.encodeDecimal d2

-- | Add all decimals.
decimalSum :: [D.Decimal] -> B.Ab D.Decimal
decimalSum = Right . foldr (+) 0

-- | Subtruction: /x/ - /y/
decimalSub :: FraclSide -> D.BinAbDecimal
decimalSub fracl x y = decimalAdd fracl x (- y)

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

