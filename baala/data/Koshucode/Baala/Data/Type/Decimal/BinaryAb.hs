{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

-- | Arithmetics on decimals.

module Koshucode.Baala.Data.Type.Decimal.BinaryAb
  ( -- * Precision
    PrecisionSide (..),

    -- * Binary operator
    decimalAdd, decimalAddHigh,
    decimalSum,
    decimalSub,
    decimalMul, decimalDiv,
    decimalQuo, decimalRem,
  ) where

import qualified Control.Monad                               as M
import qualified Koshucode.Baala.Base                        as B
import qualified Koshucode.Baala.Data.Type.Decimal.Binary    as D
import qualified Koshucode.Baala.Data.Type.Decimal.Coder     as D
import qualified Koshucode.Baala.Data.Type.Decimal.Decimal   as D
import qualified Koshucode.Baala.Data.Type.Decimal.Fraction  as D
import qualified Koshucode.Baala.Data.Type.Decimal.Instance  ()
import qualified Koshucode.Baala.Data.Type.Message           as Msg

data PrecisionSide
    = PrecisionHigh       -- ^ Select high precision
    | PrecisionLeft       -- ^ Select left precision
    | PrecisionRight      -- ^ Select right precision
    | PrecisionStrict     -- ^ Check same precision
      deriving (Show, Ord, Eq)

constLeft :: a -> b -> a
constLeft = const

constRight :: a -> b -> b
constRight _ y = y


-- ----------------------  Type and Higher function

decimalBinAbMax :: D.BinRatio -> D.BinAbDecimal
decimalBinAbMax = D.decimalBinAb max

decimalBinAbPlus :: D.BinRatio -> D.BinAbDecimal
decimalBinAbPlus = D.decimalBinAb (+)

decimalBinAbLeft :: D.BinRatio -> D.BinAbDecimal
decimalBinAbLeft = D.decimalBinAb constLeft

decimalBinAbRight :: D.BinRatio -> D.BinAbDecimal
decimalBinAbRight = D.decimalBinAb constRight


-- ----------------------  Binary operator

-- | Addition: /x/ + /y/
decimalAdd :: PrecisionSide -> D.BinAbDecimal
decimalAdd PrecisionHigh    = decimalBinAbMax   (+)
decimalAdd PrecisionLeft    = decimalBinAbLeft  (+)
decimalAdd PrecisionRight   = decimalBinAbRight (+)
decimalAdd PrecisionStrict  = decimalAddStrict

-- | Addition with 'PrecisionHigh'.
decimalAddHigh :: D.BinAbDecimal
decimalAddHigh = decimalBinAbMax (+)

decimalAddStrict :: D.BinAbDecimal
decimalAddStrict d1@D.Decimal { D.decimalFracl = p1 }
                 d2@D.Decimal { D.decimalFracl = p2 }
    | p1 == p2   = decimalAddHigh d1 d2
    | otherwise  = Msg.heteroDecimal txt1 txt2
    where txt1   = D.encodeDecimal d1
          txt2   = D.encodeDecimal d2

-- | Add all decimals.
decimalSum :: [D.Decimal] -> B.Ab D.Decimal
decimalSum = M.foldM decimalAddHigh D.decimal0

-- | Subtruction: /x/ - /y/
decimalSub :: PrecisionSide -> D.BinAbDecimal
decimalSub pr x y = decimalAdd pr x $ negate y

-- | Multiplication: /x/ × /y/
decimalMul :: D.BinAbDecimal
decimalMul = decimalBinAbPlus (*)

-- | Division: /x/ ÷ /y/
decimalDiv :: D.BinAbDecimal
decimalDiv = decimalBinAbPlus (/)

-- | Quotient: integral part of /x/ ÷ /y/
decimalQuo :: D.BinAbDecimal
decimalQuo x y = do z <- decimalDiv x y
                    Right $ D.decimalIntPart z

-- | Remainder.
decimalRem :: D.BinAbDecimal
decimalRem x y = do z <- decimalDiv x y
                    r <- y `decimalMul` D.decimalFracPart z
                    Right r

