{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

-- | Arithmetics on decimals.

module Koshucode.Baala.Data.Type.Decimal.Arithmetic
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

decimalBinAbMax :: D.Bin D.DecimalRatio -> D.DecimalBinAb
decimalBinAbMax = D.decimalBinAb max

decimalBinAbPlus :: D.Bin D.DecimalRatio -> D.DecimalBinAb
decimalBinAbPlus = D.decimalBinAb (+)

decimalBinAbLeft :: D.Bin D.DecimalRatio -> D.DecimalBinAb
decimalBinAbLeft = D.decimalBinAb constLeft

decimalBinAbRight :: D.Bin D.DecimalRatio -> D.DecimalBinAb
decimalBinAbRight = D.decimalBinAb constRight


-- ----------------------  Binary operator

-- | Addition: /x/ + /y/
decimalAdd :: PrecisionSide -> D.DecimalBinAb
decimalAdd PrecisionHigh    = decimalBinAbMax   (+)
decimalAdd PrecisionLeft    = decimalBinAbLeft  (+)
decimalAdd PrecisionRight   = decimalBinAbRight (+)
decimalAdd PrecisionStrict  = decimalAddStrict

-- | Addition with 'PrecisionHigh'.
decimalAddHigh :: D.DecimalBinAb
decimalAddHigh = decimalBinAbMax (+)

decimalAddStrict :: D.DecimalBinAb
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
decimalSub :: PrecisionSide -> D.DecimalBinAb
decimalSub pr x y = decimalAdd pr x $ negate y

-- | Multiplication: /x/ × /y/
decimalMul :: D.DecimalBinAb
decimalMul = decimalBinAbPlus (*)

-- | Division: /x/ ÷ /y/
decimalDiv :: D.DecimalBinAb
decimalDiv = decimalBinAbPlus (/)

-- | Quotient: integral part of /x/ ÷ /y/
decimalQuo :: D.DecimalBinAb
decimalQuo x y = do z <- decimalDiv x y
                    Right $ D.decimalIntPart z

-- | Remainder.
decimalRem :: D.DecimalBinAb
decimalRem x y = do z <- decimalDiv x y
                    r <- y `decimalMul` D.decimalFracPart z
                    Right r

