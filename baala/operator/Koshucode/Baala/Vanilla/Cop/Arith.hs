{-# OPTIONS_GHC -Wall #-}

{-| Content operators. -}

module Koshucode.Baala.Vanilla.Cop.Arith
( copsArith
  -- $Operators
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C
import Koshucode.Baala.Vanilla.Type



-- ----------------------
{- $Operators

 [@+@]     Addition.

 [@-@]     Subtruction.

 [@*@]     Multipication.

 [@abs@]   Absolute value.

-}

copsArith :: [B.Named (C.Cop VContent)]
copsArith =
    [ C.namedEager  "+"    copPlus
    , C.namedEager  "*"    copTimes
    , C.namedEager  "-"    copMinus
    , C.namedEager  "abs"  copAbs
    ]

copDec :: VContent -> B.AbOr C.Decimal
copDec (VDec  n) = Right n
copDec (VText n) = C.litDecimal n
copDec x = Left $ B.AbortNotNumber (show x)

copPlus :: VCop
copPlus xs = fmap VDec $ loop xs where
    loop [] = Right $ C.intDecimal 0
    loop (n : m) = do n' <- copDec n
                      m' <- loop m
                      C.decimalAdd n' m'

copTimes :: VCop
copTimes xs = fmap VDec $ loop xs where
    loop [] = Right $ C.intDecimal 1
    loop (n : m) = do n' <- copDec n
                      m' <- loop m
                      C.decimalMul n' m'

copMinus :: VCop
copMinus [a] =
    do a' <- copDec a
       Right . VDec $ C.decimalRevsign a'
copMinus [a, b] =
    do a' <- copDec a
       b' <- copDec b
       c' <- C.decimalSub a' b'
       Right . VDec $ c'
copMinus _ = Left $ B.AbortMalformedOperand "-"

copAbs :: VCop
copAbs [VList cs] = Right . VList =<< mapM copAbs1 cs
copAbs [c] = copAbs1 c
copAbs _ = Left B.AbortUnmatchArity

copAbs1 :: VContent -> B.AbOr VContent
copAbs1 (VDec n) = Right . VDec $ C.decimalAbs n
copAbs1 _ = Left B.AbortUnmatchArity

-- let tree = singleTree . tokenTrees . tokens
-- let Right e2 = vanillaContent [] $ tree "1 = 1 and 2 = 3"
-- let Right e2 = vanillaContent [] $ tree "(+ (int 1) (int 2) (int 3))"
-- let Right e2 = vanillaContent [] $ tree "(+ 1 2 (+ 3 4 5) 6 7 8 9)"
-- let Right e2 = vanillaContent [] $ tree "1 + 2 + 3 + 4 + 5"
-- let Right e3 = runCox (e2 $ Relhead []) []
-- let Left  e3 = runCox e2 []

