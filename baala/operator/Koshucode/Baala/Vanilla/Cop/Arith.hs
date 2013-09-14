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
    , C.namedEager  "quo"  copQuo
    , C.namedEager  "rem"  copRem
    , C.namedEager  "abs"  copAbs
    ]

copDec :: VContent -> B.Ab B.Decimal
copDec (VDec  n) = Right n
copDec (VText n) = B.litDecimal n
copDec x = Left $ B.AbortNotNumber (show x)

copPlus :: VCop
copPlus xs = fmap VDec $ loop xs where
    loop [] = Right $ B.intDecimal 0
    loop (n : m) = do n' <- copDec n
                      m' <- loop m
                      B.decimalAdd n' m'

copTimes :: VCop
copTimes xs = fmap VDec $ loop xs where
    loop [] = Right $ B.intDecimal 1
    loop (n : m) = do n' <- copDec n
                      m' <- loop m
                      B.decimalMul n' m'

copMinus :: VCop
copMinus [a] =
    do a' <- copDec a
       Right . VDec $ B.decimalRevsign a'
copMinus [a, b] =
    do a' <- copDec a
       b' <- copDec b
       c' <- B.decimalSub a' b'
       Right . VDec $ c'
copMinus _ = Left $ B.AbortMalformedOperand "-"

copQuo :: VCop
copQuo [a, b] =
    do a' <- copDec a
       b' <- copDec b
       c' <- B.decimalQuo a' b'
       Right . VDec $ c'
copQuo _ = Left $ B.AbortMalformedOperand "quo"

copRem :: VCop
copRem [a, b] =
    do a' <- copDec a
       b' <- copDec b
       c' <- B.decimalRem a' b'
       Right . VDec $ c'
copRem _ = Left $ B.AbortMalformedOperand "rem"

copAbs :: VCop
copAbs [VList cs] = Right . VList =<< mapM copAbs1 cs
copAbs [c] = copAbs1 c
copAbs _ = Left B.AbortUnmatchArity

copAbs1 :: VContent -> B.Ab VContent
copAbs1 (VDec n) = Right . VDec $ B.decimalAbs n
copAbs1 _ = Left B.AbortUnmatchArity

-- let tree = treeG . tokenTrees . tokens
-- let Right e2 = vanillaContent [] $ tree "1 = 1 and 2 = 3"
-- let Right e2 = vanillaContent [] $ tree "(+ (int 1) (int 2) (int 3))"
-- let Right e2 = vanillaContent [] $ tree "(+ 1 2 (+ 3 4 5) 6 7 8 9)"
-- let Right e2 = vanillaContent [] $ tree "1 + 2 + 3 + 4 + 5"
-- let Right e3 = runCox (e2 $ Relhead []) []
-- let Left  e3 = runCox e2 []

