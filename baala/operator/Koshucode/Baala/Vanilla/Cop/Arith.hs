{-# OPTIONS_GHC -Wall #-}

{-| Content operators. -}

module Koshucode.Baala.Vanilla.Cop.Arith
( copsArith
  -- $Operators
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C
import qualified Koshucode.Baala.Vanilla.Type as V



-- ----------------------
{- $Operators

 [@+@]     Addition.

 [@-@]     Subtruction.

 [@*@]     Multipication.

 [@quo@]   Quotient.

 [@rem@]   Remainder.

 [@abs@]   Absolute value.

-}

copsArith :: [C.Cop V.VContent]
copsArith =
    [ C.CopFun  "+"    copPlus
    , C.CopFun  "-"    copMinus
    , C.CopFun  "*"    copTimes
    , C.CopFun  "quo"  copQuo
    , C.CopFun  "rem"  copRem
    , C.CopFun  "abs"  copAbs
    ]

copDec :: B.Ab V.VContent -> B.Ab B.Decimal
copDec (Right (V.VDec  n)) = Right n
copDec (Right (V.VText n)) = B.litDecimal n
copDec x = Left $ B.AbortSyntax [] $ B.ASNotNumber (show x)

copPlus :: V.VCop
copPlus xs = fmap V.VDec $ loop xs where
    loop [] = Right $ B.intDecimal 0
    loop (n : m) = do n' <- copDec n
                      m' <- loop m
                      B.decimalAdd n' m'

copTimes :: V.VCop
copTimes xs = fmap V.VDec $ loop xs where
    loop [] = Right $ B.intDecimal 1
    loop (n : m) = do n' <- copDec n
                      m' <- loop m
                      B.decimalMul n' m'

copMinus :: V.VCop
copMinus [a] =
    do a' <- copDec a
       Right . V.VDec $ B.decimalRevsign a'
copMinus [a, b] =
    do a' <- copDec a
       b' <- copDec b
       c' <- B.decimalSub a' b'
       Right . V.VDec $ c'
copMinus _ = Left $ B.abortOperand "-"

copQuo :: V.VCop
copQuo [a, b] =
    do a' <- copDec a
       b' <- copDec b
       c' <- B.decimalQuo a' b'
       Right . V.VDec $ c'
copQuo _ = Left $ B.abortOperand "quo"

copRem :: V.VCop
copRem arg =
    do (ac, bc) <- C.getArg2 arg
       a <- copDec ac
       b <- copDec bc
       c <- B.decimalRem a b
       C.putDec $ c

copAbs :: V.VCop
copAbs [Right (V.VList cs)] = Right . V.VList =<< mapM copAbs1 cs
copAbs [Right c] = copAbs1 c
copAbs _ = Left $ B.abortOperand "abs"

copAbs1 :: V.VContent -> B.Ab V.VContent
copAbs1 (V.VDec n) = C.putDec $ B.decimalAbs n
copAbs1 _ = Left $ B.abortOperand "abc"

