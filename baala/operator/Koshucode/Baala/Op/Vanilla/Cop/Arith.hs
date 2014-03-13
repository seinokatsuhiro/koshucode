{-# OPTIONS_GHC -Wall #-}

-- | Content operators.

module Koshucode.Baala.Op.Vanilla.Cop.Arith
( copsArith
  -- $Operators
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C
import qualified Koshucode.Baala.Op.Vanilla.Type as Op



-- ----------------------
-- $Operators
--
--  [@+@]     Addition.
--
--  [@-@]     Subtruction.
--
--  [@*@]     Multipication.
--
--  [@quo@]   Quotient.
--
--  [@rem@]   Remainder.
--
--  [@abs@]   Absolute value.
--

copsArith :: [C.Cop Op.VContent]
copsArith =
    [ C.CopFun  "+"    copPlus
    , C.CopFun  "-"    copMinus
    , C.CopFun  "*"    copTimes
    , C.CopFun  "quo"  copQuo
    , C.CopFun  "rem"  copRem
    , C.CopFun  "abs"  copAbs
    ]

copDec :: B.Ab Op.VContent -> B.Ab B.Decimal
copDec (Right (Op.VDec  n)) = Right n
copDec (Right (Op.VText n)) = B.litDecimal n
copDec x = Left $ B.AbortSyntax [] $ B.ASNotNumber (show x)

copPlus :: Op.VCop
copPlus xs = fmap Op.VDec $ loop xs where
    loop [] = Right $ B.intDecimal 0
    loop (n : m) = do n' <- copDec n
                      m' <- loop m
                      B.decimalAdd n' m'

copTimes :: Op.VCop
copTimes xs = fmap Op.VDec $ loop xs where
    loop [] = Right $ B.intDecimal 1
    loop (n : m) = do n' <- copDec n
                      m' <- loop m
                      B.decimalMul n' m'

copMinus :: Op.VCop
copMinus [a] =
    do a' <- copDec a
       Right . Op.VDec $ B.decimalRevsign a'
copMinus [a, b] =
    do a' <- copDec a
       b' <- copDec b
       c' <- B.decimalSub a' b'
       Right . Op.VDec $ c'
copMinus _ = Left $ B.abortOperand "-"

copQuo :: Op.VCop
copQuo [a, b] =
    do a' <- copDec a
       b' <- copDec b
       c' <- B.decimalQuo a' b'
       Right . Op.VDec $ c'
copQuo _ = Left $ B.abortOperand "quo"

copRem :: Op.VCop
copRem arg =
    do (ac, bc) <- C.getArg2 arg
       a <- copDec ac
       b <- copDec bc
       c <- B.decimalRem a b
       C.putDec $ c

copAbs :: Op.VCop
copAbs [Right (Op.VList cs)] = Right . Op.VList =<< mapM copAbs1 cs
copAbs [Right c] = copAbs1 c
copAbs _ = Left $ B.abortOperand "abs"

copAbs1 :: Op.VContent -> B.Ab Op.VContent
copAbs1 (Op.VDec n) = C.putDec $ B.decimalAbs n
copAbs1 _ = Left $ B.abortOperand "abc"

