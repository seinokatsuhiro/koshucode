{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Content operators.

module Koshucode.Baala.Op.Content.Arith
( copsArith
  -- $Operators
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C



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

copsArith :: (C.CDec c, C.CList c, C.CText c) => [C.Cop c]
copsArith =
    [ C.CopFun  "+"    copPlus
    , C.CopFun  "-"    copMinus
    , C.CopFun  "*"    copTimes
    , C.CopFun  "quo"  copQuo
    , C.CopFun  "rem"  copRem
    , C.CopFun  "abs"  copAbs
    ]

copDec :: (Show c, C.CText c, C.CDec c) => B.Ab c -> B.Ab B.Decimal
copDec (Right c) | C.isDec  c = Right $ C.gDec c
                 | C.isText c = B.litDecimal $ C.gText c
copDec x = Left $ B.AbortSyntax [] $ B.ASNotNumber (show x)

copPlus :: (C.CText c, C.CDec c) => C.CopFun c
copPlus xs = fmap C.pDec $ loop xs where
    loop [] = Right $ B.intDecimal 0
    loop (n : m) = do n' <- copDec n
                      m' <- loop m
                      B.decimalAdd n' m'

copTimes :: (C.CText c, C.CDec c) => C.CopFun c
copTimes xs = fmap C.pDec $ loop xs where
    loop [] = Right $ B.intDecimal 1
    loop (n : m) = do n' <- copDec n
                      m' <- loop m
                      B.decimalMul n' m'

copMinus :: (C.CText c, C.CDec c) => C.CopFun c
copMinus [a] =
    do a' <- copDec a
       Right $ C.pDec $ B.decimalRevsign a'
copMinus [a, b] =
    do a' <- copDec a
       b' <- copDec b
       c' <- B.decimalSub a' b'
       Right $ C.pDec c'
copMinus _ = Left $ B.abortOperand "-"

copQuo :: (C.CText c, C.CDec c) => C.CopFun c
copQuo [a, b] =
    do a' <- copDec a
       b' <- copDec b
       c' <- B.decimalQuo a' b'
       Right $ C.pDec c'
copQuo _ = Left $ B.abortOperand "quo"

copRem :: (C.CText c, C.CDec c) => C.CopFun c
copRem arg =
    do (ac, bc) <- C.getArg2 arg
       a <- copDec ac
       b <- copDec bc
       c <- B.decimalRem a b
       C.putDec $ c

-- copAbs :: Op.VCop
-- copAbs [Right (Op.VList cs)] = Right . Op.VList =<< mapM copAbs1 cs
-- copAbs [Right c] = copAbs1 c
-- copAbs _ = Left $ B.abortOperand "abs"

copAbs :: (C.CList c, C.CDec c) => C.CopFun c
copAbs [Right c] | C.isList c = Right . C.pList =<< mapM copAbs1 (C.gList c)
                 | otherwise  = copAbs1 c
copAbs _ = Left $ B.abortOperand "abs"

copAbs1 :: (C.CDec c) => B.AbMap c
copAbs1 c | C.isDec c = C.putDec $ B.decimalAbs $ C.gDec c
copAbs1 _ = Left $ B.abortOperand "abc"

