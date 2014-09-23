{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Content operators.

module Koshucode.Baala.Op.Cop.Arith
( copsArith
  -- $Operators
) where

import qualified Koshucode.Baala.Base       as B
import qualified Koshucode.Baala.Core       as C
import qualified Koshucode.Baala.Op.Message as Msg



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
    [ C.CopCalc  (C.copInfix "+")    copPlus
    , C.CopCalc  (C.copInfix "-")    copMinus
    , C.CopCalc  (C.copInfix "*")    copTimes
    , C.CopCalc  (C.copInfix "quo")  copQuo
    , C.CopCalc  (C.copInfix "rem")  copRem

    , C.CopCalc  (C.copNormal "+")     copPlus
    , C.CopCalc  (C.copNormal "-")     copMinus
    , C.CopCalc  (C.copNormal "*")     copTimes
    , C.CopCalc  (C.copNormal "quo")   copQuo
    , C.CopCalc  (C.copNormal "rem")   copRem
    , C.CopCalc  (C.copNormal "abs")   copAbs
    ]

copDec :: (Show c, C.CText c, C.CDec c) => B.Ab c -> B.Ab B.Decimal
copDec (Right c) | C.isDec  c = Right $ C.gDec c
                 | C.isText c = B.litDecimal $ C.gText c
copDec x = Msg.notNumber (show x)

copPlus :: (C.CText c, C.CDec c) => C.CopCalc c
copPlus xs = fmap C.pDec $ loop xs where
    loop [] = Right $ B.intDecimal 0
    loop (n : m) = do n' <- copDec n
                      m' <- loop m
                      B.decimalAdd n' m'

copTimes :: (C.CText c, C.CDec c) => C.CopCalc c
copTimes xs = fmap C.pDec $ loop xs where
    loop [] = Right $ B.intDecimal 1
    loop (n : m) = do n' <- copDec n
                      m' <- loop m
                      B.decimalMul n' m'

copMinus :: (C.CText c, C.CDec c) => C.CopCalc c
copMinus [a] =
    do a' <- copDec a
       Right $ C.pDec $ B.decimalRevsign a'
copMinus [a, b] =
    do a' <- copDec a
       b' <- copDec b
       c' <- B.decimalSub a' b'
       Right $ C.pDec c'
copMinus _ = Msg.unexpAttr "-"

copQuo :: (C.CText c, C.CDec c) => C.CopCalc c
copQuo [a, b] =
    do a' <- copDec a
       b' <- copDec b
       c' <- B.decimalQuo a' b'
       Right $ C.pDec c'
copQuo _ = Msg.unexpAttr "quo"

copRem :: (C.CText c, C.CDec c) => C.CopCalc c
copRem arg =
    do (ac, bc) <- C.getArg2 arg
       a <- copDec ac
       b <- copDec bc
       c <- B.decimalRem a b
       C.putDec $ c

copAbs :: (C.CList c, C.CDec c) => C.CopCalc c
copAbs [Right c] | C.isList c = Right . C.pList =<< mapM copAbs1 (C.gList c)
                 | otherwise  = copAbs1 c
copAbs _ = Msg.unexpAttr "abs"

copAbs1 :: (C.CDec c) => B.AbMap c
copAbs1 c | C.isDec c = C.putDec $ B.decimalAbs $ C.gDec c
copAbs1 _ = Msg.unexpAttr "abc"

