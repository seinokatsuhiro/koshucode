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

copsArith :: (C.CContent c) => [C.Cop c]
copsArith =
    [ C.CopCalc  (C.copInfix "+")      copPlus2
    , C.CopCalc  (C.copInfix "-")      copMinus
    , C.CopCalc  (C.copInfix "*")      copTimes
    , C.CopCalc  (C.copInfix "quo")    copQuo
    , C.CopCalc  (C.copInfix "rem")    copRem

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

copPlus2 :: (C.CDec c, C.CClock c) => C.CopCalc c
copPlus2 [Right xc, Right yc]
    | C.isDec   xc && C.isDec   yc = C.putDec   =<< B.decimalAdd (C.gDec   xc) (C.gDec   yc)
    | C.isClock xc && C.isClock yc = C.putClock =<< B.clockAdd   (C.gClock xc) (C.gClock yc)
copPlus2 _ = Msg.unexpAttr "+"

copTimes :: (C.CText c, C.CDec c) => C.CopCalc c
copTimes xs = fmap C.pDec $ loop xs where
    loop [] = Right $ B.intDecimal 1
    loop (n : m) = do n' <- copDec n
                      m' <- loop m
                      B.decimalMul n' m'

copMinus :: (C.CText c, C.CDec c, C.CClock c, C.CTime c) => C.CopCalc c
copMinus [a] =
    do a' <- copDec a
       Right $ C.pDec $ B.decimalRevsign a'
copMinus [Right xc, Right yc]
    | C.isDec   xc && C.isDec   yc = C.putDec   =<< B.decimalSub (C.gDec   xc) (C.gDec   yc)
    | C.isClock xc && C.isClock yc = C.putClock =<< B.clockSub   (C.gClock xc) (C.gClock yc)
    | C.isTime  xc && C.isTime  yc = C.putClock =<< B.timeDiff   (C.gTime  xc) (C.gTime  yc)
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

