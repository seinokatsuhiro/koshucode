{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Content operators.

module Koshucode.Baala.Op.Cop.Arith
  ( copsArith
    -- $Operators
  ) where

import qualified Koshucode.Baala.Base       as B
import qualified Koshucode.Baala.Data       as D
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

copsArith :: (D.CContent c) => [D.Cop c]
copsArith =
    [ D.CopCalc  (D.copPrefix "+")     copPlus1
    , D.CopCalc  (D.copPrefix "-")     copMinus1
    , D.CopCalc  (D.copInfix  "+")     copPlus2
    , D.CopCalc  (D.copInfix  "-")     copMinus2
    , D.CopCalc  (D.copInfix  "*")     copTimes
    , D.CopCalc  (D.copInfix  "quo")   copQuo
    , D.CopCalc  (D.copInfix  "rem")   copRem

    , D.CopCalc  (D.copNormal "+")     copPlus
    , D.CopCalc  (D.copNormal "-")     copMinus2
    , D.CopCalc  (D.copNormal "*")     copTimes
    , D.CopCalc  (D.copNormal "quo")   copQuo
    , D.CopCalc  (D.copNormal "rem")   copRem
    , D.CopCalc  (D.copNormal "abs")   copAbs
    ]

copDec :: (Show c, D.CText c, D.CDec c) => B.Ab c -> B.Ab D.Decimal
copDec (Right c) | D.isDec  c = Right $ D.gDec c
                 | D.isText c = D.litDecimal $ D.gText c
copDec x = Msg.notNumber (show x)

getDecFrom :: (D.CDec c, D.CText c) => c -> B.Ab D.Decimal
getDecFrom c | D.isDec  c  = Right $ D.gDec c
             | D.isText c  = D.litDecimal $ D.gText c
             | otherwise   = Right $ D.intDecimal 0

copPlus :: (D.CText c, D.CDec c) => D.CopCalc c
copPlus xs = fmap D.pDec $ loop xs where
    loop [] = Right $ D.intDecimal 0
    loop (n : m) = do n' <- copDec n
                      m' <- loop m
                      D.decimalAdd n' m'

copPlus2 :: (D.CDec c, D.CClock c, D.CTime c) => D.CopCalc c
copPlus2 [Right xc, Right yc]
    | D.isDec   xc && D.isDec   yc = D.putDec   =<< D.decimalAdd   (D.gDec   xc) (D.gDec   yc)
    | D.isClock xc && D.isClock yc = D.putClock =<< D.clockAdd     (D.gClock xc) (D.gClock yc)
    | D.isTime  xc && D.isClock yc = D.putTime  =<< D.timeAddClock (D.gClock yc) (D.gTime  xc) 
    | D.isClock xc && D.isTime  yc = D.putTime  =<< D.timeAddClock (D.gClock xc) (D.gTime  yc)
copPlus2 _ = Msg.unexpAttr "+"

copPlus1 :: (D.CDec c, D.CClock c, D.CTime c) => D.CopCalc c
copPlus1 [Right x] | D.isDec x = Right x
copPlus1 _ = Msg.unexpAttr "+"

copTimes :: (D.CText c, D.CDec c) => D.CopCalc c
copTimes xs = fmap D.pDec $ loop xs where
    loop [] = Right $ D.intDecimal 1
    loop (n : m) = do n' <- copDec n
                      m' <- loop m
                      D.decimalMul n' m'

copMinus2 :: (D.CText c, D.CDec c, D.CClock c, D.CTime c) => D.CopCalc c
copMinus2 [Right xc, Right yc]
    | D.isDec   xc && D.isDec   yc = D.putDec   =<< D.decimalSub (D.gDec   xc) (D.gDec   yc)
    | D.isClock xc && D.isClock yc = D.putClock =<< D.clockSub   (D.gClock xc) (D.gClock yc)
    | D.isTime  xc && D.isTime  yc = D.putClock =<< D.timeDiff   (D.gTime  xc) (D.gTime  yc)
copMinus2 _ = Msg.unexpAttr "-"

copMinus1 :: (D.CDec c) => D.CopCalc c
copMinus1 [Right x] | D.isDec x = D.putDec $ D.decimalRevsign (D.gDec x)
copMinus1 _ = Msg.unexpAttr "-"

copQuo :: (D.CText c, D.CDec c) => D.CopCalc c
copQuo arg =
    do (ac, bc) <- D.getRightArg2 arg
       a <- getDecFrom ac
       b <- getDecFrom bc
       c <- D.decimalQuo a b
       D.putDec c

copRem :: (D.CText c, D.CDec c) => D.CopCalc c
copRem arg =
    do (ac, bc) <- D.getRightArg2 arg
       a <- getDecFrom ac
       b <- getDecFrom bc
       c <- D.decimalRem a b
       D.putDec $ c

copAbs :: (D.CList c, D.CDec c) => D.CopCalc c
copAbs [Right c] | D.isList c = Right . D.pList =<< mapM copAbs1 (D.gList c)
                 | otherwise  = copAbs1 c
copAbs _ = Msg.unexpAttr "abs"

copAbs1 :: (D.CDec c) => B.AbMap c
copAbs1 c | D.isDec c = D.putDec $ D.decimalAbs $ D.gDec c
copAbs1 _ = Msg.unexpAttr "abc"

