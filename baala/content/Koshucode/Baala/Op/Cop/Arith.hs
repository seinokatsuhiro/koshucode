{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Content operators.

module Koshucode.Baala.Op.Cop.Arith
  ( copsArith
    -- $Operators
  ) where

import qualified Koshucode.Baala.Base       as B
import qualified Koshucode.Baala.Data       as C
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
    [ C.CopCalc  (C.copPrefix "+")     copPlus1
    , C.CopCalc  (C.copPrefix "-")     copMinus1
    , C.CopCalc  (C.copInfix  "+")     copPlus2
    , C.CopCalc  (C.copInfix  "-")     copMinus2
    , C.CopCalc  (C.copInfix  "*")     copTimes
    , C.CopCalc  (C.copInfix  "quo")   copQuo
    , C.CopCalc  (C.copInfix  "rem")   copRem

    , C.CopCalc  (C.copNormal "+")     copPlus
    , C.CopCalc  (C.copNormal "-")     copMinus2
    , C.CopCalc  (C.copNormal "*")     copTimes
    , C.CopCalc  (C.copNormal "quo")   copQuo
    , C.CopCalc  (C.copNormal "rem")   copRem
    , C.CopCalc  (C.copNormal "abs")   copAbs
    ]

copDec :: (Show c, C.CText c, C.CDec c) => B.Ab c -> B.Ab B.Decimal
copDec (Right c) | C.isDec  c = Right $ C.gDec c
                 | C.isText c = B.litDecimal $ C.gText c
copDec x = Msg.notNumber (show x)

getDecFrom :: (C.CDec c, C.CText c) => c -> B.Ab B.Decimal
getDecFrom c | C.isDec  c  = Right $ C.gDec c
             | C.isText c  = B.litDecimal $ C.gText c
             | otherwise   = Right $ B.intDecimal 0

copPlus :: (C.CText c, C.CDec c) => C.CopCalc c
copPlus xs = fmap C.pDec $ loop xs where
    loop [] = Right $ B.intDecimal 0
    loop (n : m) = do n' <- copDec n
                      m' <- loop m
                      B.decimalAdd n' m'

copPlus2 :: (C.CDec c, C.CClock c, C.CTime c) => C.CopCalc c
copPlus2 [Right xc, Right yc]
    | C.isDec   xc && C.isDec   yc = C.putDec   =<< B.decimalAdd   (C.gDec   xc) (C.gDec   yc)
    | C.isClock xc && C.isClock yc = C.putClock =<< B.clockAdd     (C.gClock xc) (C.gClock yc)
    | C.isTime  xc && C.isClock yc = C.putTime  =<< B.timeAddClock (C.gClock yc) (C.gTime  xc) 
    | C.isClock xc && C.isTime  yc = C.putTime  =<< B.timeAddClock (C.gClock xc) (C.gTime  yc)
copPlus2 _ = Msg.unexpAttr "+"

copPlus1 :: (C.CDec c, C.CClock c, C.CTime c) => C.CopCalc c
copPlus1 [Right x] | C.isDec x = Right x
copPlus1 _ = Msg.unexpAttr "+"

copTimes :: (C.CText c, C.CDec c) => C.CopCalc c
copTimes xs = fmap C.pDec $ loop xs where
    loop [] = Right $ B.intDecimal 1
    loop (n : m) = do n' <- copDec n
                      m' <- loop m
                      B.decimalMul n' m'

copMinus2 :: (C.CText c, C.CDec c, C.CClock c, C.CTime c) => C.CopCalc c
copMinus2 [Right xc, Right yc]
    | C.isDec   xc && C.isDec   yc = C.putDec   =<< B.decimalSub (C.gDec   xc) (C.gDec   yc)
    | C.isClock xc && C.isClock yc = C.putClock =<< B.clockSub   (C.gClock xc) (C.gClock yc)
    | C.isTime  xc && C.isTime  yc = C.putClock =<< B.timeDiff   (C.gTime  xc) (C.gTime  yc)
copMinus2 _ = Msg.unexpAttr "-"

copMinus1 :: (C.CDec c) => C.CopCalc c
copMinus1 [Right x] | C.isDec x = C.putDec $ B.decimalRevsign (C.gDec x)
copMinus1 _ = Msg.unexpAttr "-"

copQuo :: (C.CText c, C.CDec c) => C.CopCalc c
copQuo arg =
    do (ac, bc) <- C.getRightArg2 arg
       a <- getDecFrom ac
       b <- getDecFrom bc
       c <- B.decimalQuo a b
       C.putDec c

copRem :: (C.CText c, C.CDec c) => C.CopCalc c
copRem arg =
    do (ac, bc) <- C.getRightArg2 arg
       a <- getDecFrom ac
       b <- getDecFrom bc
       c <- B.decimalRem a b
       C.putDec $ c

copAbs :: (C.CList c, C.CDec c) => C.CopCalc c
copAbs [Right c] | C.isList c = Right . C.pList =<< mapM copAbs1 (C.gList c)
                 | otherwise  = copAbs1 c
copAbs _ = Msg.unexpAttr "abs"

copAbs1 :: (C.CDec c) => B.AbMap c
copAbs1 c | C.isDec c = C.putDec $ B.decimalAbs $ C.gDec c
copAbs1 _ = Msg.unexpAttr "abc"

