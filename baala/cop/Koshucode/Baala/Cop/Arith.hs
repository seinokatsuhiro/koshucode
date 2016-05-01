{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Content operators.

module Koshucode.Baala.Cop.Arith
  ( copsArith
    -- $Operators
  ) where

import qualified Koshucode.Baala.Base        as B
import qualified Koshucode.Baala.Data        as D
import qualified Koshucode.Baala.Cop.Message as Msg



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
    [ D.CopCalc  (D.copPrefix "+")          copPlus1
    , D.CopCalc  (D.copPrefix "-")          copMinus1

    , D.CopCalc  (D.copNormal "abs")        copAbs
    , D.CopCalc  (D.copNormal "int-part")   copIntPart
    , D.CopCalc  (D.copNormal "frac-part")  copFracPart

    -- ----------------------  fractional

    , D.CopCalc  (D.copNormal "round")           copRound
    , D.CopCalc  (D.copNormal "round-at")        copRoundAt
    , D.CopCalc  (D.copNormal "round-per")       copRoundPer

    , D.CopCalc  (D.copNormal "round-even")      copRoundEven
    , D.CopCalc  (D.copNormal "round-even-at")   copRoundEvenAt
    , D.CopCalc  (D.copNormal "round-even-per")  copRoundEvenPer

    , D.CopCalc  (D.copNormal "trunc")           copTrunc
    , D.CopCalc  (D.copNormal "trunc-at")        copTruncAt
    , D.CopCalc  (D.copNormal "trunc-per")       copTruncPer

    , D.CopCalc  (D.copNormal "floor")           copFloor
    , D.CopCalc  (D.copNormal "floor-at")        copFloorAt
    , D.CopCalc  (D.copNormal "floor-per")       copFloorPer

    , D.CopCalc  (D.copNormal "ceil")            copCeil
    , D.CopCalc  (D.copNormal "ceil-at")         copCeilAt
    , D.CopCalc  (D.copNormal "ceil-per")        copCeilPer

    -- ----------------------  add and subtract

    , D.CopCalc  (D.copInfix  "+")    $ copPlus2 D.FraclLong
    , D.CopCalc  (D.copInfix  ".+")   $ copPlus2 D.FraclLeft
    , D.CopCalc  (D.copInfix  "+.")   $ copPlus2 D.FraclRight
    , D.CopCalc  (D.copInfix  ".+.")  $ copPlus2 D.FraclStrict

    , D.CopCalc  (D.copNormal "+")    $ copPlus D.FraclLong
    , D.CopCalc  (D.copNormal ".+")   $ copPlus D.FraclLeft
    , D.CopCalc  (D.copNormal "+.")   $ copPlus D.FraclRight
    , D.CopCalc  (D.copNormal ".+.")  $ copPlus D.FraclStrict

    , D.CopCalc  (D.copInfix  "-")    $ copMinus2 D.FraclLong
    , D.CopCalc  (D.copInfix  ".-")   $ copMinus2 D.FraclLeft
    , D.CopCalc  (D.copInfix  "-.")   $ copMinus2 D.FraclRight
    , D.CopCalc  (D.copInfix  ".-.")  $ copMinus2 D.FraclStrict

    , D.CopCalc  (D.copNormal "-")    $ copMinus2 D.FraclLong
    , D.CopCalc  (D.copNormal ".-")   $ copMinus2 D.FraclLeft
    , D.CopCalc  (D.copNormal "-.")   $ copMinus2 D.FraclRight
    , D.CopCalc  (D.copNormal ".-.")  $ copMinus2 D.FraclStrict

    -- ----------------------  multiply and divide

    , D.CopCalc  (D.copInfix  "*")      copMul
    , D.CopCalc  (D.copNormal "*")      copMul

    , D.CopCalc  (D.copInfix  "div")    copDiv
    , D.CopCalc  (D.copInfix  "quo")    copQuo
    , D.CopCalc  (D.copInfix  "rem")    copRem

    , D.CopCalc  (D.copNormal "recip")  copRecip
    , D.CopCalc  (D.copNormal "div")    copDiv
    , D.CopCalc  (D.copNormal "quo")    copQuo
    , D.CopCalc  (D.copNormal "rem")    copRem
    ]


copDec :: (Show c, D.CText c, D.CDec c) => B.Ab c -> B.Ab D.Decimal
copDec (Right c) | D.isDec  c = Right $ D.gDec c
                 | D.isText c = D.decodeDecimal $ D.gText c
copDec x = Msg.notNumber (show x)

getDecFrom :: (D.CDec c, D.CText c) => c -> B.Ab D.Decimal
getDecFrom c | D.isDec  c  = Right $ D.gDec c
             | D.isText c  = D.decodeDecimal $ D.gText c
             | otherwise   = Right 0

getDec1 :: (D.CDec c, D.CText c) => [B.Ab c] -> B.Ab D.Decimal
getDec1 arg =
    do x' <- D.getRightArg1 arg
       getDecFrom x'

getDec2 :: (D.CDec c, D.CText c) => [B.Ab c] -> B.Ab (D.Decimal, D.Decimal)
getDec2 arg =
    do (x', y') <- D.getRightArg2 arg
       x <- getDecFrom x'
       y <- getDecFrom y'
       Right (x, y)

copIntPart :: (D.CText c, D.CDec c) => D.CopCalc c
copIntPart arg =
    do a <- getDec1 arg
       D.putDec $ D.decimalIntPart a

copFracPart :: (D.CText c, D.CDec c) => D.CopCalc c
copFracPart arg =
    do a <- getDec1 arg
       D.putDec $ D.decimalFracPart a

copAbs :: (D.CList c, D.CDec c) => D.CopCalc c
copAbs [Right c] | D.isList c = Right . D.pList =<< mapM copAbs1 (D.gList c)
                 | otherwise  = copAbs1 c
copAbs _ = Msg.unexpAttr "abs"

copAbs1 :: (D.CDec c) => B.AbMap c
copAbs1 c | D.isDec c = D.putDec $ abs $ D.gDec c
copAbs1 _ = Msg.unexpAttr "abc"

copRound         :: (D.CText c, D.CDec c) => D.CopCalc c
copRoundAt       :: (D.CText c, D.CDec c) => D.CopCalc c
copRoundPer      :: (D.CText c, D.CDec c) => D.CopCalc c

copRoundEven     :: (D.CText c, D.CDec c) => D.CopCalc c
copRoundEvenAt   :: (D.CText c, D.CDec c) => D.CopCalc c
copRoundEvenPer  :: (D.CText c, D.CDec c) => D.CopCalc c

copTrunc         :: (D.CText c, D.CDec c) => D.CopCalc c
copTruncAt       :: (D.CText c, D.CDec c) => D.CopCalc c
copTruncPer      :: (D.CText c, D.CDec c) => D.CopCalc c

copFloor         :: (D.CText c, D.CDec c) => D.CopCalc c
copFloorAt       :: (D.CText c, D.CDec c) => D.CopCalc c
copFloorPer      :: (D.CText c, D.CDec c) => D.CopCalc c

copCeil          :: (D.CText c, D.CDec c) => D.CopCalc c
copCeilAt        :: (D.CText c, D.CDec c) => D.CopCalc c
copCeilPer       :: (D.CText c, D.CDec c) => D.CopCalc c

copRound         = round1 D.decimalRound
copRoundAt       = round2 D.decimalRoundAt
copRoundPer      = round2 D.decimalRoundPer

copRoundEven     = round1 D.decimalRoundEven
copRoundEvenAt   = round2 D.decimalRoundEvenAt
copRoundEvenPer  = round2 D.decimalRoundEvenPer

copTrunc         = round1 D.decimalTrunc
copTruncAt       = round2 D.decimalTruncAt
copTruncPer      = round2 D.decimalTruncPer

copFloor         = round1 D.decimalFloor
copFloorAt       = round2 D.decimalFloorAt
copFloorPer      = round2 D.decimalFloorPer

copCeil          = round1 D.decimalCeil
copCeilAt        = round2 D.decimalCeilAt
copCeilPer       = round2 D.decimalCeilPer

round1 :: (D.CText c, D.CDec c) => B.Map D.Decimal -> [B.Ab c] -> B.Ab c
round1 f arg = 
    do dec <- getDec1 arg
       D.putDec $ f dec

round2 :: (D.CText c, D.CDec c) => B.Bin D.Decimal -> [B.Ab c] -> B.Ab c
round2 f arg = 
    do (per, dec) <- getDec2 arg
       D.putDec $ f per dec

-- --------------------------------------------  Add and subtract

copPlus :: (D.CText c, D.CDec c) => D.FraclSide -> D.CopCalc c
copPlus pr xs = fmap D.pDec $ loop xs where
    loop [] = Right 0
    loop (n : m) = do n' <- copDec n
                      m' <- loop m
                      (D.decimalAdd pr) n' m'

copPlus2 :: (D.CDec c, D.CClock c, D.CTime c) => D.FraclSide -> D.CopCalc c
copPlus2 pr [Right xc, Right yc]
    | D.isDec   xc && D.isDec   yc = D.putDec   =<< D.decimalAdd pr  (D.gDec   xc) (D.gDec   yc)
    | D.isClock xc && D.isClock yc = D.putClock =<< D.clockAdd       (D.gClock xc) (D.gClock yc)
    | D.isTime  xc && D.isClock yc = D.putTime  =<< D.timeAddClock   (D.gClock yc) (D.gTime  xc) 
    | D.isClock xc && D.isTime  yc = D.putTime  =<< D.timeAddClock   (D.gClock xc) (D.gTime  yc)
copPlus2 _ _ = Msg.unexpAttr "+"

copPlus1 :: (D.CDec c, D.CClock c, D.CTime c) => D.CopCalc c
copPlus1 [Right x] | D.isDec x = Right x
copPlus1 _ = Msg.unexpAttr "+"


-- --------------------------------------------  Multiply and divide

copMul :: (D.CText c, D.CDec c) => D.CopCalc c
copMul xs = fmap D.pDec $ loop xs where
    loop [] = Right 1
    loop (n : m) = do n' <- copDec n
                      m' <- loop m
                      D.decimalMul n' m'

copMinus2 :: (D.CText c, D.CDec c, D.CClock c, D.CTime c) => D.FraclSide -> D.CopCalc c
copMinus2 pr [Right xc, Right yc]
    | D.isDec   xc && D.isDec   yc = D.putDec   =<< D.decimalSub pr (D.gDec   xc) (D.gDec   yc)
    | D.isClock xc && D.isClock yc = D.putClock =<< D.clockSub      (D.gClock xc) (D.gClock yc)
    | D.isTime  xc && D.isTime  yc = D.putClock =<< D.timeDiff      (D.gTime  xc) (D.gTime  yc)
copMinus2 _ _ = Msg.unexpAttr "-"

copMinus1 :: (D.CDec c) => D.CopCalc c
copMinus1 [Right x] | D.isDec x = D.putDec $ negate $ D.gDec x
copMinus1 _ = Msg.unexpAttr "-"

copRecip :: (D.CText c, D.CDec c) => D.CopCalc c
copRecip arg =
    do a <- getDec1 arg
       D.putDec $ recip a

copDiv :: (D.CText c, D.CDec c) => D.CopCalc c
copDiv arg =
    do (a, b) <- getDec2 arg
       D.putDec $ a / b

copQuo :: (D.CText c, D.CDec c) => D.CopCalc c
copQuo arg =
    do (a, b) <- getDec2 arg
       c <- D.decimalQuo a b
       D.putDec c

copRem :: (D.CText c, D.CDec c) => D.CopCalc c
copRem arg =
    do (a, b) <- getDec2 arg
       c <- D.decimalRem a b
       D.putDec c

