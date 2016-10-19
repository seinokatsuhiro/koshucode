{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Arithmetic content operators.

module Koshucode.Baala.Cop.Arith
  ( copsArith
    -- $Operators
  ) where

import qualified Koshucode.Baala.Overture    as O
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

-- | Arithmetic content operators.
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

    , D.CopCalc  (D.copNormal "round-in")        copRoundIn
    , D.CopCalc  (D.copNormal "round-in-at")     copRoundInAt
    , D.CopCalc  (D.copNormal "round-in-per")    copRoundInPer

    , D.CopCalc  (D.copNormal "round-out")       copRoundOut
    , D.CopCalc  (D.copNormal "round-out-at")    copRoundOutAt
    , D.CopCalc  (D.copNormal "round-out-per")   copRoundOutPer

    , D.CopCalc  (D.copNormal "round-down")      copRoundDown
    , D.CopCalc  (D.copNormal "round-down-at")   copRoundDownAt
    , D.CopCalc  (D.copNormal "round-down-per")  copRoundDownPer

    , D.CopCalc  (D.copNormal "round-up")        copRoundUp
    , D.CopCalc  (D.copNormal "round-up-at")     copRoundUpAt
    , D.CopCalc  (D.copNormal "round-up-per")    copRoundUpPer

    , D.CopCalc  (D.copNormal "trunc")           copRoundIn
    , D.CopCalc  (D.copNormal "trunc-at")        copRoundInAt
    , D.CopCalc  (D.copNormal "trunc-per")       copRoundInPer
    , D.CopCalc  (D.copNormal "trunc-error")     copTruncError

    , D.CopCalc  (D.copNormal "floor")           copRoundDown
    , D.CopCalc  (D.copNormal "floor-at")        copRoundDownAt
    , D.CopCalc  (D.copNormal "floor-per")       copRoundDownPer

    , D.CopCalc  (D.copNormal "ceil")            copRoundUp
    , D.CopCalc  (D.copNormal "ceil-at")         copRoundUpAt
    , D.CopCalc  (D.copNormal "ceil-per")        copRoundUpPer

    -- ----------------------  add and subtract

    , D.CopCalc  (D.copInfix  "+")    $ copPlus2 D.FracleLong
    , D.CopCalc  (D.copInfix  ".+")   $ copPlus2 D.FracleLeft
    , D.CopCalc  (D.copInfix  "+.")   $ copPlus2 D.FracleRight
    , D.CopCalc  (D.copInfix  ".+.")  $ copPlus2 D.FracleStrict

    , D.CopCalc  (D.copNormal "+")    $ copPlus D.FracleLong
    , D.CopCalc  (D.copNormal ".+")   $ copPlus D.FracleLeft
    , D.CopCalc  (D.copNormal "+.")   $ copPlus D.FracleRight
    , D.CopCalc  (D.copNormal ".+.")  $ copPlus D.FracleStrict

    , D.CopCalc  (D.copInfix  "-")    $ copMinus2 D.FracleLong
    , D.CopCalc  (D.copInfix  ".-")   $ copMinus2 D.FracleLeft
    , D.CopCalc  (D.copInfix  "-.")   $ copMinus2 D.FracleRight
    , D.CopCalc  (D.copInfix  ".-.")  $ copMinus2 D.FracleStrict

    , D.CopCalc  (D.copNormal "-")    $ copMinus2 D.FracleLong
    , D.CopCalc  (D.copNormal ".-")   $ copMinus2 D.FracleLeft
    , D.CopCalc  (D.copNormal "-.")   $ copMinus2 D.FracleRight
    , D.CopCalc  (D.copNormal ".-.")  $ copMinus2 D.FracleStrict

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

copRoundIn       :: (D.CText c, D.CDec c) => D.CopCalc c
copRoundInAt     :: (D.CText c, D.CDec c) => D.CopCalc c
copRoundInPer    :: (D.CText c, D.CDec c) => D.CopCalc c
copTruncError    :: (D.CText c, D.CDec c) => D.CopCalc c

copRoundOut      :: (D.CText c, D.CDec c) => D.CopCalc c
copRoundOutAt    :: (D.CText c, D.CDec c) => D.CopCalc c
copRoundOutPer   :: (D.CText c, D.CDec c) => D.CopCalc c

copRoundDown     :: (D.CText c, D.CDec c) => D.CopCalc c
copRoundDownAt   :: (D.CText c, D.CDec c) => D.CopCalc c
copRoundDownPer  :: (D.CText c, D.CDec c) => D.CopCalc c

copRoundUp       :: (D.CText c, D.CDec c) => D.CopCalc c
copRoundUpAt     :: (D.CText c, D.CDec c) => D.CopCalc c
copRoundUpPer    :: (D.CText c, D.CDec c) => D.CopCalc c

copRound         = round1 D.decimalRound
copRoundAt       = round2 D.decimalRoundAt
copRoundPer      = round2 D.decimalRoundPer

copRoundEven     = round1 D.decimalRoundEven
copRoundEvenAt   = round2 D.decimalRoundEvenAt
copRoundEvenPer  = round2 D.decimalRoundEvenPer

copRoundIn       = round1 D.decimalTrunc
copRoundInAt     = round2 D.decimalTruncAt
copRoundInPer    = round2 D.decimalTruncPer
copTruncError    = round1 D.decimalTruncError

copRoundOut      = round1 D.decimalRoundOut
copRoundOutAt    = round2 D.decimalRoundOutAt
copRoundOutPer   = round2 D.decimalRoundOutPer

copRoundDown     = round1 D.decimalFloor
copRoundDownAt   = round2 D.decimalFloorAt
copRoundDownPer  = round2 D.decimalFloorPer

copRoundUp       = round1 D.decimalCeil
copRoundUpAt     = round2 D.decimalCeilAt
copRoundUpPer    = round2 D.decimalCeilPer

round1 :: (D.CText c, D.CDec c) => O.Map D.Decimal -> [B.Ab c] -> B.Ab c
round1 f arg = 
    do dec <- getDec1 arg
       D.putDec $ f dec

round2 :: (D.CText c, D.CDec c) => B.Bin D.Decimal -> [B.Ab c] -> B.Ab c
round2 f arg = 
    do (per, dec) <- getDec2 arg
       D.putDec $ f per dec

-- --------------------------------------------  Add and subtract

copPlus :: (Show c, D.CText c, D.CDec c) => D.FracleSide -> D.CopCalc c
copPlus pr xs = fmap D.pDec $ loop xs where
    loop [] = Right 0
    loop (n : m) = do n' <- copDec n
                      m' <- loop m
                      (D.decimalAdd pr) n' m'

copPlus2 :: (D.CDec c, D.CClock c, D.CTime c) => D.FracleSide -> D.CopCalc c
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

copMul :: (Show c, D.CText c, D.CDec c) => D.CopCalc c
copMul xs = fmap D.pDec $ loop xs where
    loop [] = Right 1
    loop (n : m) = do n' <- copDec n
                      m' <- loop m
                      D.decimalMul n' m'

copMinus2 :: (D.CText c, D.CDec c, D.CClock c, D.CTime c) => D.FracleSide -> D.CopCalc c
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

