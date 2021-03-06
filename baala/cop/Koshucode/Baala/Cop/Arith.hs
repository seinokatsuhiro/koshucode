{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Arithmetic content operators.

module Koshucode.Baala.Cop.Arith
  ( copsArith,
    copPlus2, copMinus2, copMinus1,
    copMul, copDiv,
  ) where

import qualified Koshucode.Baala.Overture          as O
import qualified Koshucode.Baala.Base              as B
import qualified Koshucode.Baala.Type              as T
import qualified Koshucode.Baala.Data              as D
import qualified Koshucode.Baala.Rop.Base.Message  as Msg


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

    , D.CopCalc  (D.copInfix  "+")    $ copPlus2 T.FracleLong
    , D.CopCalc  (D.copInfix  ".+")   $ copPlus2 T.FracleLeft
    , D.CopCalc  (D.copInfix  "+.")   $ copPlus2 T.FracleRight
    , D.CopCalc  (D.copInfix  ".+.")  $ copPlus2 T.FracleStrict

    , D.CopCalc  (D.copNormal "+")    $ copPlus T.FracleLong
    , D.CopCalc  (D.copNormal ".+")   $ copPlus T.FracleLeft
    , D.CopCalc  (D.copNormal "+.")   $ copPlus T.FracleRight
    , D.CopCalc  (D.copNormal ".+.")  $ copPlus T.FracleStrict

    , D.CopCalc  (D.copInfix  "-")    $ copMinus2 T.FracleLong
    , D.CopCalc  (D.copInfix  ".-")   $ copMinus2 T.FracleLeft
    , D.CopCalc  (D.copInfix  "-.")   $ copMinus2 T.FracleRight
    , D.CopCalc  (D.copInfix  ".-.")  $ copMinus2 T.FracleStrict

    , D.CopCalc  (D.copNormal "-")    $ copMinus2 T.FracleLong
    , D.CopCalc  (D.copNormal ".-")   $ copMinus2 T.FracleLeft
    , D.CopCalc  (D.copNormal "-.")   $ copMinus2 T.FracleRight
    , D.CopCalc  (D.copNormal ".-.")  $ copMinus2 T.FracleStrict

    -- ----------------------  multiply and divide

    , D.CopCalc  (D.copInfix  "*")      copMul
    , D.CopCalc  (D.copNormal "*")      copMul

    , D.CopCalc  (D.copInfix  "div")    copDiv
    , D.CopCalc  (D.copInfix  "per")    copDiv
    , D.CopCalc  (D.copInfix  "quo")    copQuo
    , D.CopCalc  (D.copInfix  "rem")    copRem

    , D.CopCalc  (D.copNormal "recip")  copRecip
    , D.CopCalc  (D.copNormal "div")    copDiv
    , D.CopCalc  (D.copNormal "per")    copDiv
    , D.CopCalc  (D.copNormal "quo")    copQuo
    , D.CopCalc  (D.copNormal "rem")    copRem
    ]


copDec :: (Show c, D.CText c, D.CDec c) => B.Ab c -> B.Ab T.Decimal
copDec (Right c) | D.isDec  c = Right $ D.gDec c
                 | D.isText c = T.decodeDecimal $ D.gText c
copDec x = Msg.notNumber (show x)

getDecFrom :: (D.CDec c, D.CText c) => c -> B.Ab T.Decimal
getDecFrom c | D.isDec  c  = Right $ D.gDec c
             | D.isText c  = T.decodeDecimal $ D.gText c
             | otherwise   = Right 0

getDec1 :: (D.CDec c, D.CText c) => [B.Ab c] -> B.Ab T.Decimal
getDec1 arg =
    do x' <- D.getRightArg1 arg
       getDecFrom x'

getDec2 :: (D.CDec c, D.CText c) => [B.Ab c] -> B.Ab (T.Decimal, T.Decimal)
getDec2 arg =
    do (x', y') <- D.getRightArg2 arg
       x <- getDecFrom x'
       y <- getDecFrom y'
       Right (x, y)

copIntPart :: (D.CText c, D.CDec c) => D.CopCalc c
copIntPart arg =
    do a <- getDec1 arg
       D.putDec $ T.decimalIntPart a

copFracPart :: (D.CText c, D.CDec c) => D.CopCalc c
copFracPart arg =
    do a <- getDec1 arg
       D.putDec $ T.decimalFracPart a

copAbs :: (D.CContent c) => D.CopCalc c
copAbs [Right c] | D.isList c = Right . D.pList =<< mapM copAbs1 (D.gList c)
                 | otherwise  = copAbs1 c
copAbs xs = Msg.badArg xs

copAbs1 :: (D.CContent c) => B.AbMap c
copAbs1 c | D.isDec c = D.putDec $ abs $ D.gDec c
copAbs1 c = Msg.badArg [Right c]

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

copRound         = round1 T.decimalRound
copRoundAt       = round2 T.decimalRoundAt
copRoundPer      = round2 T.decimalRoundPer

copRoundEven     = round1 T.decimalRoundEven
copRoundEvenAt   = round2 T.decimalRoundEvenAt
copRoundEvenPer  = round2 T.decimalRoundEvenPer

copRoundIn       = round1 T.decimalTrunc
copRoundInAt     = round2 T.decimalTruncAt
copRoundInPer    = round2 T.decimalTruncPer
copTruncError    = round1 T.decimalTruncError

copRoundOut      = round1 T.decimalRoundOut
copRoundOutAt    = round2 T.decimalRoundOutAt
copRoundOutPer   = round2 T.decimalRoundOutPer

copRoundDown     = round1 T.decimalFloor
copRoundDownAt   = round2 T.decimalFloorAt
copRoundDownPer  = round2 T.decimalFloorPer

copRoundUp       = round1 T.decimalCeil
copRoundUpAt     = round2 T.decimalCeilAt
copRoundUpPer    = round2 T.decimalCeilPer

round1 :: (D.CText c, D.CDec c) => O.Map T.Decimal -> [B.Ab c] -> B.Ab c
round1 f arg = 
    do dec <- getDec1 arg
       D.putDec $ f dec

round2 :: (D.CText c, D.CDec c) => O.Bin T.Decimal -> [B.Ab c] -> B.Ab c
round2 f arg = 
    do (per, dec) <- getDec2 arg
       D.putDec $ f per dec

-- --------------------------------------------  Add and subtract

copPlus :: (Show c, D.CText c, D.CDec c) => T.FracleSide -> D.CopCalc c
copPlus pr xs = fmap D.pDec $ loop xs where
    loop [] = Right 0
    loop (n : m) = do n' <- copDec n
                      m' <- loop m
                      (T.decimalAdd pr) n' m'

{-| [/X/ + /Y/]    Add /X/ and /Y/. -}
copPlus2 :: (D.CContent c) => T.FracleSide -> D.CopCalc c
copPlus2 pr [Right xc, Right yc]
    | D.isDec   xc && D.isDec   yc = D.putDec   =<< T.decimalAdd pr  (D.gDec   xc) (D.gDec   yc)
    | D.isClock xc && D.isClock yc = D.putClock =<< T.clockAdd       (D.gClock xc) (D.gClock yc)
    | D.isTime  xc && D.isClock yc = D.putTime  =<< T.timeAddClock   (D.gClock yc) (D.gTime  xc) 
    | D.isClock xc && D.isTime  yc = D.putTime  =<< T.timeAddClock   (D.gClock xc) (D.gTime  yc)
copPlus2 _ xs = Msg.badArg xs

{-| [+ /X/]  Just /X/. -}
copPlus1 :: (D.CContent c) => D.CopCalc c
copPlus1 [Right x] | D.isDec x = Right x
copPlus1 xs = Msg.badArg xs

{-| [/X/ - /Y/]    Calculate /X/ minus /Y/. -}
copMinus2 :: (D.CContent c) => T.FracleSide -> D.CopCalc c
copMinus2 pr [Right xc, Right yc]
    | D.isDec   xc && D.isDec   yc = D.putDec   =<< T.decimalSub pr (D.gDec   xc) (D.gDec   yc)
    | D.isClock xc && D.isClock yc = D.putClock =<< T.clockSub      (D.gClock xc) (D.gClock yc)
    | D.isTime  xc && D.isTime  yc = D.putClock =<< T.timeDiff      (D.gTime  xc) (D.gTime  yc)
copMinus2 _ xs = Msg.badArg xs

{-| [- /X/]  Negate /X/. -}
copMinus1 :: (D.CContent c) => D.CopCalc c
copMinus1 [Right x] | D.isDec x = D.putDec $ negate $ D.gDec x
copMinus1 xs = Msg.badArg xs


-- --------------------------------------------  Multiply and divide

{-| [/X/ * /Y/]  Multiply /X/ by /Y/. -}
copMul :: (Show c, D.CText c, D.CDec c) => D.CopCalc c
copMul xs = fmap D.pDec $ loop xs where
    loop [] = Right 1
    loop (n : m) = do n' <- copDec n
                      m' <- loop m
                      T.decimalMul n' m'

copRecip :: (D.CText c, D.CDec c) => D.CopCalc c
copRecip arg =
    do a <- getDec1 arg
       D.putDec $ recip a

{-| [/X/ div /Y/]  Divide /X/ by /Y/.
    [/X/ per /Y/]  Divide /X/ by /Y/. -}
copDiv :: (D.CText c, D.CDec c) => D.CopCalc c
copDiv arg =
    do (a, b) <- getDec2 arg
       D.putDec $ a / b

copQuo :: (D.CText c, D.CDec c) => D.CopCalc c
copQuo arg =
    do (a, b) <- getDec2 arg
       c <- T.decimalQuo a b
       D.putDec c

copRem :: (D.CText c, D.CDec c) => D.CopCalc c
copRem arg =
    do (a, b) <- getDec2 arg
       c <- T.decimalRem a b
       D.putDec c

