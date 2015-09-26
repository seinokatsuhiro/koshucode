{-# OPTIONS_GHC -Wall #-}

-- | Content operators.

module Koshucode.Baala.Op.Cop.Logic
  ( copsLogic
    -- $Operators
  ) where

import qualified Koshucode.Baala.Base            as B
import qualified Koshucode.Baala.Data            as C
import qualified Koshucode.Baala.Core            as C
import qualified Koshucode.Baala.Op.Cop.Coxhand  as H


-- ----------------------
-- $Operators
--
--  [@all@]      @x is all f g h@ == @f x and g x and h x@.
--
--  [@and@]      Logical conjunction.
--
--  [@any@]      @x is any f g h@ == @f x or g x or h x@.
--
--  [@not@]      Logical negation.
--
--  [@or@]       Logical disjunction.
--
--  [@then@]     Logical implication.
--
--  [@when@]     Inverse implication.
--

copsLogic :: (C.CBool c, C.CEmpty c) => [C.Cop c]
copsLogic =
    [ C.CopCalc  (C.copInfix    "and")     copAnd
    , C.CopCalc  (C.copInfix    "or")      copOr
    , C.CopCalc  (C.copInfix    "then")    copImp
    , C.CopCalc  (C.copInfix    "when")    copWhen
    , C.CopCalc  (C.copNormal   "not")     copNot
    , C.CopCalc  (C.copNormal   "and")     copAnd
    , C.CopCalc  (C.copNormal   "or")      copOr
    , C.CopCalc  (C.copNormal   "then")    copImp
    , C.CopCalc  (C.copNormal   "when")    copWhen

    , C.CopCox   (C.copNormal   "all")   $ copCollect "and"
    , C.CopCox   (C.copNormal   "any")   $ copCollect "or"
    ]

cop1 :: (C.CBool c) => (Bool -> Bool) -> C.CopCalc c
cop1 p arg =
    do xc <- C.getRightArg1 arg
       x  <- C.getBool $ Right xc
       C.putBool $ p x

cop2 :: (C.CBool c) => B.Bin Bool -> C.CopCalc c
cop2 p arg =
    do (xc, yc) <- C.getRightArg2 arg
       x <- C.getBool $ Right xc
       y <- C.getBool $ Right yc
       C.putBool $ p x y

copN :: (C.CBool c) => Bool -> B.Bin Bool -> C.CopCalc c
copN unit op = loop where
    loop []   = C.putBool unit
    loop [xc] = xc
    loop (xc1 : xc2 : xs) =
        do xc1' <- xc1
           xc2' <- xc2
           x1 <- C.getBool $ Right xc1'
           x2 <- C.getBool $ Right xc2'
           loop $ C.putBool (x1 `op` x2) : xs

copNot  :: (C.CBool c) => C.CopCalc c
copNot  = cop1 not

copWhen :: (C.CBool c) => C.CopCalc c
copWhen = cop2 $ \x y -> x || not y

copImp  :: (C.CBool c) => C.CopCalc c
copImp  = cop2 $ \x y -> not x || y

copAnd  :: (C.CBool c) => C.CopCalc c
copAnd  = copN True (&&)

copOr   :: (C.CBool c) => C.CopCalc c
copOr   = copN False (||)

copCollect :: String -> C.CopCox c
copCollect n fs = Right $ H.f1 $ H.ib (C.copInfix n) (map fill fs) where
    fill f = H.ix f [H.b1]

