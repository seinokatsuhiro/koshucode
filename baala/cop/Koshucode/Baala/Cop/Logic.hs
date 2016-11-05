{-# OPTIONS_GHC -Wall #-}

-- | Content operators on Boolean.

module Koshucode.Baala.Cop.Logic
  ( copsLogic
    -- $Operators
  ) where

import qualified Koshucode.Baala.Overture     as O
import qualified Koshucode.Baala.Data         as D
import qualified Koshucode.Baala.Cop.Coxhand  as H


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

-- | Content operators on Boolean.
copsLogic :: (D.CBool c, D.CEmpty c) => [D.Cop c]
copsLogic =
    [ D.CopCalc  (D.copInfix    "and")     copAnd
    , D.CopCalc  (D.copInfix    "or")      copOr
    , D.CopCalc  (D.copInfix    "then")    copImp
    , D.CopCalc  (D.copInfix    "when")    copWhen
    , D.CopCalc  (D.copNormal   "not")     copNot
    , D.CopCalc  (D.copNormal   "and")     copAnd
    , D.CopCalc  (D.copNormal   "or")      copOr
    , D.CopCalc  (D.copNormal   "then")    copImp
    , D.CopCalc  (D.copNormal   "when")    copWhen

    , D.CopCox   (D.copNormal   "all")   $ copCollect "and"
    , D.CopCox   (D.copNormal   "any")   $ copCollect "or"
    ]

cop1 :: (D.CBool c) => (Bool -> Bool) -> D.CopCalc c
cop1 p arg =
    do xc <- D.getRightArg1 arg
       x  <- D.getBool $ Right xc
       D.putBool $ p x

cop2 :: (D.CBool c) => O.Bin Bool -> D.CopCalc c
cop2 p arg =
    do (xc, yc) <- D.getRightArg2 arg
       x <- D.getBool $ Right xc
       y <- D.getBool $ Right yc
       D.putBool $ p x y

copN :: (D.CBool c) => Bool -> O.Bin Bool -> D.CopCalc c
copN unit op = loop where
    loop []   = D.putBool unit
    loop [xc] = xc
    loop (xc1 : xc2 : xs) =
        do xc1' <- xc1
           xc2' <- xc2
           x1 <- D.getBool $ Right xc1'
           x2 <- D.getBool $ Right xc2'
           loop $ D.putBool (x1 `op` x2) : xs

copNot  :: (D.CBool c) => D.CopCalc c
copNot  = cop1 not

copWhen :: (D.CBool c) => D.CopCalc c
copWhen = cop2 $ \x y -> x || not y

copImp  :: (D.CBool c) => D.CopCalc c
copImp  = cop2 $ \x y -> not x || y

copAnd  :: (D.CBool c) => D.CopCalc c
copAnd  = copN True (&&)

copOr   :: (D.CBool c) => D.CopCalc c
copOr   = copN False (||)

copCollect :: String -> D.CopCox c
copCollect n fs = Right $ H.f1 $ H.ib (D.copInfix n) (map fill fs) where
    fill f = H.ix f [H.b1]

