{-# OPTIONS_GHC -Wall #-}

{-| Content operators. -}

module Koshucode.Baala.Vanilla.Cop.Logic
( copsLogic
  -- $Operators
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C
import Koshucode.Baala.Vanilla.Type



-- ----------------------
{- $Operators

 [@not@]    Logical negation.

 [@and@]    Logical conjunction.

 [@or@]     Logical disjunction.

 [@then@]   Logical implication.

 [@when@]   Inverse implication.

-}

copsLogic :: [B.Named (C.Cop VContent)]
copsLogic =
    [ C.copFun  "not"   copNot
    , C.copFun  "and"   copAnd
    , C.copFun  "or"    copOr
    , C.copFun  "then"  copImp
    , C.copFun  "when"  copWhen
    , C.copFun  "if"    copIf
    ]

cop1 :: (Bool -> Bool) -> VCop
cop1 p [x] =
    do x' <- C.needBool x
       Right . C.putBool $ p x'
cop1 _ xs = typeUnmatch xs

cop2 :: (Bool -> Bool -> Bool) -> VCop
cop2 p [x, y] =
    do x' <- C.needBool x
       y' <- C.needBool y
       Right . C.putBool $ p x' y'
cop2 _ xs = typeUnmatch xs

copN :: Bool -> (Bool -> Bool -> Bool) -> VCop
copN unit p = loop where
    loop [] = Right . C.putBool $ unit
    loop (x : xs) =
        do x' <- C.needBool x
           VBool xs' <- loop xs 
           Right . C.putBool $ p x' xs'

copNot :: VCop
copNot =  cop1 not

copWhen  :: VCop
copWhen  =  cop2 $ \x y -> x || not y

copImp :: VCop
copImp =  cop2 $ \x y -> not x || y

copAnd :: VCop
copAnd =  copN True (&&)

copOr  :: VCop
copOr  =  copN False (||)

copIf  :: VCop
copIf [test, a, b] =
    do test' <- C.needBool test
       case test' of
         True  -> Right a
         False -> Right b
copIf xs = typeUnmatch xs

typeUnmatch :: C.PrimContent a => [a] -> Either B.AbortReason b
typeUnmatch xs = Left $ B.AbortCalc [] $ B.ACUnmatchType (concatMap C.typename xs)

