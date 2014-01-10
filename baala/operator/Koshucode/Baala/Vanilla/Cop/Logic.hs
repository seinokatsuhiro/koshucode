{-# OPTIONS_GHC -Wall #-}

{-| Content operators. -}

module Koshucode.Baala.Vanilla.Cop.Logic
( copsLogic
  -- $Operators
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C
import qualified Koshucode.Baala.Vanilla.Type as V



-- ----------------------
{- $Operators

 [@not@]    Logical negation.

 [@and@]    Logical conjunction.

 [@or@]     Logical disjunction.

 [@then@]   Logical implication.

 [@when@]   Inverse implication.

-}

copsLogic :: [C.Cop V.VContent]
copsLogic =
    [ C.CopFun  "not"   copNot
    , C.CopFun  "and"   copAnd
    , C.CopFun  "or"    copOr
    , C.CopFun  "then"  copImp
    , C.CopFun  "when"  copWhen
    , C.CopFun  "if"    copIf
    ]

cop1 :: (Bool -> Bool) -> V.VCop
cop1 p [x] =
    do x' <- C.needBool x
       Right . C.putBool $ p x'
cop1 _ xs = typeUnmatch xs

cop2 :: (Bool -> Bool -> Bool) -> V.VCop
cop2 p [x, y] =
    do x' <- C.needBool x
       y' <- C.needBool y
       Right . C.putBool $ p x' y'
cop2 _ xs = typeUnmatch xs

copN :: Bool -> (Bool -> Bool -> Bool) -> V.VCop
copN unit p = loop where
    loop [] = Right . C.putBool $ unit
    loop (x : xs) =
        do x' <- C.needBool x
           V.VBool xs' <- loop xs 
           Right . C.putBool $ p x' xs'

copNot :: V.VCop
copNot =  cop1 not

copWhen  :: V.VCop
copWhen  =  cop2 $ \x y -> x || not y

copImp :: V.VCop
copImp =  cop2 $ \x y -> not x || y

copAnd :: V.VCop
copAnd =  copN True (&&)

copOr  :: V.VCop
copOr  =  copN False (||)

copIf  :: V.VCop
copIf [test, a, b] =
    do test' <- C.needBool test
       case test' of
         True  -> Right a
         False -> Right b
copIf xs = typeUnmatch xs

typeUnmatch :: C.PrimContent a => [a] -> Either B.AbortReason b
typeUnmatch xs = Left $ B.AbortCalc [] $ B.ACUnmatchType (concatMap C.typename xs)

