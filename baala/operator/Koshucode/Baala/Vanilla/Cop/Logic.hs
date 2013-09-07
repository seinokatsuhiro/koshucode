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
    [ C.namedEager  "not"   copNot
    , C.namedEager  "and"   copAnd
    , C.namedEager  "or"    copOr
    , C.namedEager  "then"  copImp
    , C.namedEager  "when"  copWhen
    , C.namedEager  "if"    copIf
    ]

cop1 :: (Bool -> Bool) -> VCop
cop1 p [x] =
    do x' <- C.needBool x
       Right . C.putBool $ p x'
cop1 _ xs = Left $ B.AbortUnmatchType (concatMap C.typename xs)

cop2 :: (Bool -> Bool -> Bool) -> VCop
cop2 p [x, y] =
    do x' <- C.needBool x
       y' <- C.needBool y
       Right . C.putBool $ p x' y'
cop2 _ xs = Left $ B.AbortUnmatchType (concatMap C.typename xs)

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
copIf xs = Left $ B.AbortUnmatchType (concatMap C.typename xs)

