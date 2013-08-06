{-# OPTIONS_GHC -Wall #-}

{-| Content operators. -}

module Koshucode.Baala.Vanilla.Cop.Logic
( copsLogic
  -- $Operators
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C
import Koshucode.Baala.Vanilla.Type.Content



-- ----------------------
{- $Operators

 [@not@]    Logical negation.

 [@and@]    Logical conjunction.

 [@or@]     Logical disjunction.

 [@then@]   Logical implication.

 [@where@]  Inverse implication.

-}

copsLogic :: [B.Named (C.Cop VContent)]
copsLogic =
 [ C.namedEager  "not"   copNot
 , C.namedEager  "and"   copAnd
 , C.namedEager  "or"    copOr
 , C.namedEager  "then"  copImp
 , C.namedEager  "where" copIf
 ]

copN :: Bool -> (Bool -> Bool -> Bool) -> VCop
copN unit p = loop where
    loop [] = Right . C.putBool $ unit
    loop (x : xs) =
        do x' <- C.needBool x
           VBool xs' <- loop xs 
           Right . C.putBool $ p x' xs'

cop2 :: (Bool -> Bool -> Bool) -> VCop
cop2 p [x, y] = do x' <- C.needBool x
                   y' <- C.needBool y
                   Right . C.putBool $ p x' y'
cop2 _ xs = Left $ B.AbortUnmatchType (concatMap C.typename xs)

cop1 :: (Bool -> Bool) -> VCop
cop1 p [x] = do x' <- C.needBool x
                Right . C.putBool $ p x'
cop1 _ xs = Left $ B.AbortUnmatchType (concatMap C.typename xs)

copAnd :: VCop
copAnd =  copN True (&&)

copOr  :: VCop
copOr  =  copN False (||)

copIf  :: VCop
copIf  =  cop2 $ \x y -> x || not y

copImp :: VCop
copImp =  cop2 $ \x y -> not x || y

copNot :: VCop
copNot = cop1 not

