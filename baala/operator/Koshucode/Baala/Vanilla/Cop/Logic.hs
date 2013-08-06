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

 [@not@]   Logical negation.

 [@and@]   Logical conjunction.

 [@or@]    Logical disjunction.

-}

copsLogic :: [B.Named (C.Cop VContent)]
copsLogic =
 [ C.namedEager  "not"  copNot
 , C.namedEager  "and"  copAnd
 , C.namedEager  "or"   copOr
 , C.namedEager  "==>"  copImply
 , C.namedEager  "<=="  copIf
 ]

copN :: Bool -> (Bool -> Bool -> Bool) -> VCop
copN unit p = loop where
    loop [] = Right . C.putBool $ unit
    loop (VBool x : xs) =
        do VBool xs' <- loop xs 
           Right . C.putBool $ p x xs'
    loop xs = Left $ B.AbortUnmatchType (concatMap C.typename xs)

cop2 :: (Bool -> Bool -> Bool) -> VCop
cop2 p [VBool x, VBool y] = Right . C.putBool $ p x y
cop2 _ xs = Left $ B.AbortUnmatchType (concatMap C.typename xs)

copAnd    :: VCop
copAnd    =  copN True (&&)

copOr     :: VCop
copOr     =  copN False (||)

copIf     :: VCop
copIf     =  cop2 $ \x y -> x || not y

copImply  :: VCop
copImply  =  cop2 $ \x y -> not x || y

copNot :: VCop
copNot [VBool x] = Right . C.putBool $ not x
copNot xs = Left $ B.AbortUnmatchType (concatMap C.typename xs)

