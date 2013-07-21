{-# OPTIONS_GHC -Wall #-}

{-| Content operators. -}

module Koshucode.Baala.Vanilla.Cop.Logic
( copLogic
-- $Operators
) where

import Koshucode.Baala.Base.Abort
import Koshucode.Baala.Base.Content
import Koshucode.Baala.Base.Prelude

import Koshucode.Baala.Vanilla.Value.Content



-- ----------------------
{- $Operators

 [@not@]   Logical negation.

 [@and@]   Logical conjunction.

 [@or@]    Logical disjunction.

-}

copLogic :: [Named (Cop VContent)]
copLogic =
 [ namedEager  "not"  logiNot
 , namedEager  "and"  logiAnd
 , namedEager  "or"   logiOr
 , namedEager  "==>"  logiImply
 , namedEager  "<=="  logiIf
 ]

logiN :: Bool -> (Bool -> Bool -> Bool) -> [VContent] -> AbOr VContent
logiN unit p = loop where
    loop [] = Right . putBool $ unit
    loop (VBool x : xs) =
        do VBool xs' <- loop xs 
           Right . putBool $ p x xs'
    loop _ = Left AbortUnmatchType

logi2 :: (Bool -> Bool -> Bool) -> [VContent] -> AbOr VContent
logi2 p [VBool x, VBool y] = Right . putBool $ p x y
logi2 _ _ = Left AbortUnmatchType

logiAnd    :: [VContent] -> AbOr VContent
logiAnd    =  logiN True (&&)

logiOr     :: [VContent] -> AbOr VContent
logiOr     =  logiN False (||)

logiIf     :: [VContent] -> AbOr VContent
logiIf     =  logi2 $ \x y -> x || not y

logiImply  :: [VContent] -> AbOr VContent
logiImply  =  logi2 $ \x y -> not x || y

logiNot :: [VContent] -> AbOr VContent
logiNot [VBool x] = Right . putBool $ not x
logiNot _ = Left AbortUnmatchType

