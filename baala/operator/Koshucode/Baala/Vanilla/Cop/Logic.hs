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

copLogic :: [Named (ContentOp VContent)]
copLogic =
 [ namedEager  "not"  logiNot
 , namedEager  "and"  logiAnd
 , namedEager  "or"   logiOr
 , namedEager  "==>"  logiImply
 , namedEager  "<=="  logiIf
 ]

logi :: (Bool -> Bool -> Bool) -> [VContent] -> AbOr VContent
logi p [VBool x, VBool y] = Right . putBool $ x `p` y
logi _ _ = Left $ AbortUndefined "not boolean"

logiAnd :: [VContent] -> AbOr VContent
logiAnd = logi (&&)

logiOr :: [VContent] -> AbOr VContent
logiOr = logi (||)

logiIf :: [VContent] -> AbOr VContent
logiIf = logi $ \x y -> x || not y

logiImply :: [VContent] -> AbOr VContent
logiImply = logi $ \x y -> not x || y

logiNot :: [VContent] -> AbOr VContent
logiNot [VBool x] = Right . putBool $ not x
logiNot _ = Left $ AbortUndefined "not boolean"

