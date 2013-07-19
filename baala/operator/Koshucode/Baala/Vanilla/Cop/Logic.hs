{-# OPTIONS_GHC -Wall #-}

{-| Content operators. -}

module Koshucode.Baala.Vanilla.Cop.Logic
( copLogic
) where

import Koshucode.Baala.Base.Abort
import Koshucode.Baala.Base.Content
import Koshucode.Baala.Base.Prelude hiding ((<>), hang, empty, semi)

import Koshucode.Baala.Vanilla.Value.Val



-- ----------------------  Logic

copLogic :: [Named (ContentOp Val)]
copLogic =
 [ namedEager  "not"  logiNot
 , namedEager  "and"  logiAnd
 , namedEager  "or"   logiOr
 , namedEager  "==>"  logiImply
 , namedEager  "<=="  logiIf
 ]

logi :: (Bool -> Bool -> Bool) -> [Val] -> AbOr Val
logi p [Boolv x, Boolv y] = Right . boolValue $ x `p` y
logi _ _ = Left $ AbortUndefined "not boolean"

logiAnd :: [Val] -> AbOr Val
logiAnd = logi (&&)

logiOr :: [Val] -> AbOr Val
logiOr = logi (||)

logiIf :: [Val] -> AbOr Val
logiIf = logi $ \x y -> x || not y

logiImply :: [Val] -> AbOr Val
logiImply = logi $ \x y -> not x || y

logiNot :: [Val] -> AbOr Val
logiNot [Boolv x] = Right . boolValue $ not x
logiNot _ = Left $ AbortUndefined "not boolean"

