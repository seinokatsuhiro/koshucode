{-# OPTIONS_GHC -Wall #-}

{-| Content operators. -}

module Koshucode.Baala.Vanilla.Cop.List
( copList
) where

import Koshucode.Baala.Base.Abort
import Koshucode.Baala.Base.Content
import Koshucode.Baala.Base.Prelude hiding ((<>), hang, empty, semi)
import Koshucode.Baala.Base.Syntax

import Koshucode.Baala.Vanilla.Value.Val



-- ----------------------

abortReason :: AbortReason -> [SourceLine] -> Abort
abortReason a src = (a, src)



-- ----------------------  Order

copList :: [Named (ContentOp Val)]
copList =
 [ namedEager  "list"     list
 , namedEager  "total"    listTotal
 , namedEager  "size"     listSize
 , namedEager  "min"      listMin
 , namedEager  "max"      listMax
 ]

list :: [Val] -> AbOr Val
list = Right . listValue

listTotal :: [Val] -> AbOr Val
listTotal [Listv xs] = Right . intValue $ sum (map toInt xs)
listTotal xs = Left . abortReason $ AbortLookup (show xs)

listMin :: [Val] -> AbOr Val
listMin [Listv xs] = Right $ minimum xs
listMin xs = Left . abortReason $ AbortLookup (show xs)

listMax :: [Val] -> AbOr Val
listMax [Listv xs] = Right $ maximum xs
listMax xs = Left . abortReason $ AbortLookup (show xs)

listSize :: [Val] -> AbOr Val
listSize [Listv   xs]     = Right . intValue $ length xs
listSize [Stringv xs]     = Right . intValue $ length xs
listSize [Relv (Rel _ b)] = Right . intValue $ length b
listSize xs = Left . abortReason $ AbortLookup (show xs)


