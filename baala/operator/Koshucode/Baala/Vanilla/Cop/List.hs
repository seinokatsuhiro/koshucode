{-# OPTIONS_GHC -Wall #-}

{-| Content operators. -}

module Koshucode.Baala.Vanilla.Cop.List
( copList
-- $Operators
) where

import Koshucode.Baala.Base.Abort
import Koshucode.Baala.Base.Content
import Koshucode.Baala.Base.Prelude

import Koshucode.Baala.Vanilla.Value.Content



-- ----------------------
{- $Operators

 [@list@]    Construct list.

 [@total@]   Calculate total amount of elements in list,
             i.e., summation.

 [@size@]    Number of elements.

 [@min@]     Minimal element.

 [@max@]     Maximal element.

-}

copList :: [Named (ContentOp VContent)]
copList =
 [ namedEager  "list"     list
 , namedEager  "total"    listTotal
 , namedEager  "size"     listSize
 , namedEager  "min"      listMin
 , namedEager  "max"      listMax
 ]

list :: [VContent] -> AbOr VContent
list = Right . putList

listTotal :: [VContent] -> AbOr VContent
listTotal [VList xs] = Right . putInt $ sum (map toInt xs)
listTotal xs = Left $ AbortLookup (show xs)

listMin :: [VContent] -> AbOr VContent
listMin [VList xs] = Right $ minimum xs
listMin xs = Left $ AbortLookup (show xs)

listMax :: [VContent] -> AbOr VContent
listMax [VList xs] = Right $ maximum xs
listMax xs = Left $ AbortLookup (show xs)

listSize :: [VContent] -> AbOr VContent
listSize [VList   xs]     = Right . putInt $ length xs
listSize [VString xs]     = Right . putInt $ length xs
listSize [VRel (Rel _ b)] = Right . putInt $ length b
listSize xs = Left $ AbortLookup (show xs)

