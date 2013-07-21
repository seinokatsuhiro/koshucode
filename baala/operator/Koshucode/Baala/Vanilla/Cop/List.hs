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

 [@length@]  Number of elements.

 [@min@]     Minimal element.

 [@max@]     Maximal element.

-}

copList :: [Named (Cop VContent)]
copList =
 [ namedEager  "list"     list
 , namedEager  "total"    listTotal
 , namedEager  "length"   listLength
 , namedEager  "min"      listMin
 , namedEager  "max"      listMax
 ]

list :: [VContent] -> AbOr VContent
list = Right . putList

listTotal :: [VContent] -> AbOr VContent
listTotal [VList xs] = Right . putInt $ sum (map toInt xs)
listTotal _ = Left AbortUnmatchType

listMin :: [VContent] -> AbOr VContent
listMin [VList xs] = Right $ minimum xs
listMin _ = Left AbortUnmatchType

listMax :: [VContent] -> AbOr VContent
listMax [VList xs] = Right $ maximum xs
listMax _ = Left AbortUnmatchType

listLength :: [VContent] -> AbOr VContent
listLength [VList   xs]     = Right . putInt $ length xs
listLength [VString xs]     = Right . putInt $ length xs
listLength [VRel (Rel _ b)] = Right . putInt $ length b
listLength _ = Left AbortUnmatchType

