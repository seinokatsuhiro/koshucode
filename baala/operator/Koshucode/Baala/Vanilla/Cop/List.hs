{-# OPTIONS_GHC -Wall #-}

{-| Content operators. -}

module Koshucode.Baala.Vanilla.Cop.List
( copList
-- $Operators
) where

import Koshucode.Baala.Base
import Koshucode.Baala.Core

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

litText :: [TokenTree] -> AbOr VContent
litText xs =
    do ss <- mapM litT xs
       Right . putText $ concat ss

litT :: TokenTree -> AbOr String
litT (TreeL (TWord _ _ w)) = Right w
litT x = Left $ AbortNotText (show x)

copList :: [Named (Cop VContent)]
copList =
    [ namedEager  "list"    list
    , namedEager  "total"   listTotal
    , namedEager  "length"  listLength
    , namedEager  "min"     listMin
    , namedEager  "max"     listMax
    , namedLit    "'"       litText
    ]

list :: [VContent] -> AbOr VContent
list = Right . putList

listTotal :: [VContent] -> AbOr VContent
listTotal [VList xs] = Right . putInt $ sum (map toInt xs)
listTotal xs = Left $ AbortUnmatchType (concatMap typename xs)

listMin :: [VContent] -> AbOr VContent
listMin [VList xs] = Right $ minimum xs
listMin xs = Left $ AbortUnmatchType (concatMap typename xs)

listMax :: [VContent] -> AbOr VContent
listMax [VList xs] = Right $ maximum xs
listMax xs = Left $ AbortUnmatchType (concatMap typename xs)

listLength :: [VContent] -> AbOr VContent
listLength [VList   xs]     = Right . putInt $ length xs
listLength [VText xs]       = Right . putInt $ length xs
listLength [VRel (Rel _ b)] = Right . putInt $ length b
listLength xs = Left $ AbortUnmatchType (concatMap typename xs)

