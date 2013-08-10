{-# OPTIONS_GHC -Wall #-}

{-| Content operators. -}

module Koshucode.Baala.Vanilla.Cop.List
( copsList
  -- $Operators
) where

import qualified Data.List as L
import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C
import Koshucode.Baala.Vanilla.Type



-- ----------------------
{- $Operators

 [@list@]       Construct list.

 [@total@]      Calculate total amount of elements in list,
                i.e., summation.

 [@length@]     Number of elements.

 [@min@]        Minimal element.

 [@max@]        Maximal element.

 [@++@]         Append lists.

 [@intersect@]  Common elements.

 [@minus@]      Remove elements.

-}

litText :: [B.TokenTree] -> B.Ab VContent
litText xs =
    do ss <- mapM litT xs
       Right . C.putText $ concat ss

litT :: B.TokenTree -> B.Ab String
litT (B.TreeL (B.TWord _ _ w)) = Right w
litT x = Left $ B.AbortNotText (show x)

copsList :: [B.Named (C.Cop VContent)]
copsList =
    [ C.namedEager  "list"      copList
    , C.namedEager  "total"     copTotal
    , C.namedEager  "length"    copLength
    , C.namedEager  "min"       copMin
    , C.namedEager  "max"       copMax
    , C.namedEager  "++"        copAppend
    , C.namedEager  "intersect" copIntersect
    , C.namedEager  "minus"     copMinus
    , C.namedLit    "'"         litText
    ]

copList :: VCop
copList = Right . C.putList



-- ----------------------  aggregation

copTotal :: VCop
copTotal = op where
    op [VList xs] = Right . C.putDec =<< C.decimalSum (map C.getDec xs)
    op xs = Left $ B.AbortUnmatchType (concatMap C.typename xs)

copMin :: VCop
copMin = op where
    op [VList xs] = Right $ minimum xs
    op xs = Left $ B.AbortUnmatchType (concatMap C.typename xs)

copMax :: VCop
copMax = op where
    op [VList xs] = Right $ maximum xs
    op xs = Left $ B.AbortUnmatchType (concatMap C.typename xs)

copLength :: VCop
copLength = op where
    op [VList xs]         = Right . C.putDecFromInt $ length xs
    op [VText xs]         = Right . C.putDecFromInt $ length xs
    op [VRel (B.Rel _ b)] = Right . C.putDecFromInt $ length b
    op xs = Left $ B.AbortUnmatchType (concatMap C.typename xs)



-- ----------------------  set-like operation

copAppend :: VCop
copAppend [] = Right VNil
copAppend xs@(x : _) = op x where
    op (VText _) = Right . C.putText . concat =<< mapM C.needText xs
    op (VSet  _) = Right . C.putSet  . concat =<< mapM C.needSet  xs
    op (VList _) = Right . C.putList . concat =<< mapM C.needList xs
    op _ = Left $ B.AbortUnmatchType (concatMap C.typename xs)

copIntersect :: VCop
copIntersect [] = Right VNil
copIntersect xs@(x : _) = op x where
    op (VSet  _) = Right . C.putSet  . intersectLists =<< mapM C.needSet  xs
    op (VList _) = Right . C.putList . intersectLists =<< mapM C.needList xs
    op _ = Left $ B.AbortUnmatchType (concatMap C.typename xs)

copMinus :: VCop
copMinus = op where
    op [VSet a,  VSet b]  = Right $ VSet  (a L.\\ b)
    op [VList a, VList b] = Right $ VList (a L.\\ b)
    op xs = Left $ B.AbortUnmatchType (concatMap C.typename xs)

intersectLists :: (Eq a) => [[a]] -> [a]
intersectLists [] = []
intersectLists [a] = a
intersectLists (a : b : xs) = intersectLists $ L.intersect a b : xs

