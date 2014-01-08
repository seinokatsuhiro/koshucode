{-# OPTIONS_GHC -Wall #-}

{-| Content operators. -}

module Koshucode.Baala.Vanilla.Cop.List
( copsList
  -- $Operators
) where

import qualified Data.List as L
import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C
import qualified Koshucode.Baala.Vanilla.Type as V



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

litText :: [B.TokenTree] -> B.Ab V.VContent
litText xs =
    do ss <- mapM litT xs
       Right . C.putText $ concat ss

litT :: B.TokenTree -> B.Ab String
litT (B.TreeL (B.TWord _ _ w)) = Right w
litT x = Left $ B.AbortSyntax [] $ B.ASNotText (show x)

copsList :: [B.Named (C.Cop V.VContent)]
copsList =
    [ C.copFun  "++"          copAppend
    , C.copFun  "intersect"   copIntersect
    , C.copFun  "length"      copLength
    , C.copFun  "list"        copList
    , C.copFun  "max"         copMax
    , C.copFun  "min"         copMin
    , C.copFun  "minus"       copMinus
    , C.copFun  "reverse"     copReverse
    , C.copFun  "total"       copTotal
    , C.copFun  "sub-index"   copSubIndex
    , C.copFun  "sub-length"  copSubLength
    , C.coxLit  "'"           litText
    ]

copList :: V.VCop
copList = Right . C.putList



-- ----------------------  aggregation

copTotal :: V.VCop
copTotal = op where
    op [V.VList xs] = Right . C.putDec =<< B.decimalSum (map C.getDec xs)
    op xs = typeUnmatch xs

copMin :: V.VCop
copMin = op where
    op [V.VList xs] = Right $ minimum xs
    op xs = typeUnmatch xs

copMax :: V.VCop
copMax = op where
    op [V.VList xs] = Right $ maximum xs
    op xs = typeUnmatch xs

copLength :: V.VCop
copLength = op where
    op [V.VList xs]         = Right . C.putDecFromInt $ length xs
    op [V.VText xs]         = Right . C.putDecFromInt $ length xs
    op [V.VRel (B.Rel _ b)] = Right . C.putDecFromInt $ length b
    op xs = typeUnmatch xs

typeUnmatch :: C.PrimContent a => [a] -> Either B.AbortReason b
typeUnmatch xs = Left $ B.AbortCalc [] $ B.ACUnmatchType (concatMap C.typename xs)



-- ----------------------  set-like operation

copAppend :: V.VCop
copAppend [] = Right V.VNil
copAppend xs@(x : _) = op x where
    op (V.VText _) = Right . C.putText . concat =<< mapM C.needText xs
    op (V.VSet  _) = Right . C.putSet  . concat =<< mapM C.needSet  xs
    op (V.VList _) = Right . C.putList . concat =<< mapM C.needList xs
    op _ = typeUnmatch xs

copIntersect :: V.VCop
copIntersect [] = Right V.VNil
copIntersect xs@(x : _) = op x where
    op (V.VSet  _) = Right . C.putSet  . intersectLists =<< mapM C.needSet  xs
    op (V.VList _) = Right . C.putList . intersectLists =<< mapM C.needList xs
    op _ = typeUnmatch xs

copMinus :: V.VCop
copMinus = op where
    op [V.VSet a,  V.VSet b]  = Right $ V.VSet  (a L.\\ b)
    op [V.VList a, V.VList b] = Right $ V.VList (a L.\\ b)
    op xs = typeUnmatch xs

intersectLists :: (Eq a) => [[a]] -> [a]
intersectLists [] = []
intersectLists [a] = a
intersectLists (a : b : xs) = intersectLists $ L.intersect a b : xs



-- ----------------------  others

copReverse :: V.VCop
copReverse = op where
    op [V.VText xs] = Right . V.VText $ reverse xs
    op [V.VList xs] = Right . V.VList $ reverse xs
    op xs = typeUnmatch xs

copSubIndex :: V.VCop
copSubIndex = op where
    op [V.VText xs, V.VDec from, V.VDec to] =
        Right $ V.VText (subIndexDecimal from to xs)
    op xs = typeUnmatch xs

copSubLength :: V.VCop
copSubLength = op where
    op [V.VText xs, V.VDec from, V.VDec to] =
        Right $ V.VText (subLengthDecimal from to xs)
    op xs = typeUnmatch xs

subIndexDecimal :: B.Decimal -> B.Decimal -> [a] -> [a]
subIndexDecimal from to = subIndex intFrom intTo where
    intFrom = B.decimalNum from
    intTo   = B.decimalNum to

subLengthDecimal :: B.Decimal -> B.Decimal -> [a] -> [a]
subLengthDecimal from len = subLength intFrom intLen where
    intFrom = B.decimalNum from
    intLen  = B.decimalNum len

subIndex :: Int -> Int -> [a] -> [a]
subIndex from to xs = xs2 where
    xs2 = take to $ drop (from - 1) xs

subLength :: Int -> Int -> [a] -> [a]
subLength from len xs = xs2 where
    xs2 = take len $ drop (from - 1) xs

