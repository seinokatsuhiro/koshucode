{-# OPTIONS_GHC -Wall #-}

-- | Content operators.

module Koshucode.Baala.Vanilla.Cop.List
( copsList
  -- $Operators
) where

import qualified Data.List as List
import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C
import qualified Koshucode.Baala.Vanilla.Type as V



-- ----------------------
-- $Operators
--
--  [@list@]       Construct list.
--
--  [@total@]      Calculate total amount of elements in list,
--                 i.e., summation.
--
--  [@length@]     Number of elements.
--
--  [@min@]        Minimal element.
--
--  [@max@]        Maximal element.
--
--  [@++@]         Append lists.
--
--  [@intersect@]  Common elements.
--
--  [@minus@]      Remove elements.
--

copsList :: [C.Cop V.VContent]
copsList =
    [ C.CopFun  "++"          copAppend
    , C.CopFun  "intersect"   copIntersect
    , C.CopFun  "length"      copLength
    , C.CopFun  "list"        copList
    , C.CopFun  "max"         copMax
    , C.CopFun  "min"         copMin
    , C.CopFun  "minus"       copMinus
    , C.CopFun  "reverse"     copReverse
    , C.CopFun  "total"       copTotal
    , C.CopFun  "sub-index"   copSubIndex
    , C.CopFun  "sub-length"  copSubLength
    ]

copList :: (C.CList c) => C.CopFun c
copList argC = do arg <- sequence argC
                  C.putList arg


-- ----------------------  aggregation

copTotal :: V.VCop
copTotal = op where
    op [Right (V.VList xs)] = C.putDec =<< B.decimalSum (map C.gDec xs)
    op xs = typeUnmatch xs

copMin :: V.VCop
copMin = op where
    op [Right (V.VList xs)] = Right $ minimum xs
    op xs = typeUnmatch xs

copMax :: V.VCop
copMax = op where
    op [Right (V.VList xs)] = Right $ maximum xs
    op xs = typeUnmatch xs

copLength :: V.VCop
copLength = op where
    op [Right (V.VList xs)]         = Right . C.pDecFromInt $ length xs
    op [Right (V.VText xs)]         = Right . C.pDecFromInt $ length xs
    op [Right (V.VRel (B.Rel _ b))] = Right . C.pDecFromInt $ length b
    op xs = typeUnmatch xs

typeUnmatch :: C.PrimContent a => [B.Ab a] -> Either B.AbortReason b
typeUnmatch _ = Left $ B.AbortCalc [] $ B.ACUnmatchType []



-- ----------------------  set-like operation

copAppend :: V.VCop
copAppend [] = Right V.VNil
copAppend xs@(x : _) = op x where
    op (Right (V.VText _)) = C.putText . concat =<< mapM C.getText xs
    op (Right (V.VSet  _)) = C.putSet  . concat =<< mapM C.getSet  xs
    op (Right (V.VList _)) = C.putList . concat =<< mapM C.getList xs
    op _ = typeUnmatch xs

copIntersect :: V.VCop
copIntersect [] = Right V.VNil
copIntersect xs@(x : _) = op x where
    op (Right (V.VSet  _)) = C.putSet  . intersectLists =<< mapM C.getSet  xs
    op (Right (V.VList _)) = C.putList . intersectLists =<< mapM C.getList xs
    op _ = typeUnmatch xs

copMinus :: V.VCop
copMinus = op where
    op [Right (V.VSet a),  Right (V.VSet b)]  = C.putSet  (a List.\\ b)
    op [Right (V.VList a), Right (V.VList b)] = C.putList (a List.\\ b)
    op xs = typeUnmatch xs

intersectLists :: (Eq a) => [[a]] -> [a]
intersectLists [] = []
intersectLists [a] = a
intersectLists (a : b : xs) = intersectLists $ List.intersect a b : xs



-- ----------------------  others

copReverse :: V.VCop
copReverse = op where
    op [Right (V.VText xs)] = C.putText $ reverse xs
    op [Right (V.VList xs)] = C.putList $ reverse xs
    op xs = typeUnmatch xs

copSubIndex :: V.VCop
copSubIndex = op where
    op arg = do arg3 <- C.getArg3 arg
                case arg3 of
                  (Right (V.VText xs), Right (V.VDec from), Right (V.VDec to))
                      -> C.putText (subIndexDecimal from to xs)
                  _ -> typeUnmatch arg

copSubLength :: V.VCop
copSubLength = op where
    op arg = do arg3 <- C.getArg3 arg
                case arg3 of
                  (Right (V.VText xs), Right (V.VDec from), Right (V.VDec to))
                      -> C.putText (subLengthDecimal from to xs)
                  _ -> typeUnmatch arg

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

