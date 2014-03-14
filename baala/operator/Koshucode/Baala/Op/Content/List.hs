{-# OPTIONS_GHC -Wall #-}

-- | Content operators.

module Koshucode.Baala.Op.Content.List
( copsList
  -- $Operators
) where

import qualified Data.List                       as List
import qualified Koshucode.Baala.Base            as B
import qualified Koshucode.Baala.Core            as C
import qualified Koshucode.Baala.Op.Vanilla.Type as Op



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

copsList :: [C.Cop Op.VContent]
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

copTotal :: Op.VCop
copTotal = op where
    op [Right (Op.VList xs)] = C.putDec =<< B.decimalSum (map C.gDec xs)
    op xs = typeUnmatch xs

copMin :: Op.VCop
copMin = op where
    op [Right (Op.VList xs)] = Right $ minimum xs
    op xs = typeUnmatch xs

copMax :: Op.VCop
copMax = op where
    op [Right (Op.VList xs)] = Right $ maximum xs
    op xs = typeUnmatch xs

copLength :: Op.VCop
copLength = op where
    op [Right (Op.VList xs)]         = Right . C.pDecFromInt $ length xs
    op [Right (Op.VText xs)]         = Right . C.pDecFromInt $ length xs
    op [Right (Op.VRel (B.Rel _ b))] = Right . C.pDecFromInt $ length b
    op xs = typeUnmatch xs

typeUnmatch :: C.PrimContent a => [B.Ab a] -> Either B.AbortReason b
typeUnmatch _ = Left $ B.AbortCalc [] $ B.ACUnmatchType []



-- ----------------------  set-like operation

copAppend :: Op.VCop
copAppend [] = Right Op.VNil
copAppend xs@(x : _) = op x where
    op (Right (Op.VText _)) = C.putText . concat =<< mapM C.getText xs
    op (Right (Op.VSet  _)) = C.putSet  . concat =<< mapM C.getSet  xs
    op (Right (Op.VList _)) = C.putList . concat =<< mapM C.getList xs
    op _ = typeUnmatch xs

copIntersect :: Op.VCop
copIntersect [] = Right Op.VNil
copIntersect xs@(x : _) = op x where
    op (Right (Op.VSet  _)) = C.putSet  . intersectLists =<< mapM C.getSet  xs
    op (Right (Op.VList _)) = C.putList . intersectLists =<< mapM C.getList xs
    op _ = typeUnmatch xs

copMinus :: Op.VCop
copMinus = op where
    op [Right (Op.VSet a),  Right (Op.VSet b)]  = C.putSet  (a List.\\ b)
    op [Right (Op.VList a), Right (Op.VList b)] = C.putList (a List.\\ b)
    op xs = typeUnmatch xs

intersectLists :: (Eq a) => [[a]] -> [a]
intersectLists [] = []
intersectLists [a] = a
intersectLists (a : b : xs) = intersectLists $ List.intersect a b : xs



-- ----------------------  others

copReverse :: Op.VCop
copReverse = op where
    op [Right (Op.VText xs)] = C.putText $ reverse xs
    op [Right (Op.VList xs)] = C.putList $ reverse xs
    op xs = typeUnmatch xs

copSubIndex :: Op.VCop
copSubIndex = op where
    op arg = do arg3 <- C.getArg3 arg
                case arg3 of
                  (Right (Op.VText xs), Right (Op.VDec from), Right (Op.VDec to))
                      -> C.putText (subIndexDecimal from to xs)
                  _ -> typeUnmatch arg

copSubLength :: Op.VCop
copSubLength = op where
    op arg = do arg3 <- C.getArg3 arg
                case arg3 of
                  (Right (Op.VText xs), Right (Op.VDec from), Right (Op.VDec to))
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

