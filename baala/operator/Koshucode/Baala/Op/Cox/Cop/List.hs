{-# OPTIONS_GHC -Wall #-}

-- | Content operators.

module Koshucode.Baala.Op.Cox.Cop.List
( copsList
  -- $Operators
) where

import qualified Data.List                     as List
import qualified Data.Char                     as Char
import qualified Koshucode.Baala.Base          as B
import qualified Koshucode.Baala.Core          as C
import qualified Koshucode.Baala.Op.Message    as Message



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

copsList :: (C.CContent c) => [C.Cop c]
copsList =
    [ C.CopFun  (C.copInfix "++")           copAppend
    , C.CopFun  (C.copInfix "intersect")    copIntersect
    , C.CopFun  "++"            copAppend
    , C.CopFun  "char"          copChar
    , C.CopFun  "char-group"    copCharGroup
    , C.CopFun  "char-group-1"  copCharGroup1
    , C.CopFun  "intersect"     copIntersect
    , C.CopFun  "length"        copLength
    , C.CopFun  "list"          copList
    , C.CopFun  "max"           copMax
    , C.CopFun  "min"           copMin
    , C.CopFun  "minus"         copMinus
    , C.CopFun  "push"          copPush
    , C.CopFun  "reverse"       copReverse
    , C.CopFun  "total"         copTotal
    , C.CopFun  "sub-index"     copSubIndex
    , C.CopFun  "sub-length"    copSubLength
    ]

copList :: (C.CList c) => C.CopFun c
copList argC = do arg <- sequence argC
                  C.putList arg


-- ----------------------  aggregation

copTotal :: (C.CContent c) => C.CopFun c
copTotal = op where
    op [Right c] | C.isList c = C.putDec =<< B.decimalSum (map C.gDec $ C.gList c)
    op xs = typeUnmatch xs

copMin :: (C.CContent c) => C.CopFun c
copMin = op where
    op [Right c] | C.isList c = Right $ minimum (C.gList c)
    op xs = typeUnmatch xs

copMax :: (C.CContent c) => C.CopFun c
copMax = op where
    op [Right c] | C.isList c = Right $ maximum (C.gList c)
    op xs = typeUnmatch xs

copLength :: (C.CContent c) => C.CopFun c
copLength = op where
    op [Right c] | C.isList c  = Right . C.pDecFromInt $ length (C.gList c)
                 | C.isText c  = Right . C.pDecFromInt $ length (C.gText c)
                 | C.isRel c   = Right . C.pDecFromInt $ length (B.relBody $ C.gRel c)
    op xs = typeUnmatch xs

typeUnmatch :: C.PrimContent a => [B.Ab a] -> B.Ab b
typeUnmatch _ = Message.unmatchType ""



-- ----------------------  set-like operation

copAppend :: (C.CContent c) => C.CopFun c
copAppend [] = Right C.empty
copAppend xs@(x : _) = op x where
    op (Right c) | C.isText c = C.putText . concat =<< mapM C.getText xs
                 | C.isSet  c = C.putSet  . concat =<< mapM C.getSet  xs
                 | C.isList c = C.putList . concat =<< mapM C.getList xs
    op _ = typeUnmatch xs

copIntersect :: (C.CContent c) => C.CopFun c
copIntersect [] = Right C.empty
copIntersect xs@(x : _) = op x where
    op (Right c) | C.isSet  c = C.putSet  . intersectLists =<< mapM C.getSet  xs
                 | C.isList c = C.putList . intersectLists =<< mapM C.getList xs
    op _ = typeUnmatch xs

copMinus :: (C.CContent c) => C.CopFun c
copMinus = op where
    op [Right a,  Right b]
        | C.isSet  a && C.isSet  b = C.putSet  (C.gSet  a List.\\ C.gSet  b)
        | C.isList a && C.isList b = C.putList (C.gList a List.\\ C.gList b)
    op xs = typeUnmatch xs

intersectLists :: (Eq a) => [[a]] -> [a]
intersectLists [] = []
intersectLists [a] = a
intersectLists (a : b : xs) = intersectLists $ List.intersect a b : xs



-- ----------------------  others

copReverse :: (C.CContent c) => C.CopFun c
copReverse = op where
    op [Right c] | C.isText c = C.putText $ reverse (C.gText c)
                 | C.isList c = C.putList $ reverse (C.gList c)
    op xs = typeUnmatch xs

copSubIndex :: (C.CContent c) => C.CopFun c
copSubIndex = op where
    op arg = do arg3 <- C.getArg3 arg
                case arg3 of
                  (Right xs', Right from', Right to')
                      | C.isText xs' && C.isDec from' && C.isDec to'
                      -> let xs   = C.gText xs'
                             from = C.gDec from'
                             to   = C.gDec to'
                         in C.putText (subIndexDecimal from to xs)
                  _ -> typeUnmatch arg

copSubLength :: (C.CContent c) => C.CopFun c
copSubLength = op where
    op arg = do arg3 <- C.getArg3 arg
                case arg3 of
                  (Right xs', Right from', Right to')
                      | C.isText xs' && C.isDec from' && C.isDec to'
                      -> let xs   = C.gText xs'
                             from = C.gDec from'
                             to   = C.gDec to'
                         in C.putText (subLengthDecimal from to xs)
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

copPush :: (C.CContent c) => C.CopFun c
copPush arg =
    do arg2 <- C.getArg2 arg
       case arg2 of
         (Right c, Right cs)
             | C.isSet cs -> C.putSet $ c : C.gSet cs
             | otherwise  -> Message.reqCollection
         _ -> typeUnmatch arg


-- ----------------------  text

copChar :: (C.CContent c) => C.CopFun c
copChar = op where
    op [Right c] | C.isDec c = C.putText [Char.chr $ B.decimalNum $ C.gDec c]
    op xs = typeUnmatch xs

copCharGroup :: (C.CContent c) => C.CopFun c
copCharGroup = op where
    op [Right t] | C.isText t = C.putList $ map (C.pText . charGroup) $ C.gText t
    op xs = typeUnmatch xs

copCharGroup1 :: (C.CContent c) => C.CopFun c
copCharGroup1 = op where
    op [Right t] | C.isText t = case C.gText t of
                                  (c : _) -> C.putText $ charGroup c
                                  _       -> Right C.empty
    op xs = typeUnmatch xs

charGroup :: Char -> String
charGroup = B.generalCategoryName . B.generalCategoryGroup

