{-# OPTIONS_GHC -Wall #-}

{-| Content operators. -}

module Koshucode.Baala.Vanilla.Cop.List
( copsList
  -- $Operators
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C
import Koshucode.Baala.Vanilla.Type.Content



-- ----------------------
{- $Operators

 [@list@]    Construct list.

 [@total@]   Calculate total amount of elements in list,
             i.e., summation.

 [@length@]  Number of elements.

 [@min@]     Minimal element.

 [@max@]     Maximal element.

 [@++@]      Append lists.

-}

litText :: [B.TokenTree] -> B.AbOr VContent
litText xs =
    do ss <- mapM litT xs
       Right . C.putText $ concat ss

litT :: B.TokenTree -> B.AbOr String
litT (B.TreeL (B.TWord _ _ w)) = Right w
litT x = Left $ B.AbortNotText (show x)

copsList :: [B.Named (C.Cop VContent)]
copsList =
    [ C.namedEager  "list"    copList
    , C.namedEager  "total"   copTotal
    , C.namedEager  "length"  copLength
    , C.namedEager  "min"     copMin
    , C.namedEager  "max"     copMax
    , C.namedEager  "++"      copAppend
    , C.namedLit    "'"       litText
    ]

copList :: VCop
copList = Right . C.putList

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

copAppend :: VCop
copAppend [] = Right VNil
copAppend xxs@(x : _) = op x where
    op (VText _) = Right . C.putText . concat =<< mapM C.needText xxs
    op (VList _) = Right . C.putList . concat =<< mapM C.needList xxs
    op (VSet  _) = Right . C.putSet  . concat =<< mapM C.needSet  xxs
    op _ = Left $ B.AbortUnmatchType (concatMap C.typename xxs)

