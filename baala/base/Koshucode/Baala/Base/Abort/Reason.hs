{-# OPTIONS_GHC -Wall #-}

{-| Abort symbol -}

module Koshucode.Baala.Base.Abort.Reason
( -- * Datatype
  AbOr,
  AbMap,
  AbMap2,
  Abort,
  AbortOr,
  (<!!>),

  -- * Reason
  AbortReason (..),
) where

import Koshucode.Baala.Base.Prelude

import Koshucode.Baala.Base.Abort.Utility



-- ---------------------- abort type

{-| Either of (1) right result, or (2) abort reason
    (without source code information). -}
type AbOr b = Either AbortReason b

{-| Abortable mapping. -}
type AbMap b = b -> AbOr b

type AbMap2 b a = b -> AbOr a

{-| Abort reason and source information. -}
type Abort = AbortType AbortReason

{-| Either of (1) right result, or (2) abort reason with source. -}
type AbortOr b = AbortOrType AbortReason b



-- ---------------------- utility

{-| Lookup association list.
    This function may abort on AbortLookup. -}
(<!!>) :: [Named a] -> String -> AbortOr a
(<!!>) assoc key = loop assoc where
    loop [] = Left (AbortLookup key, [])
    loop ((k,v) : kvs) | k == key  = Right v
                       | otherwise = loop kvs


-- ---------------------- abort reason

{-| Abort reasons -}
data AbortReason
    = AbortLookup           String
    | AbortMalformedOperand String
    | AbortMissingTermName  String
    | AbortNotNumber        String
    | AbortNotText          String
    | AbortNoTerm           String
    | AbortOddRelation      
    | AbortReqBoolean       String
    | AbortRequireFlatname  String
    | AbortUndefined        String
    | AbortUnkCop           String
    | AbortUnknownClause    
    | AbortUnknownContent   String
    | AbortUnknownRelmap    String
    | AbortUnknownSymbol    String
    | AbortUnmatchArity
    | AbortUnmatchType      String
    | AbortUsage            String [String]
      deriving (Show, Eq, Ord)

instance Name AbortReason where
    name = abortSymbol

instance AbortReasonClass AbortReason where
    abortSymbol = head . words . show

    abortTitle a = case a of
        (AbortLookup           _)   -> "項目がない"
        (AbortMalformedOperand _)   -> "演算子の引数がおかしい"
        (AbortMissingTermName  _)   -> "項目名が必要"
        (AbortNotNumber        _)   -> "数値として読めない"
        (AbortNotText          _)   -> "テキストではない"
        (AbortNoTerm           _)   -> "項目がない"
        (AbortOddRelation       )   -> "ふぞろいな関係"
        (AbortReqBoolean       _)   -> "真か偽が必要"
        (AbortRequireFlatname  _)   -> "入れ子ではない項目名が必要"
        (AbortUndefined        _)   -> "Undefined"
        (AbortUnkCop           _)   -> "未知の項目演算子"
        (AbortUnknownClause     )   -> "未知の構文"
        (AbortUnknownContent   _)   -> "未知の項目内容"
        (AbortUnknownRelmap    _)   -> "未知の演算子"
        (AbortUnknownSymbol    _)   -> "未知の記号"
        (AbortUnmatchArity      )   -> "引数の数が合わない"
        (AbortUnmatchType      _)   -> "型が合わない"
        (AbortUsage          _ _)   -> "使用法の間違い"

    abortMain a = case a of
        (AbortLookup           s)   -> par s
        (AbortMalformedOperand s)   -> par s
        (AbortMissingTermName  s)   -> par s
        (AbortNotNumber        s)   -> par s
        (AbortNotText          s)   -> par s
        (AbortNoTerm           s)   -> par s
        (AbortOddRelation       )   -> empty
        (AbortReqBoolean       s)   -> par s
        (AbortRequireFlatname  s)   -> par s
        (AbortUnkCop           s)   -> par s
        (AbortUnknownClause)        -> empty
        (AbortUnknownContent   s)   -> par s
        (AbortUnknownRelmap    s)   -> par s
        (AbortUnknownSymbol    s)   -> par s
        (AbortUsage      _ usage)   -> docv $ map par usage
        (AbortUndefined        s)   -> par s
        (AbortUnmatchArity      )   -> empty
        (AbortUnmatchType      s)   -> par s

par :: String -> Doc
par = fsep . map text . words

