{-# OPTIONS_GHC -Wall #-}

{-| Abort symbol -}

module Koshucode.Baala.Base.Abort.Symbol
( (<!!>),
  Abort,
  AbortOr,
  AbOr,
  AbortReason (..),
) where

import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Base.Syntax

import Koshucode.Baala.Base.Abort.Source
import Koshucode.Baala.Base.Abort.Utility



-- ----------------------

{-| Lookup association list.
    This function may abort on AbortLookup. -}
(<!!>) :: [(String, a)] -> String -> AbortOr a
(<!!>) assoc key = loop assoc where
    loop [] = Left (AbortLookup key, [])
    loop ((k,v) : kvs) | k == key  = Right v
                       | otherwise = loop kvs

{-| Abort information. -}
type Abort = AbortP AbortReason

{-| Abortable type. -}
type AbortOr b = AbortOrP AbortReason b

{-| Abortable type. -}
type AbOr b = Either ([SourceLine] -> Abort) b

{-| Abort symbols -}
data AbortReason
    = AbortLookup           String
    | AbortMalformedOperand String
    | AbortMissingTermName  String
    | AbortNotNumber        String
    | AbortOddRelation      
    | AbortRequireFlatname  String
    | AbortUnknownClause    
    | AbortUnknownContent   String
    | AbortUnknownRelmap    String
    | AbortUnknownSymbol    String
    | AbortUsage            String [String]
    | AbortUndefined        String
      deriving (Show, Eq, Ord)

instance Name AbortReason where
    name = abortSymbol

instance AbortSymbol AbortReason where
    abortSymbol = head . words . show

    abortTitle a = case a of
        (AbortLookup           _)   -> "項目がない"
        (AbortMalformedOperand _)   -> "演算子の引数がおかしい"
        (AbortMissingTermName  _)   -> "項目名が必要"
        (AbortNotNumber        _)   -> "数値として読めない"
        (AbortOddRelation       )   -> "ふぞろいな関係"
        (AbortRequireFlatname  _)   -> "入れ子ではない項目名が必要"
        (AbortUnknownClause     )   -> "未知の構文"
        (AbortUnknownContent   _)   -> "未知の項目内容"
        (AbortUnknownRelmap    _)   -> "未知の演算子"
        (AbortUnknownSymbol    _)   -> "未知の記号"
        (AbortUsage          _ _)   -> "使用法の間違い"
        (AbortUndefined        _)   -> "Undefined"

    abortMain a = case a of
        (AbortLookup           s)   -> par s
        (AbortMalformedOperand s)   -> par s
        (AbortMissingTermName  s)   -> par s
        (AbortNotNumber        s)   -> par s
        (AbortOddRelation       )   -> empty
        (AbortRequireFlatname  s)   -> par s
        (AbortUnknownClause)        -> empty
        (AbortUnknownContent   s)   -> par s
        (AbortUnknownRelmap    s)   -> par s
        (AbortUnknownSymbol    s)   -> par s
        (AbortUsage      _ usage)   -> docv $ map par usage
        (AbortUndefined        s)   -> par s

par :: String -> Doc
par = fsep . map text . words

