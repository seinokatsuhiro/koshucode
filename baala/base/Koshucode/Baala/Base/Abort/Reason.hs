{-# OPTIONS_GHC -Wall #-}

{-| Abort symbol -}

module Koshucode.Baala.Base.Abort.Reason
( -- * Datatype
  Ab,
  AbMap,
  AbMap2,
  Abort,
  AbortTokens,
  AbortOr,
  (<!!>),

  -- * Reason
  AbortReason (..),
) where

import qualified Text.PrettyPrint as D
import qualified Koshucode.Baala.Base.Prelude       as B
import qualified Koshucode.Baala.Base.Syntax        as B
import qualified Koshucode.Baala.Base.Abort.Utility as B



-- ---------------------- abort type

type AbortTokens b = Either (AbortReason, [B.Token]) b

{-| Either of (1) right result, or (2) abort reason
    (without source code information). -}
type Ab b = Either AbortReason b

{-| Abortable mapping. -}
type AbMap b = b -> Ab b

type AbMap2 b a = b -> Ab a

{-| Abort reason and source information. -}
type Abort = B.AbortType AbortReason

{-| Either of (1) right result, or (2) abort reason with source. -}
type AbortOr b = B.AbortOrType AbortReason b



-- ---------------------- utility

{-| Lookup association list.
    This function may abort on AbortLookup. -}
(<!!>) :: [B.Named a] -> String -> AbortTokens a
(<!!>) assoc key = loop assoc where
    loop [] = Left (AbortLookup key, [])
    loop ((k,v) : kvs) | k == key  = Right v
                       | otherwise = loop kvs


-- ---------------------- abort reason

{-| Abort reasons -}
data AbortReason
    = AbortDivideByZero
    | AbortHeteroDecimal    String String
    | AbortLookup           String
    | AbortMalformedOperand String
    | AbortMissingTermname  String
    | AbortNotNumber        String
    | AbortNotText          String
    | AbortNoTerms          [String]
    | AbortOddRelation      
    | AbortOpeandDuplicate  [String]
    | AbortOpeandUnknown    [String]
    | AbortOpeandUnmatch    String
    | AbortReqBoolean       String
    | AbortReqFlatname      String
    | AbortReqNewTerms      [String]
    | AbortReqText          String
    | AbortUndefined        String
    | AbortUnkCop           String
    | AbortUnkCox           String
    | AbortUnkWord          String
    | AbortUnkClause    
    | AbortUnkContent       String
    | AbortUnkRelmap        String
    | AbortUnkSymbol        String
    | AbortUnresToken
    | AbortUnmatchArity
    | AbortUnmatchType      String
    | AbortUsage            String [String]
      deriving (Show, Eq, Ord)

instance B.Name AbortReason where
    name = B.abortSymbol

instance B.AbortReasonClass AbortReason where
    abortSymbol = head . words . show

    abortTitle a = case a of
        (AbortDivideByZero      ) -> "ゼロで割れない"
        (AbortHeteroDecimal  _ _) -> "小数の桁数が合わない"
        (AbortLookup           _) -> "項目がない"
        (AbortMalformedOperand _) -> "演算子の引数がおかしい"
        (AbortMissingTermname  _) -> "項目名が必要"
        (AbortNotNumber        _) -> "数値として読めない"
        (AbortNotText          _) -> "テキストではない"
        (AbortNoTerms          _) -> "項目がない"
        (AbortOddRelation       ) -> "ふぞろいな関係"
        (AbortOpeandDuplicate  _) -> "同名の引数が重複"
        (AbortOpeandUnknown    _) -> "未知の引数"
        (AbortOpeandUnmatch    _) -> "引数が不一致"
        (AbortReqBoolean       _) -> "真か偽が必要"
        (AbortReqFlatname      _) -> "入れ子ではない項目名が必要"
        (AbortReqNewTerms      _) -> "新しい項目が必要"
        (AbortReqText          _) -> "テキストが必要"
        (AbortUndefined        _) -> "Undefined"
        (AbortUnkCop           _) -> "未知の項目演算子"
        (AbortUnkCox           _) -> "扱い方がわからない式"
        (AbortUnkWord          _) -> "扱い方がわからない単語"
        (AbortUnkClause         ) -> "未知の構文"
        (AbortUnkContent       _) -> "未知の項目内容"
        (AbortUnkRelmap        _) -> "未知の演算子"
        (AbortUnkSymbol        _) -> "未知の記号"
        (AbortUnresToken        ) -> "未知の略号"
        (AbortUnmatchArity      ) -> "引数の数が合わない"
        (AbortUnmatchType      _) -> "型が合わない"
        (AbortUsage          _ _) -> "使用法の間違い"

    abortMain a = case a of
        (AbortDivideByZero      ) -> B.docEmpty
        (AbortHeteroDecimal  x y) -> B.doc $ x ++ " : " ++ y
        (AbortLookup           s) -> par s
        (AbortMalformedOperand s) -> par s
        (AbortMissingTermname  s) -> par s
        (AbortNotNumber        s) -> par s
        (AbortNotText          s) -> par s
        (AbortNoTerms         ns) -> B.doch ns
        (AbortOddRelation       ) -> B.docEmpty
        (AbortOpeandDuplicate ns) -> B.doch ns
        (AbortOpeandUnknown   ns) -> B.doch ns
        (AbortOpeandUnmatch    s) -> par s
        (AbortReqBoolean       s) -> par s
        (AbortReqFlatname      s) -> par s
        (AbortReqNewTerms     ns) -> B.doch ns
        (AbortReqText          s) -> par s
        (AbortUnkCop           s) -> par s
        (AbortUnkCox           s) -> par s
        (AbortUnkWord          s) -> par s
        (AbortUnkClause         ) -> B.docEmpty
        (AbortUnkContent       s) -> par s
        (AbortUnkRelmap        s) -> par s
        (AbortUnkSymbol        s) -> par s
        (AbortUnresToken        ) -> B.docEmpty
        (AbortUsage      _ usage) -> B.docv $ map par usage
        (AbortUndefined        s) -> par s
        (AbortUnmatchArity      ) -> B.docEmpty
        (AbortUnmatchType      s) -> par s

    abortSub a = case a of
        (AbortUnkWord          _)
            -> par "テキストは 'aaa のように書きます"
        _   -> B.docEmpty

par :: String -> B.Doc
par = D.fsep . map B.doc . words

