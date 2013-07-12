{-# OPTIONS_GHC -Wall #-}

{- | Abort symbol -}

module Koshucode.Baala.Base.Abort.Symbol
( AbortOr
, Abort (..)
) where

import Koshucode.Baala.Base.Prelude.Pretty
import Koshucode.Baala.Base.Prelude.Utility
import Koshucode.Baala.Base.Syntax

import Koshucode.Baala.Base.Abort.Source



-- ----------------------

{-| Abortable type -}
type AbortOr a = Either Abort a

{-| Abort symbols -}
data Abort
    = AbortLookup           [SourceLine] String
    | AbortMalformedOperand [SourceLine] String
    | AbortMissingTermName  [SourceLine] String
    | AbortNotNumber        [SourceLine] String
    | AbortOddRelation      [SourceLine]
    | AbortRequireFlatname  [SourceLine] String
    | AbortUnknownClause    [SourceLine]
    | AbortUnknownContent   [SourceLine] String
    | AbortUnknownRelmap    [SourceLine] String
    | AbortUnknownSymbol    [SourceLine] String
    | AbortUsage            [SourceLine] String [String]
      deriving (Show, Eq, Ord)

instance Name Abort where
    name = abortSymbol

instance AbortSymbol Abort where
    abortSymbol = head . words . show

    abortTitle a = case a of
        (AbortLookup           _ _)   -> "項目がない"
        (AbortMalformedOperand _ _)   -> "演算子の引数がおかしい"
        (AbortMissingTermName  _ _)   -> "項目名が必要"
        (AbortNotNumber        _ _)   -> "数値として読めない"
        (AbortOddRelation        _)   -> "ふぞろいな関係"
        (AbortRequireFlatname  _ _)   -> "入れ子ではない項目名が必要"
        (AbortUnknownClause      _)   -> "未知の構文"
        (AbortUnknownContent   _ _)   -> "未知の項目内容"
        (AbortUnknownRelmap    _ _)   -> "未知の演算子"
        (AbortUnknownSymbol   _ _)    -> "未知の記号"
        (AbortUsage          _ _ _)   -> "使用法の間違い"

    abortMain a = case a of
        (AbortLookup _ s)             -> par s
        (AbortMalformedOperand _ s)   -> par s
        (AbortMissingTermName _ s)    -> par s
        (AbortNotNumber       _ s)    -> par s
        (AbortOddRelation     _)      -> empty
        (AbortRequireFlatname _ s)    -> par s
        (AbortUnknownClause _)        -> empty
        (AbortUnknownContent _ s)     -> par s
        (AbortUnknownRelmap _ s)      -> par s
        (AbortUnknownSymbol _ s)      -> par s
        (AbortUsage _ _ usage)        -> docv $ map par usage

    abortLines a = case a of
        (AbortLookup           ln _)   -> ln
        (AbortMalformedOperand ln _)   -> ln
        (AbortMissingTermName  ln _)   -> ln
        (AbortNotNumber        ln _)   -> ln
        (AbortOddRelation      ln)     -> ln
        (AbortRequireFlatname  ln _)   -> ln
        (AbortUnknownClause    ln)     -> ln
        (AbortUnknownContent   ln _)   -> ln
        (AbortUnknownRelmap    ln _)   -> ln
        (AbortUnknownSymbol    ln _)   -> ln
        (AbortUsage            ln _ _) -> ln

par :: String -> Doc
par = fsep . map text . words

