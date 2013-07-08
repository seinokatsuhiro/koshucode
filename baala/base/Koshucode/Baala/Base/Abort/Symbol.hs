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
    | AbortMalformedTerms   [SourceLine] String
    | AbortMissingTermName  [SourceLine] String
    | AbortRequireFlatname  [SourceLine] String
    | AbortUnknownClause    [SourceLine]
    | AbortUnknownContent   [SourceLine] String
    | AbortUnknownRelmap    [SourceLine] String
    | AbortUsage            [SourceLine] String [String]
      deriving (Show, Eq, Ord)

instance Name Abort where
    name = abortSymbol

instance AbortSymbol Abort where
    abortSymbol = head . words . show

    abortTitle a = case a of
        (AbortLookup _ _)             -> "項目がない"
        (AbortMalformedOperand _ _)   -> "演算子の引数がおかしい"
        (AbortMalformedTerms _ _)     -> "項目名と項目内容の組がない"
        (AbortMissingTermName _ _)    -> "項目名が必要"
        (AbortRequireFlatname _ _)    -> "入れ子ではない項目名が必要"
        (AbortUnknownClause _)        -> "未知の構文"
        (AbortUnknownContent _ _)     -> "未知の項目内容"
        (AbortUnknownRelmap _ _)      -> "未知の演算子"
        (AbortUsage _ _ _)            -> "使用法の間違い"

    abortMain a = case a of
        (AbortLookup _ s)             -> par s
        (AbortMalformedOperand _ s)   -> par s
        (AbortMalformedTerms _ s)     -> par s
        (AbortMissingTermName _ s)    -> par s
        (AbortRequireFlatname _ s)    -> par s
        (AbortUnknownClause _)        -> empty
        (AbortUnknownContent _ s)     -> par s
        (AbortUnknownRelmap _ s)      -> par s
        (AbortUsage _ _ usage)        -> docv $ map par usage

    abortLines a = case a of
        (AbortLookup           ln _)   -> ln
        (AbortMalformedOperand ln _)   -> ln
        (AbortMalformedTerms   ln _)   -> ln
        (AbortMissingTermName  ln _)   -> ln
        (AbortRequireFlatname  ln _)   -> ln
        (AbortUnknownClause    ln)     -> ln
        (AbortUnknownContent   ln _)   -> ln
        (AbortUnknownRelmap    ln _)   -> ln
        (AbortUsage            ln _ _) -> ln

par :: String -> Doc
par = fsep . map text . words

