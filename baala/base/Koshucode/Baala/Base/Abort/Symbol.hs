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

{- | Abortable type -}
type AbortOr a = Either Abort a

{- | Abort symbols -}
data Abort
    = AbortLookup           [SourceLine] String
    | AbortMalformedOperand [SourceLine] String
    | AbortMalformedTerms   [SourceLine] String
    | AbortMissingTermName  [SourceLine] String
    | AbortUnknownClause    [SourceLine]
    | AbortUnknownRelmap    [SourceLine] String
    | AbortUsage            [SourceLine] [String]
      deriving (Show, Eq, Ord)

instance Name Abort where
    name = abortSymbol

instance AbortSymbol Abort where
    abortSymbol = head . words . show

    abortTitle a = case a of
        (AbortLookup _ _)             -> "項目がない"
        (AbortMalformedOperand _ _)   -> "演算子の引数がおかしい"
        (AbortMalformedTerms _ _)     -> "項目名と項目内容の並びがおかしい"
        (AbortMissingTermName _ _)    -> "項目名ではない記号"
        (AbortUnknownClause _)        -> "未知の構文"
        (AbortUnknownRelmap _ _)      -> "未知の演算子"
        (AbortUsage _ _)              -> "使用法の間違い"

    abortMain a = case a of
        (AbortLookup _ s)             -> par s
        (AbortMalformedOperand _ s)   -> par s
        (AbortMalformedTerms _ s)     -> par s
        (AbortMissingTermName _ s)    -> par s
        (AbortUnknownClause _)        -> empty
        (AbortUnknownRelmap _ s)      -> par s
        (AbortUsage _ ss)             -> docv $ map par ss

    abortLines a = case a of
        (AbortLookup ln _)            -> ln
        (AbortMalformedOperand ln _)  -> ln
        (AbortMalformedTerms ln _)    -> ln
        (AbortMissingTermName ln _)   -> ln
        (AbortUnknownClause ln)       -> ln
        (AbortUnknownRelmap ln _)     -> ln
        (AbortUsage ln _)             -> ln

par :: String -> Doc
par = fsep . map text . words

