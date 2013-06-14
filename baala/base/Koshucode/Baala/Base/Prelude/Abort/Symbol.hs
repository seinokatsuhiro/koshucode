{-# OPTIONS_GHC -Wall #-}

-- | Abort symbol

module Koshucode.Baala.Base.Prelude.Abort.Symbol
( AbortOr
, Abort (..)
, abortTitle
, abortMain
, abortSub
, abortLines
) where
import Koshucode.Baala.Base.Prelude.Pretty
import Koshucode.Baala.Base.Prelude.Utility
import Koshucode.Baala.Base.Prelude.Abort.Source

type AbortOr a = Either Abort a

-- | Abort symbols
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
    name = head . words . show

abortTitle :: Abort -> String
abortTitle (AbortLookup _ _)             = "項目がない"
abortTitle (AbortMalformedOperand _ _)   = "演算子の引数がおかしい"
abortTitle (AbortMalformedTerms _ _)     = "項目名と項目内容の並びがおかしい"
abortTitle (AbortMissingTermName _ _)    = "項目名ではない記号"
abortTitle (AbortUnknownClause _)        = "未知の構文"
abortTitle (AbortUnknownRelmap _ _)      = "未知の演算子"
abortTitle (AbortUsage _ _)              = "使用法の間違い"

abortMain :: Abort -> Doc
abortMain (AbortLookup _ s)           = paragraph s
abortMain (AbortMalformedOperand _ s) = paragraph s
abortMain (AbortMalformedTerms _ s)   = paragraph s
abortMain (AbortMissingTermName _ s)  = paragraph s
abortMain (AbortUnknownClause _)      = empty
abortMain (AbortUnknownRelmap _ s)    = paragraph s
abortMain (AbortUsage _ ss)           = docv $ map paragraph ss

paragraph :: String -> Doc
paragraph = fsep . map text . words

abortSub :: Abort -> Doc
abortSub _ = empty

abortLines :: Abort -> [SourceLine]
abortLines (AbortLookup ln _)           = ln
abortLines (AbortMalformedOperand ln _) = ln
abortLines (AbortMalformedTerms ln _)   = ln
abortLines (AbortMissingTermName ln _)  = ln
abortLines (AbortUnknownClause ln)      = ln
abortLines (AbortUnknownRelmap ln _)    = ln
abortLines (AbortUsage ln _)            = ln


