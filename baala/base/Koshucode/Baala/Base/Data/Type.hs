{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Data.Type
  ( Type (..),
    -- $Types
  ) where

import qualified Koshucode.Baala.Base.Prelude  as B
import qualified Koshucode.Baala.Base.Text     as B
import qualified Koshucode.Baala.Base.Token    as B

data Type
    = TypeAny                            -- ^ Everything
    | TypeType                           -- ^ Type of types
    | TypeTerm                           -- ^ Term name
    | TypeInterp                         -- ^ Data interpreation

    | TypeEmpty                          -- ^ Empty
    | TypeBool                           -- ^ Boolean
    | TypeText                           -- ^ Text
    | TypeCode                           -- ^ Code
    | TypeDec                            -- ^ Decimal
    | TypeDate                           -- ^ Date
    | TypeTime                           -- ^ Time
    | TypeBin                            -- ^ Binary data

    | TypeList    Type                   -- ^ List
    | TypeSet     Type                   -- ^ Set
    | TypeTag     String Type            -- ^ Tagged type
    | TypeAssn    [(B.TermName, Type)]   -- ^ Association
    | TypeRel     [(B.TermName, Type)]   -- ^ Relation

    | TypeTuple   [Type]                 -- ^ Tuple (Product type)
    | TypeSum     [Type]                 -- ^ Sum type
      deriving (Show, Eq, Ord)

instance B.Write Type where
    write _ = writeType

writeType :: Type -> B.Doc
writeType = wf where
    wt = w True
    wf = w False

    w _ TypeAny          =  B.doc "any"
    w _ TypeEmpty        =  B.doc "empty"
    w _ TypeBool         =  B.doc "boolean"
    w _ TypeText         =  B.doc "text"
    w _ TypeCode         =  B.doc "code"
    w _ TypeDec          =  B.doc "decimal"
    w _ TypeDate         =  B.doc "date"
    w _ TypeTime         =  B.doc "time"
    w _ TypeBin          =  B.doc "binary"
    w _ TypeTerm         =  B.doc "term"
    w _ TypeType         =  B.doc "type"
    w _ TypeInterp       =  B.doc "interp"

    w _ (TypeList    t)  =  B.doc "list"  B.<+> wt t
    w _ (TypeSet     t)  =  B.doc "set"   B.<+> wt t
    w _ (TypeTag tag t)  =  B.doc "tag"   B.<+> B.doc (tag ++ ":") B.<+> wt t
    w _ (TypeTuple  ts)  =  B.doc "tuple" B.<+> B.doch (map wt ts)

    w q (TypeAssn   ts)  =  wrap q $ B.doc "assn"  B.<+> B.writeTerms wt ts
    w q (TypeRel    ts)  =  wrap q $ B.doc "rel"   B.<+> B.writeTerms wt ts

    w q (TypeSum ts)     =  wrap q $ B.doch $ B.writeSepsWith wf "|" ts

    wrap False x         =  x
    wrap True  x         =  B.docWraps "(" ")" x



-- ------------------------------------------------------------------
-- $Types
--
--  [Empty]     Empty means that there are no values.
--              i.e., universal negation on the term holds.
--              Textual form is the non-quoted parens: @()@.
--
--  [Boolean]   Boolean used for something is hold or unhold.
--              Textual forms: @\<0\>@ (false), @\<1\>@ (true).
--
--  [Text]      Sequence of characters.
--              Textual forms is chars with apostrophe or
--              doubly-quoted line: @\'abc@, @\"abc def\"@.
--
--  [Decimal]   Decimal number.
--              Textual forms is sequence of digits:
--              @100@, @99.50@, @hex AF@.
--
--  [Set]       Set is an unordered collection of contents.
--              Duplication among contents is not significant.
--              Textual form is a sequence of contents
--              delimited by colon, enclosed in braces:
--              @{ \'a : \'b : \'c }@.
--
--  [List]      List is an ordered list of contents.
--              Textual form is a sequence of contents
--              delimited by colon, enclosed in square brackets:
--              @[ \'abc : \'def ]@.
--
--  [Assn]      Assn is an association of terms,
--              i.e., a list of named contents.
--              Textual form is a sequence of terms
--              with bar-angles: @\<\< \/a 10 \/b 20 \>\>@.
--
--  [Relation]  Relation is a set of same-type tuples,
--              Textual form is a sequence of tuples
--              enclosed in bar-braces.
--              The first tuple is a heading of relation,
--              and succeeding tuples are delimited by vertical bar:
--              @{| \/a : \/b | \'A1 : 20 | \'A3 : 40 |}@.
--
