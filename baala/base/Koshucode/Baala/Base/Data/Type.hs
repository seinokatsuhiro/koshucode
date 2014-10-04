{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Data.Type
  ( Type (..),
  ) where

import qualified Koshucode.Baala.Base.Prelude  as B
import qualified Koshucode.Baala.Base.Text     as B
import qualified Koshucode.Baala.Base.Token    as B

data Type
    = TypeType                           -- ^ Type of types
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
    write = writeType

writeType :: B.StringMap -> Type -> B.Doc
writeType _ = wf where
    wt = w True
    wf = w False

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

