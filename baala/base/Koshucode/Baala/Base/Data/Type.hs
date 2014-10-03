{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Data.Type
  ( Type (..),
  ) where

import qualified Koshucode.Baala.Base.Prelude  as B
import qualified Koshucode.Baala.Base.Text     as B
import qualified Koshucode.Baala.Base.Token    as B

data Type
    = TypeEmpty                          -- ^ Empty
    | TypeBool                           -- ^ Boolean
    | TypeText                           -- ^ Text
    | TypeCode                           -- ^ Code
    | TypeDec                            -- ^ Decimal
    | TypeDate                           -- ^ Date
    | TypeTime                           -- ^ Time
    | TypeBin                            -- ^ Binary data
    | TypeTerm                           -- ^ Term name
    | TypeType                           -- ^ Type of types
    | TypeInterp                         -- ^ Data interpreation

    | TypeTag     String Type            -- ^ Tagged type
    | TypeList    Type                   -- ^ List
    | TypeSet     Type                   -- ^ Set
    | TypeTuple   [Type]                 -- ^ Tuple (Product type)
    | TypeAssn    [(B.TermName, Type)]   -- ^ Association
    | TypeRel     [(B.TermName, Type)]   -- ^ Relation
    | TypeSum     [Type]                 -- ^ Sum type
      deriving (Show, Eq, Ord)

instance B.Write Type where
    write = writeType

writeType :: B.StringMap -> Type -> B.Doc
writeType _ = wf where
    wt = w True
    wf = w False

    w _ TypeEmpty         =  B.doc "empty"
    w _ TypeBool          =  B.doc "boolean"
    w _ TypeText          =  B.doc "text"
    w _ TypeCode          =  B.doc "code"
    w _ TypeDec           =  B.doc "decimal"
    w _ TypeDate          =  B.doc "date"
    w _ TypeTime          =  B.doc "time"
    w _ TypeBin           =  B.doc "binary"
    w _ TypeTerm          =  B.doc "term"
    w _ TypeType          =  B.doc "type"
    w _ TypeInterp        =  B.doc "interp"

    w _ (TypeTag tag t)   =  B.doc (tag ++ ":") B.<+> wt t
    w _ (TypeList   t)    =  B.doc "list"  B.<+> wt t
    w _ (TypeSet    t)    =  B.doc "set"   B.<+> wt t
    w _ (TypeTuple ts)    =  B.doc "tuple" B.<+> B.writeBar id ts
    w _ (TypeAssn  ts)    =  B.doc "assn"  B.<+> B.writeTerms wt ts
    w _ (TypeRel   ts)    =  B.doc "rel"   B.<+> B.writeTerms wt ts

    w False (TypeSum ts)  =  bar ts
    w True  (TypeSum ts)  =  B.docWraps "(" ")" $ bar ts

    bar ts                =  B.doch $ B.writeSepsWith wf "|" ts

