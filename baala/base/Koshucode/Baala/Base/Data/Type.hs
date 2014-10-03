{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Data.Type
( Type (..),
) where

import qualified Koshucode.Baala.Base.Prelude  as B
import qualified Koshucode.Baala.Base.Text     as B
import qualified Koshucode.Baala.Base.Token    as B

data Type
    = TypeEmpty
    | TypeBool
    | TypeText
    | TypeCode
    | TypeDec
    | TypeDate
    | TypeTime
    | TypeBin
    | TypeTerm
    | TypeType
    | TypeInterp
    | TypeRel     [(B.TermName, Type)]
    | TypeSum     [Type]
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
    w _ (TypeRel ts)      =  B.doc "rel" B.<+> B.writeTerms wt ts
    w False (TypeSum ts)  =  bar ts
    w True  (TypeSum ts)  =  B.docWraps "(" ")" $ bar ts

    bar ts                =  B.doch $ B.writeSepsWith wf "|" ts

