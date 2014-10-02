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
    | TypeRel   [(B.TermName, Type)]
    | TypeSum   [Type]
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
    w _ (TypeRel ts)      =  B.doc "rel" B.<+> B.writeTerms wt ts
    w False (TypeSum ts)  =  bar ts
    w True  (TypeSum ts)  =  B.docWraps "(" ")" $ bar ts

    bar ts                =  B.doch $ B.writeSepsWith wf "|" ts

