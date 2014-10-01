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
    write sh a = case a of
        TypeEmpty    ->  B.doc "empty"
        TypeBool     ->  B.doc "boolean"
        TypeText     ->  B.doc "text"
        TypeRel ts   ->  let he = B.docWraps "{" "}" $ B.writeColon sh ts
                         in B.doc "rel" B.<+> he
        TypeSum ts   ->  B.docWraps "(" ")" $ B.writeBar sh ts

