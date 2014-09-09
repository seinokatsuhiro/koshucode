{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Data.Interp
( Interp (..),
  InterpPhrase (..),
) where

import qualified Koshucode.Baala.Base.Prelude      as B
import qualified Koshucode.Baala.Base.Text         as B

data Interp = Interp [InterpPhrase]
    deriving (Show, Eq, Ord)

data InterpPhrase
    = InterpText String
    | InterpTerm String
    deriving (Show, Eq, Ord)

instance B.Write Interp where
    write = interpDoc

instance B.Write InterpPhrase where
    write = interpPhraseDoc

interpDoc :: B.StringMap -> Interp -> B.Doc
interpDoc sh (Interp xs) = B.doc "<<<" B.<+> xsDoc B.<+> B.doc ">>>" where
    xsDoc = B.doch $ map (B.write sh) xs

interpPhraseDoc :: B.StringMap -> InterpPhrase -> B.Doc
interpPhraseDoc _ (InterpText w) = B.doc w
interpPhraseDoc _ (InterpTerm n) = B.doc $ '/' : n

