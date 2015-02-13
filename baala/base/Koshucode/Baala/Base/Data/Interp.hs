{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Data.Interp
  ( Interp (..),
    InterpWord (..),
    interp,
  ) where

import qualified Koshucode.Baala.Base.Prelude      as B
import qualified Koshucode.Baala.Base.Text         as B
import qualified Koshucode.Baala.Base.Token        as B

data Interp =
    Interp { interpWords :: [InterpWord]
           , interpTerms :: [B.TermName]
           } deriving (Show, Eq, Ord)

data InterpWord
    = InterpText String
    | InterpTerm B.TermName
    deriving (Show, Eq, Ord)

instance B.Write Interp where
    write = interpDoc

instance B.Write InterpWord where
    write = interpWordDoc

interpDoc :: B.StringMap -> Interp -> B.Doc
interpDoc sh Interp { interpWords = xs } = doc where
    doc    = B.doc "<<<" B.<+> xsDoc B.<+> B.doc ">>>"
    xsDoc  = B.doch $ map (B.write sh) xs

interpWordDoc :: B.StringMap -> InterpWord -> B.Doc
interpWordDoc _ (InterpText w) = B.doc w
interpWordDoc _ (InterpTerm n) = B.doc $ '/' : n

interp :: [InterpWord] -> Interp
interp ws = intp where
    terms = B.unique $ B.catMaybes $ map getTermName ws
    intp  = Interp { interpWords = ws
                   , interpTerms = terms }

getTermName :: InterpWord -> Maybe B.TermName
getTermName (InterpTerm n) = Just n
getTermName _              = Nothing

