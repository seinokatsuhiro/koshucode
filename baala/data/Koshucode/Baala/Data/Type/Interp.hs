{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Data.Type.Interp
  ( Interp (..),
    InterpWord (..),
    interp,
  ) where

import qualified Koshucode.Baala.Base              as B
import qualified Koshucode.Baala.Data.Token        as D

data Interp =
    Interp { interpWords :: [InterpWord]
           , interpTerms :: [D.TermName]
           } deriving (Show, Eq, Ord)

data InterpWord
    = InterpText String
    | InterpTerm D.TermName
    deriving (Show, Eq, Ord)

instance B.Write Interp where
    writeDocWith = interpDoc

instance B.Write InterpWord where
    writeDocWith = interpWordDoc

interpDoc :: B.Shortener -> Interp -> B.Doc
interpDoc sh Interp { interpWords = xs } = doc where
    doc    = B.doc D.interpOpen B.<+> xsDoc B.<+> B.doc D.interpClose
    xsDoc  = B.doch $ map (B.writeDocWith sh) xs

interpWordDoc :: B.Shortener -> InterpWord -> B.Doc
interpWordDoc _ (InterpText w) = B.doc w
interpWordDoc _ (InterpTerm n) = B.doc $ '/' : n

interp :: [InterpWord] -> Interp
interp ws = intp where
    terms = B.unique $ B.catMaybes $ map getTermName ws
    intp  = Interp { interpWords = ws
                   , interpTerms = terms }

getTermName :: InterpWord -> Maybe D.TermName
getTermName (InterpTerm n) = Just n
getTermName _              = Nothing

