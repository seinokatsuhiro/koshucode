{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Data.Type.Interp
  ( Interp (..),
    InterpWord (..),
    interp,
  ) where

import qualified Koshucode.Baala.Base              as B
import qualified Koshucode.Baala.Syntax            as S

data Interp =
    Interp { interpWords :: [InterpWord]
           , interpTerms :: [S.TermName]
           } deriving (Show, Eq, Ord)

data InterpWord
    = InterpText String
    | InterpTerm S.TermName
    deriving (Show, Eq, Ord)

instance B.MixShortEncode Interp where
    mixShortEncode sh = B.mixShow . interpDoc sh

instance B.Write Interp where
    writeDocWith = interpDoc

instance B.Write InterpWord where
    writeDocWith = interpWordDoc

interpDoc :: B.Shorten -> Interp -> B.Doc
interpDoc sh Interp { interpWords = xs } = doc where
    doc    = B.doc S.interpOpen B.<+> xsDoc B.<+> B.doc S.interpClose
    xsDoc  = B.doch $ map (B.writeDocWith sh) xs

interpWordDoc :: B.Shorten -> InterpWord -> B.Doc
interpWordDoc _ (InterpText w) = B.doc w
interpWordDoc _ (InterpTerm n) = B.doc $ '/' : n

interp :: [InterpWord] -> Interp
interp ws = intp where
    terms = B.unique $ B.catMaybes $ map getTermName ws
    intp  = Interp { interpWords = ws
                   , interpTerms = terms }

getTermName :: InterpWord -> Maybe S.TermName
getTermName (InterpTerm n) = Just n
getTermName _              = Nothing

