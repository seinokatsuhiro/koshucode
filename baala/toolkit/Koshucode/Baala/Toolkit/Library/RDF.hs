{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

{-| RDF-to-Koshucode conversion -}

module Koshucode.Baala.Toolkit.Library.RDF
( judgesFromRdf,
  judgeFromTriple,
  RDFTupleType (..),
) where

import Data.RDF
import Koshucode.Baala.Base.Data
import Koshucode.Baala.Core.Content.Class
import qualified Data.Text as T

-- | Type of conversion
data RDFTupleType
    = RDFTuple2   -- ^ Binary relation: @\/s@ subject and @\/o@ object.
    | RDFTuple3   -- ^ Ternary relation: @\/s@ subject, @\/p@ predicate, and @\/o@ object.
      deriving (Show, Eq, Ord, Enum, Bounded)

-- | Convert RDF graph to list of judges.
judgesFromRdf :: (RDF g, CString v) => RDFTupleType -> g -> [Judge v]
judgesFromRdf k g = map (judgeFromTriple k) $ triplesOf g

-- | Convert RDF triple to affirmed judge.
judgeFromTriple :: (CString v) => RDFTupleType -> Triple -> Judge v
judgeFromTriple RDFTuple2 (Triple s p o) =
    Judge True (nodeString p) [("/s", the s), ("/o", the o)]
judgeFromTriple RDFTuple3 (Triple s p o) =
    Judge True "RDF" [("/s", the s), ("/p", the p), ("/o", the o)]

the :: (CString v) => Node -> v
the = putString . nodeString

nodeString :: Node -> String
nodeString = T.unpack . nodeText

nodeText :: Node -> T.Text
nodeText (UNode t)    = t
nodeText (BNode t)    = t
nodeText (BNodeGen _) = ""
nodeText (LNode v)    = literalText v

literalText :: LValue -> T.Text
literalText (PlainL  t)   = t
literalText (PlainLL t _) = t
literalText (TypedL  t _) = t

