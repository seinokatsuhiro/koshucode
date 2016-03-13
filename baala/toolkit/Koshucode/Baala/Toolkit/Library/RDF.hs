{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | RDF-to-Koshucode conversion

module Koshucode.Baala.Toolkit.Library.RDF
  ( judgesFromRdf,
    judgeFromTriple,
    RDFTupleType (..),
  ) where

import qualified Data.RDF             as RDF
import qualified Data.Text            as Tx
import qualified Koshucode.Baala.Data as D

-- | Type of conversion
data RDFTupleType
    = RDFTuple2   -- ^ Binary relation: @\/s@ subject and @\/o@ object.
    | RDFTuple3   -- ^ Ternary relation: @\/s@ subject, @\/p@ predicate, and @\/o@ object.
      deriving (Show, Eq, Ord, Enum, Bounded)

-- | Convert RDF graph to list of judges.
judgesFromRdf :: (RDF.RDF g, D.CText c) => RDFTupleType -> g -> [D.Judge c]
judgesFromRdf k g = map (judgeFromTriple k) $ RDF.triplesOf g

-- | Convert RDF triple to affirmed judge.
judgeFromTriple :: (D.CText c) => RDFTupleType -> RDF.Triple -> D.Judge c
judgeFromTriple RDFTuple2 (RDF.Triple s p o) =
    D.affirm (nodeString p) [("s", the s), ("o", the o)]
judgeFromTriple RDFTuple3 (RDF.Triple s p o) =
    D.affirm "RDF" [("s", the s), ("p", the p), ("o", the o)]

the :: (D.CText c) => RDF.Node -> c
the = D.pText . nodeString

nodeString :: RDF.Node -> String
nodeString = Tx.unpack . nodeText

nodeText :: RDF.Node -> Tx.Text
nodeText (RDF.UNode t)    = t
nodeText (RDF.BNode t)    = t
nodeText (RDF.BNodeGen _) = ""
nodeText (RDF.LNode c)    = literalText c

literalText :: RDF.LValue -> Tx.Text
literalText (RDF.PlainL  t)   = t
literalText (RDF.PlainLL t _) = t
literalText (RDF.TypedL  t _) = t

