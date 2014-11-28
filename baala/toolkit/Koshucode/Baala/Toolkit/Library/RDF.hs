{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | RDF-to-Koshucode conversion

module Koshucode.Baala.Toolkit.Library.RDF
  ( judgesFromRdf,
    judgeFromTriple,
    RDFTupleType (..),
  ) where

import qualified Data.RDF as RDF
import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C
import qualified Data.Text as T

-- | Type of conversion
data RDFTupleType
    = RDFTuple2   -- ^ Binary relation: @\/s@ subject and @\/o@ object.
    | RDFTuple3   -- ^ Ternary relation: @\/s@ subject, @\/p@ predicate, and @\/o@ object.
      deriving (Show, Eq, Ord, Enum, Bounded)

-- | Convert RDF graph to list of judges.
judgesFromRdf :: (RDF.RDF g, C.CText c) => RDFTupleType -> g -> [B.Judge c]
judgesFromRdf k g = map (judgeFromTriple k) $ RDF.triplesOf g

-- | Convert RDF triple to affirmed judge.
judgeFromTriple :: (C.CText c) => RDFTupleType -> RDF.Triple -> B.Judge c
judgeFromTriple RDFTuple2 (RDF.Triple s p o) =
    B.affirm (nodeString p) [("s", the s), ("o", the o)]
judgeFromTriple RDFTuple3 (RDF.Triple s p o) =
    B.affirm "RDF" [("s", the s), ("p", the p), ("o", the o)]

the :: (C.CText c) => RDF.Node -> c
the = C.pText . nodeString

nodeString :: RDF.Node -> String
nodeString = T.unpack . nodeText

nodeText :: RDF.Node -> T.Text
nodeText (RDF.UNode t)    = t
nodeText (RDF.BNode t)    = t
nodeText (RDF.BNodeGen _) = ""
nodeText (RDF.LNode c)    = literalText c

literalText :: RDF.LValue -> T.Text
literalText (RDF.PlainL  t)   = t
literalText (RDF.PlainLL t _) = t
literalText (RDF.TypedL  t _) = t

