{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

{-| RDF-to-Koshucode conversion -}

module Koshucode.Baala.Toolkit.Library.RDF
( judgesFromRdf,
  judgeFromTriple,
  RDFTupleType (..),
) where

import Data.RDF
import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C
import qualified Data.Text as T

-- | Type of conversion
data RDFTupleType
    = RDFTuple2   -- ^ Binary relation: @\/s@ subject and @\/o@ object.
    | RDFTuple3   -- ^ Ternary relation: @\/s@ subject, @\/p@ predicate, and @\/o@ object.
      deriving (Show, Eq, Ord, Enum, Bounded)

-- | Convert RDF graph to list of judges.
judgesFromRdf :: (RDF g, C.CText c) => RDFTupleType -> g -> [B.Judge c]
judgesFromRdf k g = map (judgeFromTriple k) $ triplesOf g

-- | Convert RDF triple to affirmed judge.
judgeFromTriple :: (C.CText c) => RDFTupleType -> Triple -> B.Judge c
judgeFromTriple RDFTuple2 (Triple s p o) =
    B.Judge True (nodeString p) [("/s", the s), ("/o", the o)]
judgeFromTriple RDFTuple3 (Triple s p o) =
    B.Judge True "RDF" [("/s", the s), ("/p", the p), ("/o", the o)]

the :: (C.CText c) => Node -> c
the = C.putText . nodeString

nodeString :: Node -> String
nodeString = T.unpack . nodeText

nodeText :: Node -> T.Text
nodeText (UNode t)    = t
nodeText (BNode t)    = t
nodeText (BNodeGen _) = ""
nodeText (LNode c)    = literalText c

literalText :: LValue -> T.Text
literalText (PlainL  t)   = t
literalText (PlainLL t _) = t
literalText (TypedL  t _) = t

