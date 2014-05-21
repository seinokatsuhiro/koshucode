{-# OPTIONS_GHC -Wall #-}

-- | Heading of relations

module Koshucode.Baala.Base.Data.Relhead
( -- * Type
  Relhead (..),
  headExplain,
  headExplainLines,
  headExplainDoc,

  -- * Constructor
  headEmpty,
  headFrom,
  headWords,
  headNames,
  headDegree,
  -- $ConstructorExample

  -- * Predicate
  headEquiv,
  isSubhead, isSuperhead,

  -- * Add terms
  headConsTerm,
  headCons, headCons2, headCons3,
  headAppend,
  -- $AddTermExample

  -- * Utility
  headChange,
  headRename,
  headIndex1, headIndex, 
  headAlign, bodyAlign,
  headNested,
  -- $UtilityExample
) where

import qualified Data.List                         as L
import qualified Koshucode.Baala.Base.Prelude      as B
import qualified Koshucode.Baala.Base.Text         as B
import qualified Koshucode.Baala.Base.Token        as B
import qualified Koshucode.Baala.Base.Data.Term    as B


-- ---------------------- Type

-- | Heading of relation as a list of terms
data Relhead = Relhead { headTerms :: [B.Term] }
               deriving (Show, Eq, Ord)

instance B.Monoid Relhead where
    mempty = Relhead []
    mappend (Relhead t1) (Relhead t2) =
        Relhead $ B.unionUp t1 t2

instance B.Write Relhead where
    write sh (Relhead ts) = B.writeColon sh $ map (B.showTermName . B.termName) ts

headExplain :: Relhead -> String
headExplain = show . headExplainDoc

headExplainLines :: Relhead -> [String]
headExplainLines = lines . headExplain

headExplainDoc :: Relhead -> B.Doc
headExplainDoc (Relhead ts) = B.docv $ map B.termExplainDoc ts


-- ---------------------- Constructor

-- $ConstructorExample
--
--  /Examples/
--
--  Make heading of @\/a@ @\/b@.
--
--    >>> headFrom ["a", "b"]
--    Relhead [TermFlat "a", TermFlat "b"]
--
--  Show heading.
--
--    >>> B.doc $ headFrom ["a", "b"]
--    /a : /b
--
--  Term names of heading.
--
--    >>> let h = headFrom ["a", "b"] in headNames h
--    ["a", "b"]
--
--  Degree of relation.
--
--    >>> let h = headFrom ["a", "b"] in headDegree h
--    2
--

-- | Empty heading, i.e., no terms in heading.
headEmpty :: Relhead
headEmpty = headFrom []

-- | Make head from term names.
headFrom :: [B.TermName] -> Relhead
headFrom = Relhead . map B.TermFlat

headWords :: String -> Relhead
headWords = headFrom . words

-- | List of term names.
headNames :: Relhead -> [B.TermName]
headNames = B.names . headTerms

-- | Degree of relation, i.e., number of terms.
headDegree :: Relhead -> Int
headDegree = length . headTerms



-- ----------------------  Predicate

headEquiv :: Relhead -> Relhead -> Bool
headEquiv (Relhead a) (Relhead b) = L.sort a == L.sort b

isSubhead :: Relhead -> Relhead -> Bool
isSubhead h1 h2 = null $ headNames h1 `B.snipLeft` headNames h2 

isSuperhead :: Relhead -> Relhead -> Bool
isSuperhead h1 h2 = isSubhead h2 h1



-- ----------------------  Add terms

-- $AddTermExample
--
--  Add term @\/c@ to heading.
--
--    >>> let h = headFrom ["a", "b"] in headCons "c" h
--    Relhead [TermFlat "c", TermFlat "a", TermFlat "b"]
--

headConsTerm :: B.Term -> B.Map Relhead
headConsTerm t1 (Relhead ns) = Relhead $ t1 : ns

-- | Add term to head.
headCons :: B.TermName -> B.Map Relhead
headCons n1 (Relhead ns) =
    Relhead $ B.TermFlat n1 : ns

headCons2 :: B.TermName2 -> B.Map Relhead
headCons2 (n1, n2) (Relhead ns) =
    Relhead $ B.TermFlat n1 : B.TermFlat n2 : ns

headCons3 :: B.TermName3 -> B.Map Relhead
headCons3 (n1, n2, n3) (Relhead ns) =
    Relhead $ B.TermFlat n1 : B.TermFlat n2 : B.TermFlat n3 : ns

-- | Add any number of terms to head.
headAppend :: [B.TermName] -> B.Map Relhead
headAppend ns1 (Relhead ns) = Relhead $ map B.TermFlat ns1 ++ ns



-- ----------------------  Utility

-- $UtilityExample
--
--  /Examples/
--
--  Reverse term names.
--
--    >>> let h = headFrom ["a", "b"] in headChange reverse h
--    Relhead [TermFlat "b", TermFlat "a"]
--
--  Calculate indices of heading.
--
--    >>> let h = headFrom ["a", "b"] in headIndex1 h ["a"]
--    [0]
--    >>> let h = headFrom ["a", "b"] in headIndex h [["a"], ["b"], ["c"]]
--    [[0], [1], [-1]]
--
--  Heading of @\/a@ @\/b@ is a subhead of @\/a@ @\/b@ @\/c@, and @\/a@ @\/e@ is not.
--
--    >>> headFrom ["a", "b"] `isSubhead` headFrom ["a", "b", "c"]
--    True
--    >>> headFrom ["a", "e"] `isSubhead` headFrom ["a", "b", "c"]
--    False
--

-- | Reconstruct head.
headChange :: B.Map [B.Term] -> B.Map Relhead
headChange f = Relhead . f . headTerms

headRename :: B.Map B.TermName -> B.Map Relhead
headRename f = headChange (map $ B.termChange f)

-- | Index of a term.
headIndex1 :: Relhead -> B.TermPath -> [Int]
headIndex1 = B.termIndex . headTerms

-- | Indices of terms.
headIndex :: Relhead -> [B.TermPath] -> [[Int]]
headIndex = B.termsIndex . headTerms

headAlign :: Relhead -> Relhead -> B.Map [c]
headAlign to from = B.snipOrder (headNames to) (headNames from)

bodyAlign :: Relhead -> Relhead -> B.Map [[c]]
bodyAlign h1 h2 = (headAlign h1 h2 `map`)

headNested :: Relhead -> [(String, Relhead)]
headNested (Relhead ts1) = map h $ filter B.isTermNest ts1 where
    h (B.TermNest n ts2) = (n, Relhead ts2)
    h (B.TermFlat n)     = (n, Relhead [])

