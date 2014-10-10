{-# OPTIONS_GHC -Wall #-}

-- | Heading of relations

module Koshucode.Baala.Base.Data.Head
( -- * Type
  Head (..),
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
  headConsNest,
  headCons,
  headAppend,
  headNests,
  -- $AddTermExample

  -- * Utility
  headMap,
  headRename,
  headIndex1,
  headAlign, bodyAlign,
  headNested,
  headUp,
  -- $UtilityExample
) where

import qualified Koshucode.Baala.Base.Prelude      as B
import qualified Koshucode.Baala.Base.Text         as B
import qualified Koshucode.Baala.Base.Token        as B
import qualified Koshucode.Baala.Base.Data.Type    as B


-- ---------------------- Type

-- | Heading of relation as a list of terms
data Head =
    Head { headType :: B.Type
         } deriving (Show, Eq, Ord)

instance B.Monoid Head where
    mempty = headEmpty
    mappend he1 he2 = headEmpty { headType = headType he1 `a` headType he2 }
        where a (B.TypeRel xs1) (B.TypeRel xs2) = B.TypeRel $ B.unionUp xs1 xs2
              a _ _ = B.TypeAny

instance B.Write Head where
    write _ = B.typeTermDoc . headType

headExplainLines :: Head -> [String]
headExplainLines = lines . headExplain

headExplain :: Head -> String
headExplain = show . headExplainDoc

headExplainDoc :: Head -> B.Doc
headExplainDoc = B.typeDoc . headType



-- ----------------------  Constructor

-- $ConstructorExample
--
--  /Examples/
--
--  Make heading of @\/a@ @\/b@.
--
--    >>> headFrom ["a", "b"]
--    Head [TermFlat "a", TermFlat "b"]
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

-- | Make head from term names.
headFrom :: [B.TermName] -> Head
headFrom ns = Head { headType = B.typeFlatRel ns }

-- | Empty heading, i.e., no terms in heading.
headEmpty :: Head
headEmpty = headFrom []

headWords :: String -> Head
headWords = headFrom . words

-- | List of term names.
headNames :: Head -> [B.TermName]
headNames = B.typeRelTermNames . headType

-- | Degree of relation, i.e., number of terms.
headDegree :: Head -> Int
headDegree = B.typeRelDegree . headType



-- ----------------------  Predicate

headEquiv :: Head -> Head -> Bool
headEquiv he1 he2 = ts he1 == ts he2 where
    ts = B.sort . B.typeRelTermNames . headType

isSubhead :: Head -> Head -> Bool
isSubhead he1 he2 = null $ headNames he1 `B.snipLeft` headNames he2 

isSuperhead :: Head -> Head -> Bool
isSuperhead he1 he2 = isSubhead he2 he1



-- ----------------------  Add terms

-- $AddTermExample
--
--  Add term @\/c@ to heading.
--
--    >>> let h = headFrom ["a", "b"] in headCons "c" h
--    Head [TermFlat "c", TermFlat "a", TermFlat "b"]
--

headConsNest :: B.TermName -> Head -> B.Map Head
headConsNest n1 Head { headType = t1 } Head { headType = t } =
    Head { headType = B.typeConsNest n1 t1 t }

-- | Add term name to head.
headCons :: B.TermName -> B.Map Head
headCons n1 h@Head { headType = t } =
    h { headType = B.typeConsRel n1 t }

-- | Add term names to head.
headAppend :: [B.TermName] -> B.Map Head
headAppend ns1 h@Head { headType = t } =
    h { headType = B.typeAppendRel ns1 t }

headNests :: [B.TermName] -> B.Map Head
headNests ns1 Head { headType = t } =
    Head { headType = B.TypeRel $ map nest ns1 }
    where nest n = (n, t)



-- ----------------------  Utility

-- $UtilityExample
--
--  /Examples/
--
--  Reverse term names.
--
--    >>> let h = headFrom ["a", "b"] in headChange reverse h
--    Head [TermFlat "b", TermFlat "a"]
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
headMap :: B.Map [B.NamedType] -> B.Map Head
headMap = headMapBy B.typeRelMapTerms

headRename :: B.Map B.TermName -> B.Map Head
headRename = headMapBy B.typeRelMapName

headMapBy :: (a -> B.Map B.Type) -> a -> B.Map Head
headMapBy g f he = he { headType = g f $ headType he }

-- | Index of a term.
headIndex1 :: Head -> B.TermPath -> [Int]
headIndex1 = B.typeRelIndex . headType

headAlign :: Head -> Head -> B.Map [c]
headAlign to from = B.snipOrder (headNames to) (headNames from)

bodyAlign :: Head -> Head -> B.Map [[c]]
bodyAlign he1 he2 = (headAlign he1 he2 `map`)

headNested :: Head -> [(String, Head)]
headNested Head { headType = ty } = ts2 where
    ts2 = map h $ filter (B.isTypeRel . snd) $ B.typeTerms ty
    h (n, t) = (n, headEmpty { headType = t })

headUp :: B.Map Head
headUp Head { headType = ty } =
    headEmpty { headType = up ty } where
        up (B.TypeRel [(_, t)]) = t
        up t                    = t
