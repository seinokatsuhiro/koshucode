{-# OPTIONS_GHC -Wall #-}

-- | Heading of relations

module Koshucode.Baala.Type.Rel.Head
  ( -- * Type
    Head (..),
    headExplain,
  
    -- * Constructor
    headOf, headFrom,
    
    -- * Selector
    headEquiv,
    isSubhead, isSuperhead,
    headDegree,
    headNested,
    headIndex1,
    headTypes,
    -- $Selector
  
    -- * Add terms
    headCons,
    headAppend,
    headConsNest,
    headNests,
  
    -- * Mapping
    TypeTerm,
    headMap,
    headMapName,
    headUp,
    headForward,
    bodyForward,
  ) where

import qualified Koshucode.Baala.Overture              as O
import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Syntax                as S
import qualified Koshucode.Baala.Type.Type             as T
import qualified Koshucode.Baala.Type.Judge            as D
import qualified Koshucode.Baala.Type.Rel.TermPicker   as D


-- ---------------------- Type

-- | Heading of relations.
data Head =
    Head { headType :: T.Type
         } deriving (Show, Eq, Ord)

instance Monoid Head where
    mempty = headFrom ([] :: [String])
    mappend he1 he2 = headOf $ headType he1 `a` headType he2
        where a (T.TypeRel ts1) (T.TypeRel ts2) = T.TypeRel $ B.unionUp ts1 ts2
              a _ _ = T.TypeAny

instance D.GetTermNames Head where
    getTermNames = T.typeRelTermNames . headType

instance B.MixEncode Head where
    mixEncode = T.typeTermMix . headType

-- | Pretty print head of relation.
--
--   >>> headExplain $ headFrom ["a", "b"]
--   rel /a any
--       /b any
--
headExplain :: Head -> B.Doc
headExplain = T.typeExplain . headType


-- ----------------------  Constructor

-- | Make head of given type..
headOf :: T.Type -> Head
headOf ty = Head { headType = ty }

-- | Make head from term names.
--
--   >>> headFrom ["a", "b"]
--   Head { headType = TypeRel [("a", TypeAny), ("b", TypeAny)] }
--
--   >>> B.mixEncode $ headFrom ["a", "b"]
--   MixText "/a /b"
--
headFrom :: (S.ToTermName n) => [n] -> Head
headFrom = headOf . T.typeFlatRel . map S.toTermName


-- ----------------------  Selector

-- $Selector
--
--  /Examples/
--
--  Calculate indices of heading.
--
--    >>> let h = headFrom ["a", "b"] in headIndex1 h ["a"]
--    [0]
--    >>> let h = headFrom ["a", "b"] in headIndex h [["a"], ["b"], ["c"]]
--    [[0], [1], [-1]]

-- | Test two heads are equivalent.
headEquiv :: Head -> Head -> Bool
headEquiv he1 he2 = ts he1 == ts he2 where
    ts = B.sort . T.typeRelTermNames . headType

-- | Test heading is subset of another heading.
--
--   >>> headFrom ["a", "b"] `isSubhead` headFrom ["a", "b", "c"]
--   True
--   >>> headFrom ["a", "e"] `isSubhead` headFrom ["a", "b", "c"]
--   False
--
isSubhead :: Head -> Head -> Bool
isSubhead he1 he2 = D.getTermNames he1 `B.sublist` D.getTermNames he2

-- | Test heading is superset of another heading.
isSuperhead :: Head -> Head -> Bool
isSuperhead he1 he2 = isSubhead he2 he1

-- | Degree of relation, i.e., number of terms.
--
--   >>> headDegree $ headFrom ["a", "b"]
--   2
--
headDegree :: Head -> Int
headDegree = T.typeRelDegree . headType

-- | Index of a term.
headIndex1 :: Head -> S.TermPath -> [Int]
headIndex1 = T.typeRelIndex . headType

-- | Select nested terms.
headNested :: Head -> [(S.TermName, Head)]
headNested he = ts2 where
    ts2 = map h $ filter (T.isTypeRel . snd) $ T.typeTerms $ headType he
    h (n, t) = (n, headOf t)

-- | Get types of relation terms.
headTypes :: Head -> [T.Type]
headTypes (Head (T.TypeRel ts)) = map snd ts
headTypes _ = B.bug "headTypes"


-- ----------------------  Add terms

-- | Add term name to head.
--
--   >>> headCons "c" $ headFrom ["a", "b"]
--   Head { headType = TypeRel [("c",TypeAny), ("a",TypeAny), ("b",TypeAny)] }
--
headCons :: S.TermName -> O.Map Head
headCons n1 = headOf . T.typeConsRel n1 . headType

-- | Add term names to head.
headAppend :: [S.TermName] -> O.Map Head
headAppend ns1 = headOf . T.typeAppendRel ns1 . headType

-- | Add term name for nested relation.
headConsNest :: S.TermName -> Head -> O.Map Head
headConsNest n1 Head { headType = t1 } Head { headType = t } =
    headOf $ T.typeConsNest n1 t1 t

-- | Create nested relation terms.
--
--   >>> headNests ["x", "y"] $ headFrom ["a"]
--   Head { headType = TypeRel [("x", TypeRel [("a",TypeAny)]), ("y", TypeRel [("a",TypeAny)])] }
--
headNests :: [S.TermName] -> O.Map Head
headNests ns1 Head { headType = t } =
    headOf $ T.TypeRel $ map nest ns1
        where nest n = (n, t)


-- ----------------------  Mapping

-- | Term whose content is type.
type TypeTerm = S.Term T.Type

-- | Reconstruct head.
--
--   >>> headMap reverse $ headFrom ["a", "b"]
--   Head {headType = TypeRel [("b",TypeAny),("a",TypeAny)]}
--
headMap :: O.Map [TypeTerm] -> O.Map Head
headMap = headMapBy T.typeRelMapTerms

-- | Convert term names.
--
--   >>> headMapName ("x" ++) $ headFrom ["a", "b"]
--   Head { headType = TypeRel [("xa",TypeAny), ("xb",TypeAny)] }
--
headMapName :: O.Map S.TermName -> O.Map Head
headMapName = headMapBy T.typeRelMapName

headMapBy :: (a -> O.Map T.Type) -> a -> O.Map Head
headMapBy g f he = headOf $ g f $ headType he

-- | Move up nested relation.
headUp :: O.Map Head
headUp = headOf . up . headType where
    up (T.TypeRel [(_, ty)]) = ty
    up ty                    = ty

-- | Move terms forward.
--
--   >>> headForward ["c"] ["a", "b", "c"] ["x", "y", "z"]
--   ["z", "y", "x"]
--
--   >>> headForward (headFrom ["a", "b"]) (headFrom ["b", "a"]) ["x", "y"]
--   ["y", "x"]
--
headForward :: (D.GetTermNames t1, D.GetTermNames t2) => t1 -> t2 -> O.Map [c]
headForward to from = B.pkRForward $ D.termPicker to from

-- | Move terms forward.
bodyForward :: (D.GetTermNames t1, D.GetTermNames t2) => t1 -> t2 -> O.Map [[c]]
bodyForward to from = (headForward to from <$>)

