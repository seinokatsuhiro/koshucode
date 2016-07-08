{-# OPTIONS_GHC -Wall #-}

-- | Heading of relations

module Koshucode.Baala.Data.Type.Rel.Head
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
    -- $AddTerm
  
    -- * Mapping
    headMap,
    headMapName,
    headUp,
    bodyAlign,
    -- $Mapping

    -- * Picker
    HeadLR (..),
    HeadLRMap,
    HeadLRMap2,
    headLR, headLROrd,
  ) where

import qualified Koshucode.Baala.Base              as B
import qualified Koshucode.Baala.Syntax            as S
import qualified Koshucode.Baala.Data.Type.Type    as D
import qualified Koshucode.Baala.Data.Type.Judge   as D



-- ---------------------- Type

-- | Heading of relations.
data Head =
    Head { headType :: D.Type
         } deriving (Show, Eq, Ord)

instance Monoid Head where
    mempty = headFrom []
    mappend he1 he2 = headOf $ headType he1 `a` headType he2
        where a (D.TypeRel ts1) (D.TypeRel ts2) = D.TypeRel $ B.unionUp ts1 ts2
              a _ _ = D.TypeAny

instance D.GetTermNames Head where
    getTermNames = D.typeRelTermNames . headType

instance B.MixEncode Head where
    mixEncode = D.typeTermMix . headType

-- | Pretty print head of relation.
headExplain :: Head -> B.Doc
headExplain = D.typeExplain . headType


-- ----------------------  Constructor

-- | Make head of given type..
headOf :: D.Type -> Head
headOf ty = Head { headType = ty }

-- | Make head from term names.
--
--   >>> headFrom ["a", "b"]
--   Head { headType = TypeRel [("a", TypeAny), ("b", TypeAny)] }
--
--   >>> B.mixEncode $ headFrom ["a", "b"]
--   MixText "/a /b"

headFrom :: [S.TermName] -> Head
headFrom = headOf . D.typeFlatRel


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
    ts = B.sort . D.typeRelTermNames . headType

-- | Test heading is subset of another heading.
--
--   >>> headFrom ["a", "b"] `isSubhead` headFrom ["a", "b", "c"]
--   True
--   >>> headFrom ["a", "e"] `isSubhead` headFrom ["a", "b", "c"]
--   False

isSubhead :: Head -> Head -> Bool
isSubhead he1 he2 = null $ D.getTermNames he1 `B.snipLeft` D.getTermNames he2 

-- | Test heading is superset of another heading.
isSuperhead :: Head -> Head -> Bool
isSuperhead he1 he2 = isSubhead he2 he1

-- | Degree of relation, i.e., number of terms.
--
--   >>> headDegree $ headFrom ["a", "b"]
--   2

headDegree :: Head -> Int
headDegree = D.typeRelDegree . headType

-- | Index of a term.
headIndex1 :: Head -> S.TermPath -> [Int]
headIndex1 = D.typeRelIndex . headType

-- | Select nested terms.
headNested :: Head -> [(S.TermName, Head)]
headNested he = ts2 where
    ts2 = map h $ filter (D.isTypeRel . snd) $ D.typeTerms $ headType he
    h (n, t) = (n, headOf t)

headTypes :: Head -> [D.Type]
headTypes (Head (D.TypeRel ts)) = map snd ts
headTypes _ = B.bug "headTypes"


-- ----------------------  Add terms

-- $AddTerm
--
--  /Example/
--
--  Add term @\/c@ to heading.
--
--    >>> let h = headFrom ["a", "b"] in headCons "c" h
--    Head { headType = TypeRel [("c", TypeAny), ("a", TypeAny), ("b", TypeAny)] }
--

-- | Add term name to head.
headCons :: S.TermName -> B.Map Head
headCons n1 = headOf . D.typeConsRel n1 . headType

-- | Add term names to head.
headAppend :: [S.TermName] -> B.Map Head
headAppend ns1 = headOf . D.typeAppendRel ns1 . headType

headConsNest :: S.TermName -> Head -> B.Map Head
headConsNest n1 Head { headType = t1 } Head { headType = t } =
    headOf $ D.typeConsNest n1 t1 t

headNests :: [S.TermName] -> B.Map Head
headNests ns1 Head { headType = t } =
    headOf $ D.TypeRel $ map nest ns1
        where nest n = (n, t)


-- ----------------------  Mapping

-- $Mapping
--
--  /Example/
--
--  Reverse term names.
--
--    >>> let h = headFrom ["a", "b"] in headMap reverse h
--    Head { headType = TypeRel [("b", TypeAny), ("a", TypeAny)] }
--

-- | Reconstruct head.
headMap :: B.Map [D.NamedType] -> B.Map Head
headMap = headMapBy D.typeRelMapTerms

headMapName :: B.Map S.TermName -> B.Map Head
headMapName = headMapBy D.typeRelMapName

headMapBy :: (a -> B.Map D.Type) -> a -> B.Map Head
headMapBy g f he = headOf $ g f $ headType he

-- | Up nested relation.
headUp :: B.Map Head
headUp = headOf . up . headType where
    up (D.TypeRel [(_, ty)]) = ty
    up ty                    = ty

headAlign :: Head -> Head -> B.Map [c]
headAlign to from = B.snipOrder (D.getTermNames to) (D.getTermNames from)

bodyAlign :: Head -> Head -> B.Map [[c]]
bodyAlign to from = (headAlign to from `map`)


-- ----------------------  Picker

type HeadLRMap c = HeadLR c -> [c] -> [c]
type HeadLRMap2 a b = (HeadLRMap a, HeadLRMap b)

data HeadLR c = HeadLR
    { headLShareIndex  :: [Int]         -- ^ Indicies of right-shared part
    , headRShareIndex  :: [Int]         -- ^ Indicies of left-shared part
    , headDisjoint     :: Bool          -- ^ Whether shared part is empty

    , headLSideNames   :: [S.TermName]  -- ^ Left-side term names
    , headLShareNames  :: [S.TermName]  -- ^ Left-shared term names
    , headRShareNames  :: [S.TermName]  -- ^ Right-shared term names
    , headRSideNames   :: [S.TermName]  -- ^ Right-side term names

    , headLSide        :: [c] -> [c]    -- ^ Pick left-side part from left contents
    , headLShare       :: [c] -> [c]    -- ^ Pick left-shared part from left contents
    , headRShare       :: [c] -> [c]    -- ^ Pick right-shared part from right contents
    , headRSide        :: [c] -> [c]    -- ^ Pick right-side part from right contents

    , headRForward     :: [c] -> [c]
    , headRBackward    :: [c] -> [c]

    , headRSplit       :: [c] -> ([c], [c])  -- ^ Pick right-shared and right-side part
    , headRAssoc       :: [c] -> ([c], [c])  -- ^ Pick right-shared part and right contents
    }

headLR :: [S.TermName] -> [S.TermName] -> HeadLR a
headLR left right = headLRBody li ri left right where
    (li, ri) = sharedIndex left right

-- sharedIndex "dxcy" "abcd"
-- >>> ([0,2], [3,2])
--       d c    d c
sharedIndex :: (Ord a) => [a] -> [a] -> ([Int], [Int])
sharedIndex xs1 xs2 = (ind1, ind2) where
    ind1  = B.snipIndex sh xs1
    ind2  = B.snipIndex sh xs2
    sh    = B.intersectionFilter xs2 xs1

headLROrd :: [S.TermName] -> [S.TermName] -> HeadLR a
headLROrd left right = headLRBody li ri left right where
    (li, ri)  = (ind2 left, ind2 right)
    ind       = B.snipIndex left right
    ind2      = B.snipIndex $ B.snipFrom ind right

headLRBody :: [Int] -> [Int] -> [S.TermName] -> [S.TermName] -> HeadLR a
headLRBody li ri left right = lr where
    lside      = B.snipOff  li
    lshare     = B.snipFrom li
    rshare     = B.snipFrom ri
    rside      = B.snipOff  ri
    rfor       = B.snipForward  ri
    rback      = B.snipBackward ri
    rsplit xs  = (rshare xs, rside xs)
    rassoc xs  = (rshare xs, xs)

    lr = HeadLR { headLShareIndex  = li
                , headRShareIndex  = ri
                , headDisjoint     = null li
                , headLSideNames   = lside  left
                , headLShareNames  = lshare left
                , headRShareNames  = rshare right
                , headRSideNames   = rside  right
                , headLSide        = lside
                , headLShare       = lshare
                , headRShare       = rshare
                , headRSide        = rside
                , headRForward     = rfor
                , headRBackward    = rback
                , headRSplit       = rsplit
                , headRAssoc       = rassoc
                }
