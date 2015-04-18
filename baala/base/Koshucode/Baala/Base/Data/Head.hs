{-# OPTIONS_GHC -Wall #-}

-- | Heading of relations

module Koshucode.Baala.Base.Data.Head
  ( -- * Type
    Head (..),
    headExplain,
  
    -- * Constructor
    headOf, headFrom, headEmpty,
    -- $Constructor
  
    -- * Selector
    headEquiv,
    isSubhead, isSuperhead,
    headNames,
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
    headAlign,
    bodyAlign,
    -- $Mapping

    -- * Picker
    HeadLR (..),
    headLR,
  ) where

import qualified Koshucode.Baala.Base.Abort        as B
import qualified Koshucode.Baala.Base.Prelude      as B
import qualified Koshucode.Baala.Base.Text         as B
import qualified Koshucode.Baala.Base.Token        as B
import qualified Koshucode.Baala.Base.Data.Type    as B



-- ---------------------- Type

-- | Heading of relations.
data Head =
    Head { headType :: B.Type
         } deriving (Show, Eq, Ord)

instance B.Monoid Head where
    mempty = headEmpty
    mappend he1 he2 = headOf $ headType he1 `a` headType he2
        where a (B.TypeRel ts1) (B.TypeRel ts2) = B.TypeRel $ B.unionUp ts1 ts2
              a _ _ = B.TypeAny

instance B.Write Head where
    write _ = B.typeTermDoc . headType

headExplain :: Head -> B.Doc
headExplain = B.typeExplain . headType



-- ----------------------  Constructor

-- $Constructor
--
--  /Examples/
--
--  Make heading of @\/a@ @\/b@.
--
--    >>> headFrom ["a", "b"]
--    Head { headType = TypeRel [("a", TypeAny), ("b", TypeAny)] }
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

-- | Make head of given type..
headOf :: B.Type -> Head
headOf ty = Head { headType = ty }

-- | Make head from term names.
headFrom :: [B.TermName] -> Head
headFrom = headOf . B.typeFlatRel

-- | Empty heading, i.e., no terms in heading.
headEmpty :: Head
headEmpty = headFrom []



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
--
--  Heading of @\/a@ @\/b@ is a subhead of @\/a@ @\/b@ @\/c@, and @\/a@ @\/e@ is not.
--
--    >>> headFrom ["a", "b"] `isSubhead` headFrom ["a", "b", "c"]
--    True
--    >>> headFrom ["a", "e"] `isSubhead` headFrom ["a", "b", "c"]
--    False
--

headEquiv :: Head -> Head -> Bool
headEquiv he1 he2 = ts he1 == ts he2 where
    ts = B.sort . B.typeRelTermNames . headType

isSubhead :: Head -> Head -> Bool
isSubhead he1 he2 = null $ headNames he1 `B.snipLeft` headNames he2 

isSuperhead :: Head -> Head -> Bool
isSuperhead he1 he2 = isSubhead he2 he1

-- | List of term names.
headNames :: Head -> [B.TermName]
headNames = B.typeRelTermNames . headType

-- | Degree of relation, i.e., number of terms.
headDegree :: Head -> Int
headDegree = B.typeRelDegree . headType

-- | Index of a term.
headIndex1 :: Head -> B.TermPath -> [Int]
headIndex1 = B.typeRelIndex . headType

-- | Select nested terms.
headNested :: Head -> [(B.TermName, Head)]
headNested he = ts2 where
    ts2 = map h $ filter (B.isTypeRel . snd) $ B.typeTerms $ headType he
    h (n, t) = (n, headOf t)

headTypes :: Head -> [B.Type]
headTypes (Head (B.TypeRel ts)) = map snd ts
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
headCons :: B.TermName -> B.Map Head
headCons n1 = headOf . B.typeConsRel n1 . headType

-- | Add term names to head.
headAppend :: [B.TermName] -> B.Map Head
headAppend ns1 = headOf . B.typeAppendRel ns1 . headType

headConsNest :: B.TermName -> Head -> B.Map Head
headConsNest n1 Head { headType = t1 } Head { headType = t } =
    headOf $ B.typeConsNest n1 t1 t

headNests :: [B.TermName] -> B.Map Head
headNests ns1 Head { headType = t } =
    headOf $ B.TypeRel $ map nest ns1
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
headMap :: B.Map [B.NamedType] -> B.Map Head
headMap = headMapBy B.typeRelMapTerms

headMapName :: B.Map B.TermName -> B.Map Head
headMapName = headMapBy B.typeRelMapName

headMapBy :: (a -> B.Map B.Type) -> a -> B.Map Head
headMapBy g f he = headOf $ g f $ headType he

headUp :: B.Map Head
headUp = headOf . up . headType where
    up (B.TypeRel [(_, ty)]) = ty
    up ty                    = ty

headAlign :: Head -> Head -> B.Map [c]
headAlign to from = B.snipOrder (headNames to) (headNames from)

bodyAlign :: Head -> Head -> B.Map [[c]]
bodyAlign to from = (headAlign to from `map`)


-- ----------------------  Picker

data HeadLR c = HeadLR
    { headLShareIndex  :: [Int]        -- ^ Indicies of right-shared part
    , headRShareIndex  :: [Int]        -- ^ Indicies of left-shared part

    , headLSide        :: [c] -> [c]   -- ^ Pick left-side part from left contents
    , headLShare       :: [c] -> [c]   -- ^ Pick left-shared part from left contents
    , headRShare       :: [c] -> [c]   -- ^ Pick right-shared part from right contents
    , headRSide        :: [c] -> [c]   -- ^ Pick right-side part from right contents

    , headRSplit       :: [c] -> ([c], [c])  -- ^ Pick left-shared and right-shared part
    }

headLR :: [B.TermName] -> [B.TermName] -> HeadLR a
headLR left right = lr where
    (li, ri)   = left `B.snipPair` right

    lside      = B.snipOff  li
    lshare     = B.snipFrom li
    rshare     = B.snipFrom ri
    rside      = B.snipOff  ri
    rsplit xs  = (rshare xs, rside xs)

    lr = HeadLR { headLShareIndex  = li
                , headRShareIndex  = ri
                , headLSide        = lside
                , headLShare       = lshare
                , headRShare       = rshare
                , headRSide        = rside
                , headRSplit       = rsplit
                }
