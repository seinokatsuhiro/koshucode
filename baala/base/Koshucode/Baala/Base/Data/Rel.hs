{-# OPTIONS_GHC -Wall #-}

-- | Relation type

module Koshucode.Baala.Base.Data.Rel
( -- * Datatype
  Rel (..),
  Relbody,
  MapRel,
  relSort,

  -- * Constant
  reldum,
  reldee,
  reldau,
  -- $ConstantExample
) where

import qualified Koshucode.Baala.Base.Prelude      as B
import qualified Koshucode.Baala.Base.Text         as B
import qualified Koshucode.Baala.Base.Data.Head    as B



-- ----------------------  Data

-- | Relations on type `c`.
--   Relation is consist of heading and body.
--   Body is thoretically a set of tuples,
--   but implemented using list of list.
data Rel c = Rel
    { relHead :: B.Head       -- ^ Heading of relation
    , relBody :: Relbody c    -- ^ Body of relation
    } deriving (Show, Ord)

instance (Ord c) => Eq (Rel c) where
    (==) = relEq

-- | Body of relation, i.e., a list of tuples.
--   Tuple is list of contents.
type Relbody c = [[c]]

type MapRel c = B.Map (Rel c)

instance (B.Write c) => B.Write (Rel c) where
    write sh (Rel h1 b1) = B.docWraps "{|" "|}" $ h2 B.<+> b2
        where h2    = B.write  sh h1
              b2    = B.writeH sh $ map d b1
              d xs  = B.write  sh "|" B.<+> B.writeColon sh xs



-- ----------------------  Equality

relEq :: (Ord c) => Rel c -> Rel c -> Bool
relEq r1 r2 = pair (relSort r1) == pair (relSort r2) where
    pair (Rel he bo) = (he, bo)

relSort :: (Ord c) => B.Map (Rel c)
relSort = relSortBody . relSortHead

relSortHead :: B.Map (Rel c)
relSortHead (Rel he1 bo1) = Rel he2 bo2 where
    he2 = B.headChange B.sort he1
    bo2 = B.headAlign he2 he1 `map` bo1

relSortBody :: (Ord c) => B.Map (Rel c)
relSortBody (Rel he1 bo1) = Rel he1 $ B.unique $ B.sort bo1



-- ----------------------  Constant

-- $ConstantExample
--
--  /Examples/
--
--    >>> B.doc (reldum :: Rel Bool)
--    {| |}
--
--    >>> B.doc (reldee :: Rel Bool)
--    {| | |}

-- | The nullary empty relation.
--   In other words, relational constant
--   that has no terms and no tuples.
reldum :: Rel c
reldum = Rel B.mempty []

-- | The nullary full relation.
--   In other words, relational constant
--   that has no terms and the empty tuple.
reldee :: Rel c
reldee = Rel B.mempty [[]]

-- | The empty relation with all terms.
--   This is fake.
reldau :: Rel c
reldau = Rel B.mempty []

