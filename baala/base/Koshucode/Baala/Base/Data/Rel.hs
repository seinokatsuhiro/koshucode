{-# OPTIONS_GHC -Wall #-}

-- | Relation type

module Koshucode.Baala.Base.Data.Rel
  ( -- * Data type
    Rel (..), Body,
    relSort,
  
    -- * Constant
    reldum, reldee, reldau,
    -- $Constant

    -- * Converter
    SelectRel (..),
    judgesFromRel,
  ) where

import qualified Koshucode.Baala.Base.Prelude      as B
import qualified Koshucode.Baala.Base.Text         as B
import qualified Koshucode.Baala.Base.Token        as B
import qualified Koshucode.Baala.Base.Data.Head    as B
import qualified Koshucode.Baala.Base.Data.Judge   as B


-- ----------------------  Data

-- | Relations on type `c`.
--   Relation is consist of heading and body.
--   Body is thoretically a set of tuples,
--   but implemented using list of list.
data Rel c = Rel
    { relHead :: B.Head       -- ^ Heading of relation
    , relBody :: Body c    -- ^ Body of relation
    } deriving (Show)

-- | Body of relation, i.e., a list of tuples.
--   Tuple is a list of contents.
type Body c = [[c]]

relPair :: Rel c -> (B.Head, Body c)
relPair (Rel he bo) = (he, bo)

-- This order is different from the order of relational lattice.
instance (Ord c) => Ord (Rel c) where
    compare r1 r2 = let r1' = relPair $ relSort r1
                        r2' = relPair $ relSort r2
                    in compare r1' r2'

instance (Ord c) => Eq (Rel c) where
    a == b = (compare a b == EQ)

instance (B.Write c) => B.Write (Rel c) where
    write sh (Rel he bo) =
        let he'  = B.write  sh he
            bo'  = B.writeH sh $ map d bo
            d xs = B.docWraps "[" "]" $ B.writeBar sh xs
        in B.docWraps "{|" "|}" $ he' B.<+> bo'



-- ----------------------  Sort contents

relSort :: (Ord c) => B.Map (Rel c)
relSort = relSortBody . relSortHead

relSortHead :: B.Map (Rel c)
relSortHead (Rel he bo) = Rel he' bo' where
    he' = B.headMap B.sort he
    bo' = B.bodyAlign he' he bo

relSortBody :: (Ord c) => B.Map (Rel c)
relSortBody (Rel he bo) = Rel he (B.sort $ B.unique bo)



-- ----------------------  Constant

-- $Constant
--
--  /Examples/
--
--    >>> B.doc (reldum :: Rel Bool)
--    {| |}
--
--    >>> B.doc (reldee :: Rel Bool)
--    {| [] |}

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


-- ----------------------  Converter

class SelectRel r where
    -- | Convert judges to relation.
    selectRel :: r c -> B.JudgePat -> [B.TermName] -> Rel c

-- | Convert relation to list of judges.
judgesFromRel :: B.JudgeOf c -> B.JudgePat -> Rel c -> [B.Judge c]
judgesFromRel judgeOf pat (Rel he bo) = map judge bo where
    judge = judgeOf pat . zip names
    names = B.headNames he
