{-# OPTIONS_GHC -Wall #-}

-- | Relation type

module Koshucode.Baala.Data.Type.Rel.Rel
  ( -- * Data type
    Rel (..), Body,
    relSort, relBodyOrder,
  
    -- * Constant
    reldum, reldee, reldau,

    -- * Converter
    SelectRel (..),
    judgesFromRel,
  ) where

import qualified Koshucode.Baala.Overture              as O
import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Syntax                as S
import qualified Koshucode.Baala.Data.Type.Rel.Head    as D
import qualified Koshucode.Baala.Data.Type.Judge       as D
import qualified Koshucode.Baala.Data.Type.JudgeClass  as D


-- ----------------------  Data

-- | Relations on type `c`.
--   Relation is consist of heading and body.
--   Body is thoretically a set of tuples,
--   but implemented using list of list.
data Rel c = Rel
    { relHead :: D.Head       -- ^ Heading of relation
    , relBody :: Body c    -- ^ Body of relation
    } deriving (Show)

-- | Body of relation, i.e., a list of tuples.
--   Tuple is a list of contents.
type Body c = [[c]]

relPair :: Rel c -> (D.Head, Body c)
relPair (Rel he bo) = (he, bo)

-- This order is different from the order of relational lattice.
instance (Ord c) => Ord (Rel c) where
    compare r1 r2 = let r1' = relPair $ relSort r1
                        r2' = relPair $ relSort r2
                    in compare r1' r2'

instance (Ord c) => Eq (Rel c) where
    a == b = (compare a b == EQ)

instance Functor Rel where
    fmap f (Rel he bo) = let bo' = fmap f `fmap` bo
                         in Rel he bo'

instance D.GetTermNames (Rel c) where
    getTermNames = D.getTermNames . relHead

instance (B.MixShortEncode c) => B.MixShortEncode (Rel c) where
    mixShortEncode sh (Rel he bo) =
        let he'  = B.mixEncode he
            bo'  = B.mixJoin1 $ map d bo
            d xs = B.mixBracketS S.listOpen S.listClose $ mixBar xs
        in B.mixBracketS S.relOpen S.relClose (he' `B.mixSep` bo')
        where mixBar cs = B.mixJoinBar $ map (B.mixShortEncode sh) cs


-- ----------------------  Sort contents

-- | Sort head and body of relation.
relSort :: (Ord c) => O.Map (Rel c)
relSort = relSortBody . relSortHead

-- | Sort head of relation.
relSortHead :: O.Map (Rel c)
relSortHead (Rel he bo) = Rel he' bo' where
    he' = D.headMap B.sort he
    bo' = D.bodyAlign he' he bo

-- | Sort body of relation.
relSortBody :: (Ord c) => O.Map (Rel c)
relSortBody (Rel he bo) = Rel he (B.sort $ B.unique bo)

-- | Sort relation body according to order specification.
relBodyOrder :: (Ord c) => [S.SignedTermName] -> D.Head -> O.Map [[c]]
relBodyOrder ns he = ed where
    ed    = B.sortByName ords $ D.getTermNames he
    ords  = map B.orderingCap ns



-- ----------------------  Constant

-- | The nullary empty relation.
--   In other words, relational constant
--   that has no terms and no tuples.
--
--   >>> B.doc (reldum :: Rel Bool)
--   {= =}

reldum :: Rel c
reldum = Rel mempty []

-- | The nullary full relation.
--   In other words, relational constant
--   that has no terms and the empty tuple.
--
--   >>> B.doc (reldee :: Rel Bool)
--   {= [] =}

reldee :: Rel c
reldee = Rel mempty [[]]

-- | The empty relation with all terms.
--   This is fake.
reldau :: Rel c
reldau = Rel mempty []


-- ----------------------  Converter

-- | Type for having relations.
class SelectRel r where
    -- | Convert judges to relation.
    selectRel :: r c -> D.JudgeClass -> [S.TermName] -> Rel c

-- | Convert relation to list of judges.
judgesFromRel :: D.JudgeOf c -> D.JudgeClass -> Rel c -> [D.Judge c]
judgesFromRel judgeOf pat (Rel he bo) = map judge bo where
    judge = judgeOf pat . zip names
    names = D.getTermNames he
