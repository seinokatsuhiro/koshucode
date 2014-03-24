{-# OPTIONS_GHC -Wall #-}

-- | Relation type

module Koshucode.Baala.Base.Data.Rel
( -- * Datatype
  Rel (..),
  Relbody,
  relPosHere,
  relSort,
  headAlign,
  bodyAlign,

  -- * Constant
  reldum,
  reldee,
) where

import qualified Data.List                         as List
import qualified Koshucode.Baala.Base.Prelude      as B
import qualified Koshucode.Baala.Base.Text         as B
import qualified Koshucode.Baala.Base.Token        as B
import qualified Koshucode.Baala.Base.Data.Relhead as B
import qualified Koshucode.Baala.Base.Data.TermPos as B



-- ----------------------  Data

-- | Relations on type `c`.
--   Relation is consist of heading and body.
--   Body is thoretically a set of tuples,
--   but implemented using list of list.
data Rel c = Rel
    { relHead :: B.Relhead    -- ^ Heading of relation
    , relBody :: Relbody c    -- ^ Body of relbody
    } deriving (Show, Ord)

instance (Ord c) => Eq (Rel c) where
    (==) = relEq

-- | Body of relation, i.e., a list of tuples.
--   Tuple is list of contents.
type Relbody c = [[c]]

-- | >>> doc $ rel ["/a", "/b"] [[10, 20], [30, 40 :: Int]]
--   {| /a /b | 10 : 20 | 30 : 40 |}
instance (B.Pretty c) => B.Pretty (Rel c) where
    doc (Rel h1 b1) = B.docWraps "{|" "|}" $ h2 B.<+> b2
        where h2    = B.doc h1
              b2    = B.doch $ map d b1
              d xs  = B.doc "|" B.<+> B.docColon xs

{-| Relational version of `B.posHere`. -}
relPosHere :: Rel c -> [B.TermName] -> ([B.TermPos], [Bool])
relPosHere r = B.posHere $ relHead r



-- ----------------------  Equality

relEq :: (Ord c) => Rel c -> Rel c -> Bool
relEq r1 r2 = hb (relSort r1) == hb (relSort r2) where
    hb (Rel h b) = (h, b)

relSort :: (Ord c) => B.Map (Rel c)
relSort = relSortBody . relSortHead

relSortBody :: (Ord c) => B.Map (Rel c)
relSortBody (Rel h1 b1) = Rel h1 $ B.unique $ List.sort b1

relSortHead :: B.Map (Rel c)
relSortHead (Rel h1 b1) = Rel h2 b2 where
    h2 = B.headFrom $ List.sort $ B.headNames h1
    b2 = (h2 `headAlign` h1) `map` b1

headAlign :: B.Relhead -> B.Relhead -> B.Map [c]
headAlign to from
    | to == from = id
    | otherwise  = B.snipFrom index
    where
      index = B.posIndex `map` B.posTo from to

bodyAlign :: B.Relhead -> B.Relhead -> B.Map [[c]]
bodyAlign h1 h2 = (headAlign h1 h2 `map`)



-- ----------------------  Constant

-- | The nullary empty relation.
--   In other words, relational constant
--   that has no terms and no tuples.
--
--   >>> B.doc (reldum :: Rel Bool)
--   {| |}
reldum :: Rel c
reldum = Rel B.mempty []

-- | The nullary full relation.
--   In other words, relational constant
--   that has no terms and the empty tuple.
--
--   >>> B.doc (reldee :: Rel Bool)
--   {| | |}
reldee :: Rel c
reldee = Rel B.mempty [[]]

