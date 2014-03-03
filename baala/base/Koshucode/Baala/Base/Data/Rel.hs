{-# OPTIONS_GHC -Wall #-}

-- | Relation type

module Koshucode.Baala.Base.Data.Rel
( -- * Datatype
  Rel (..),
  Relbody,
  relPosHere,

  -- * Constant
  reldum,
  reldee,
) where

import qualified Koshucode.Baala.Base.Prelude      as B
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
    } deriving (Show, Eq, Ord)

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
relPosHere :: Rel c -> [B.Termname] -> ([B.TermPos], [Bool])
relPosHere r = B.posHere $ relHead r



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

