{-# OPTIONS_GHC -Wall #-}

{-| Type for relations -}

module Koshucode.Baala.Base.Data.Rel
( -- * Datatype
  Rel (..),
  Relbody,
  rel,
  relPosHere,
  arrangeRel,
  arrangeRelRaw,

  -- * Constant
  reldum,
  reldee,
) where

import qualified Koshucode.Baala.Base.Prelude as B
import qualified Koshucode.Baala.Base.Abort   as B
import qualified Koshucode.Baala.Base.Data.Relhead as B
import qualified Koshucode.Baala.Base.Data.TermPos as B



-- ----------------------  Data

{-| Relations on type c.
    Heading of relation and
    body of relation as a list of tuples. -}
data Rel c = Rel
    { relHead :: B.Relhead
    , relBody :: Relbody c
    } deriving (Show, Eq, Ord)

{-| List of positional args. -}
type Relbody c = [[c]]

{-| >>> doc $ rel ["/a", "/b"] [[10, 20], [30, 40 :: Int]]
    {| /a /b | 10 : 20 | 30 : 40 |}  -}
instance (B.Pretty c) => B.Pretty (Rel c) where
    doc (Rel h1 b1) = B.docWraps "{|" "|}" $ h2 B.<+> b2
        where h2    = B.doc h1
              b2    = B.doch $ map d b1
              d xs  = B.doc "|" B.<+> B.docColon xs

rel :: [String] -> Relbody c -> Rel c
rel = Rel . B.headFrom

relPosHere :: Rel c -> [String] -> ([B.TermPos], [Bool])
relPosHere r = B.posHere $ relHead r

arrangeRel
    :: (Ord c)
    => B.Arrange String    -- ^ Arranger for termnames,
                           --   e.g., 'arrangePick', 'arrangeCut', etc
    -> B.Arrange c         -- ^ Arranger for term contents
    -> [String]            -- ^ Names of terms
    -> B.AbMap (Rel c)     -- ^ Relation-to-relation mapping
arrangeRel = arrangeRelUsing B.posSortByIndex

arrangeRelRaw
    :: (Ord c)
    => B.Arrange String    -- ^ Arranger for termnames,
                           --   e.g., 'arrangePick', 'arrangeCut', etc
    -> B.Arrange c         -- ^ Arranger for term contents
    -> [String]            -- ^ Names of terms
    -> B.AbMap (Rel c)     -- ^ Relation-to-relation mapping
arrangeRelRaw = arrangeRelUsing id

arrangeRelUsing
    :: (Ord c)
    => B.Map [B.TermPos]
    -> B.Arrange String
    -> B.Arrange c
    -> [String]
    -> B.AbMap (Rel c)
arrangeRelUsing sort ha ba ns (Rel h1 b1)
    | null non   = Right $ Rel h2 b2
    | otherwise  = Left  $ B.AbortNoTerms non
    where
      non =  B.headNonExistTerms h1 ns

      pos :: [B.TermPos]
      pos =  sort $ h1 `B.posFlat` ns

      ind :: [Int]
      ind =  map B.posIndex pos

      h2  =  B.headChange (ha ind) h1
      b2  =  B.unique $ map (ba ind) b1



-- ----------------------  Constant

{-| Relational constant that has no terms and no tuples. -}
reldum :: Rel c
reldum = Rel B.mempty []

{-| Relational constant that has no terms and the empty tuple. -}
reldee :: Rel c
reldee = Rel B.mempty [[]]

