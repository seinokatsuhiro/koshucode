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

import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Base.Abort
import Koshucode.Baala.Base.Data.Relhead
import Koshucode.Baala.Base.Data.TermPos



-- ----------------------  Data

{-| Relations on type c.
    Heading of relation and
    body of relation as a list of tuples. -}
data Rel c = Rel
    { relHead :: Relhead
    , relBody :: Relbody c
    } deriving (Show, Eq, Ord)

{-| List of positional args. -}
type Relbody c = [[c]]

{-| >>> doc $ rel ["/a", "/b"] [[10, 20], [30, 40 :: Int]]
    {| /a /b | 10 : 20 | 30 : 40 |}  -}
instance (Pretty c) => Pretty (Rel c) where
    doc (Rel h1 b1) = docWraps "{|" "|}" $ h2 <+> b2
        where h2    = doc h1
              b2    = doch $ map d b1
              d xs  = doc "|" <+> docColon xs

rel :: [String] -> Relbody c -> Rel c
rel = Rel . headFrom

relPosHere :: Rel c -> [String] -> ([TermPos], [Bool])
relPosHere r = posHere $ relHead r

arrangeRel
    :: (Ord c)
    => Arrange String    -- ^ Arranger for termnames,
                         --   e.g., 'arrangePick', 'arrangeCut', etc
    -> Arrange c         -- ^ Arranger for term contents
    -> [String]          -- ^ Names of terms
    -> AbMap (Rel c)     -- ^ Relation-to-relation mapping
arrangeRel = arrangeRelUsing posSortByIndex

arrangeRelRaw
    :: (Ord c)
    => Arrange String    -- ^ Arranger for termnames,
                         --   e.g., 'arrangePick', 'arrangeCut', etc
    -> Arrange c         -- ^ Arranger for term contents
    -> [String]          -- ^ Names of terms
    -> AbMap (Rel c)     -- ^ Relation-to-relation mapping
arrangeRelRaw = arrangeRelUsing id

arrangeRelUsing
    :: (Ord c)
    => Map [TermPos]
    -> Arrange String
    -> Arrange c
    -> [String]
    -> AbMap (Rel c)
arrangeRelUsing sort ha ba ns (Rel h1 b1)
    | null non   = Right $ Rel h2 b2
    | otherwise  = Left  $ AbortNoTerms non
    where
      non =  headNonExistTerms h1 ns

      pos :: [TermPos]
      pos =  sort $ h1 `posFlat` ns

      ind :: [Int]
      ind =  map posIndex pos

      h2  =  headChange (ha ind) h1
      b2  =  unique $ map (ba ind) b1



-- ----------------------  Constant

{-| Relational constant that has no terms and no tuples. -}
reldum :: Rel c
reldum = Rel mempty []

{-| Relational constant that has no terms and the empty tuple. -}
reldee :: Rel c
reldee = Rel mempty [[]]

