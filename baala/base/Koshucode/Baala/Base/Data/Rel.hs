{-# OPTIONS_GHC -Wall #-}

{-| Type for relations -}

module Koshucode.Baala.Base.Data.Rel
( -- * Datatype
  Rel (Rel),
  Relbody,
  rel,

  -- * Constant
  reldum,
  reldee,
) where

import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Base.Data.Relhead



-- ----------------------  Data

{-| Relations on type c.
    Heading of relation and
    body of relation as a list of tuples. -}
data Rel c = Rel Relhead (Relbody c)
             deriving (Show, Eq, Ord)

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



-- ----------------------  Constant

{-| Relational constant that has no terms and no tuples. -}
reldum :: Rel c
reldum = Rel mempty []

{-| Relational constant that has no terms and the empty tuple. -}
reldee :: Rel c
reldee = Rel mempty [[]]

