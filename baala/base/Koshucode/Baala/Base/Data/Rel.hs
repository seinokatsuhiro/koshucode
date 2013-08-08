{-# OPTIONS_GHC -Wall #-}

{-| Type for relations -}

module Koshucode.Baala.Base.Data.Rel
(
  -- * Datatype
  Rel (Rel),
  Relbody,

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

instance (Pretty c) => Pretty (Rel c) where
    doc = docRelFull

docRelFull :: (Pretty a) => Rel a -> Doc
docRelFull (Rel h b) = text "{|" <+> h2 <+> b2 <+> text "|}"
    where h2    = hsep $ map text (headNames h)
          b2    = hsep $ map arg b
          arg a = text "|" <+> (hsep $ map doc a)



-- ----------------------  Constant

{-| Relational constant that has no terms and no tuples. -}
reldum :: Rel c
reldum = Rel mempty []

{-| Relational constant that has no terms and the empty tuple. -}
reldee :: Rel c
reldee = Rel mempty [[]]

