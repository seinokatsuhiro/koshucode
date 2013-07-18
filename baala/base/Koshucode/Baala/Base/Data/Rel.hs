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

-- | Relations on type v.
--   Heading of relation and
--   body of relation as a list of tuples.
data Rel v = Rel Relhead (Relbody v)
             deriving (Show, Eq, Ord)

-- | List of positional args
type Relbody v = [[v]]



-- ----------------------  Constant

-- | Relational constant that has no terms and no tuples
reldum :: Rel v
reldum = Rel mempty []

-- | Relational constant that has no terms and the empty tuple
reldee :: Rel v
reldee = Rel mempty [[]]



-- ----------------------  Pretty printing

instance (Pretty v) => Pretty (Rel v) where
    doc = docRelFull

docRelFull :: (Pretty a) => Rel a -> Doc
docRelFull (Rel h b) = text "[|" <+> h2 <+> b2 <+> text "|]"
    where h2    = hsep $ map text (headNames h)
          b2    = hsep $ map arg b
          arg a = text "|" <+> (hsep $ map doc a)

-- docRelShort :: Rel a -> Doc
-- docRelShort (Rel h b) = docTag "rel" (h2 <+> b2) where
--     h2 = docParen (hsep $ map text (headNames h))
--     b2 = text "..." <+> int (length b) <+> text "args ..."

