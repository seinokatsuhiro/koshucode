{-# OPTIONS_GHC -Wall #-}

-- | Type for relations

module Koshucode.Baala.Base.Data.Rel
( Rel (Rel)
, Relbody, RelmapFun
, reldum, reldee
, module Koshucode.Baala.Base.Data.Rel.Relhead
) where

import Koshucode.Baala.Base.Data.Rel.Relhead
import Koshucode.Baala.Base.Prelude

-- ----------------------  Data

-- | Relations on type v.
--   Heading of relation and
--   body of relation as a list of tuples.
data Rel v = Rel Relhead (Relbody v)
             deriving (Show, Eq, Ord)

-- | List of positional args
type Relbody v = [[v]]

-- | Relation-to-relation mapping
type RelmapFun v = Rel v -> Rel v

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
docRelFull (Rel h b) = docTag "rel" (comm <+> h2 <+> b2)
    where comm  = docComment $ text "card:" <+> int (length b)
          h2    = docParen (hsep $ map text (headNames h))
          b2    = hsep $ map arg b
          arg a = docParen (hsep $ map doc a)

-- docRelShort :: Rel a -> Doc
-- docRelShort (Rel h b) = docTag "rel" (h2 <+> b2) where
--     h2 = docParen (hsep $ map text (headNames h))
--     b2 = text "..." <+> int (length b) <+> text "args ..."

