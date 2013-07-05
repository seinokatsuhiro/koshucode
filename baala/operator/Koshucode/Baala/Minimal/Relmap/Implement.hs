{-# OPTIONS_GHC -Wall #-}

-- | Minimal implementations of relmaps

module Koshucode.Baala.Minimal.Relmap.Implement
( -- * Implementation
  minimalOperators

  -- * Operators
  -- $ListOfOperators
) where

import Koshucode.Baala.Minimal.OpKit as Kit
import Koshucode.Baala.Minimal.Relmap.Get
import Koshucode.Baala.Minimal.Relmap.Operand
import Koshucode.Baala.Minimal.Relmap.Pattern
import Koshucode.Baala.Minimal.Relmap.Restrict
import Koshucode.Baala.Minimal.Relmap.Tropashko
import Koshucode.Baala.Minimal.Relmap.Unary



-- ----------------------  Operators

builtinOperators :: (Ord v) => [OpImplement v]
builtinOperators = operators [ ("|", LikeEmpty, consConcat) ]

consConcat :: Relop v
consConcat = Right . mconcat . opSubmap

-- | Minimal implementations of relmaps
minimalOperators :: (Ord v) => [OpImplement v]
minimalOperators = builtinOperators ++ operators
    -- Relmap operators in alphabetical order
    [ o "cut"     LikePick    relopCut
    , o "empty"   LikeEmpty   relopEmpty
    , o "join"    LikeMeet    relopJoin
    , o "meet"    LikeMeet    relopMeet
    , o "minus"   LikeMeet    relopMinus
    , o "pick"    LikePick    relopPick
    , o "reldee"  LikeEmpty   relopReldee
    , o "reldum"  LikeEmpty   relopReldum
    , o "rename"  LikeRename  relopRename
    , o "some"    LikeMeet    relopSome
    , o "source"  LikeSource  relopSource
    ] where o = (,,)



-- ----------------------  Constructors

relopSource :: Relop v
relopSource use = do
  sign <- getWord  use "-sign"
  ns   <- getTerms use "-term"
  Right $ relmapSource use sign ns

-- Constant

relopReldee, relopReldum :: Relop v
relopReldee = consRelcon "reldee" reldee
relopReldum = consRelcon "reldee" reldum

consRelcon :: String -> Rel v -> Relop v
consRelcon op r use = Right $ relmapConst use op r

-- Project




-- ----------------------
-- $ListOfOperators
--
-- [@cut@] Project relation to unspecified terms
--
-- [@join@] Calculate join of two relations.
--
-- [@meet@] Calculate meet of two relations.
-- 
-- [@pick@] Project relation to specified terms
--
-- [@reldee@] Nullary fullset relation
--
-- [@reldum@] Nullary empty relation
--
-- [@rename@] Change term name
--
-- [@source@] Read relation from data source

