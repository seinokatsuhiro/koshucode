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

consConcat :: OpCons v
consConcat = Right . mconcat . opSubmap

-- | Minimal implementations of relmaps
minimalOperators :: (Ord v) => [OpImplement v]
minimalOperators = builtinOperators ++ operators
    -- Relmap operators in alphabetical order
    [ o "cut"     LikePick    consCut
    , o "empty"   LikeEmpty   consEmpty
    , o "join"    LikeMeet    consJoin
    , o "meet"    LikeMeet    consMeet
    , o "minus"   LikeMeet    consMinus
    , o "pick"    LikePick    consPick
    , o "reldee"  LikeEmpty   consReldee
    , o "reldum"  LikeEmpty   consReldum
    , o "rename"  LikeRename  consRename
    , o "some"    LikeMeet    consSome
    , o "source"  LikeSource  consSource
    ] where o = (,,)



-- ----------------------  Constructors

consEmpty :: OpCons v
consEmpty use = Right $ relmapEmpty use

consRename :: OpCons v
consRename use = do
  np <- getTermPairs use "-term"
  Right $ relmapRename use np

consSource :: OpCons v
consSource use = do
  sign <- getWord  use "-sign"
  ns   <- getTerms use "-term"
  Right $ relmapSource use sign ns

-- Constant

consReldee, consReldum :: OpCons v
consReldee = consRelcon "reldee" reldee
consReldum = consRelcon "reldee" reldum

consRelcon :: String -> Rel v -> OpCons v
consRelcon op r use = Right $ relmapConst use op r

-- Project

consCut :: (Ord v) => OpCons v
consCut use = do
  ns <- getTerms use "-term"
  Right $ relmapCut use ns

consPick :: (Ord v) => OpCons v
consPick use = do
  ns <- getTerms use "-term"
  Right $ relmapPick use ns

-- Binary operation

consJoin :: (Ord v) => OpCons v
consJoin use = do
  m <- getRelmap use
  Right $ relmapJoin use m

consMeet :: (Ord v) => OpCons v
consMeet use = do
  m <- getRelmap use
  Right $ relmapMeet use m

consSome :: (Ord v) => OpCons v
consSome use = do
  m <- getRelmap use
  Right $ relmapSome use m

consMinus :: (Ord v) => OpCons v
consMinus use = do
  m <- getRelmap use
  Right $ relmapMinus use m



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

