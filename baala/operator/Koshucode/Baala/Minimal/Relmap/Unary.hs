{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -Wall #-}

{-| Unary relational operators. -}

module Koshucode.Baala.Minimal.Relmap.Unary
( -- * empty
  relmapEmpty
, relEmpty

  -- * cut
, relmapCut
, relCut

  -- * pick
, relmapPick
, relPick

  -- * rename
, relmapRename
, relRename
) where

import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Minimal.OpKit as Kit
import qualified Data.List  as List
import qualified Data.Maybe as Maybe
import qualified Data.Tuple as Tuple



-- ----------------------  empty

relmapEmpty :: OpUse v -> Relmap v
relmapEmpty use = Kit.relmapCalc use "empty" sub where
    sub _ = relEmpty

{-| Throw away all tuples in a relation. -}
relEmpty :: Map (Rel v)  -- ^ Any relation to empty relation
relEmpty (Rel h1 _) = Rel h1 []

-- ----------------------  cut & pick

relmapCut :: (Ord v) => OpUse v -> [String] -> Relmap v
relmapCut use ns = Kit.relmapCalc use "cut" sub where
    sub _ = relCut ns

relCut
    :: (Ord v)
    => [String]      -- ^ Term names
    -> Map (Rel v)   -- ^ Mapping relation to relation
relCut = project indexCut

relmapPick :: (Ord v) => OpUse v -> [String] -> Relmap v
relmapPick use ns = Kit.relmapCalc use "pick" sub where
    sub _ = relPick ns

relPick
    :: (Ord v)
    => [String]      -- ^ Term names
    -> Map (Rel v)   -- ^ Mapping relation to relation
relPick = project indexPick

project :: (Ord v) => ([Int] -> Listmap v) -> [String] -> Map (Rel v)
project f ns (Rel h1 b1) = Rel h2 b2 where
    pos = List.sort $ Kit.headPoss h1 (map singleton ns)
    pj  = f $ Kit.posPoss pos
    h2  = Kit.rehead pj h1
    b2  = unique $ map pj b1

-- ----------------------  rename

relmapRename :: OpUse v -> [(String, String)] -> Relmap v
relmapRename use np = Kit.relmapCalc use "rename" sub where
    sub _ = relRename np

{-| Change terms names -}
relRename
    :: [(String, String)] -- ^ List of term name (/to/, /from/)
    -> Map (Rel v)        -- ^ Relation to relation
relRename np (Rel h1 b1) = Rel h2 b1 where
    h2 = Kit.rehead (map re) h1
    pn = map Tuple.swap np
    re p = Maybe.fromMaybe p $ lookup p pn

