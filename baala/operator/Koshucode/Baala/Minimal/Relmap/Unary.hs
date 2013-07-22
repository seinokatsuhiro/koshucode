{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -Wall #-}

{-| Unary relational operators. -}

module Koshucode.Baala.Minimal.Relmap.Unary
( -- * id
  relopId, relmapId, relId,
  -- * empty
  relopEmpty, relmapEmpty, relEmpty,
  -- * cut
  relopCut, relmapCut, relCut,
  -- * pick
  relopPick, relmapPick, relPick,
  -- * rename
  relopRename, relmapRename, relRename
) where

import Koshucode.Baala.Base.Abort
import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Minimal.OpKit as Kit
import Koshucode.Baala.Minimal.Relmap.Get
import qualified Data.List  as List
import qualified Data.Maybe as Maybe
import qualified Data.Tuple as Tuple



-- ----------------------  id

relopId :: Relop v
relopId use = Right $ relmapId use

relmapId :: OpUse v -> Relmap v
relmapId use = Kit.relmapCalc use "id" sub where
    sub _ = relId

{-| Identity mapping, i.e., do nothing. -}
relId :: AbMap (Rel v)
relId = Right



-- ----------------------  empty

relopEmpty :: Relop v
relopEmpty use = Right $ relmapEmpty use

relmapEmpty :: OpUse v -> Relmap v
relmapEmpty use = Kit.relmapCalc use "empty" sub where
    sub _ = relEmpty

{-| Throw away all tuples in a relation. -}
relEmpty :: AbMap (Rel v)  -- ^ Any relation to empty relation
relEmpty (Rel h1 _) = Right $ Rel h1 []



-- ----------------------  cut & pick

relopCut :: (Ord v) => Relop v
relopCut use = do
  ns <- getTerms use "-term"
  Right $ relmapCut use ns

relmapCut :: (Ord v) => OpUse v -> [String] -> Relmap v
relmapCut use ns = Kit.relmapCalc use "cut" sub where
    sub _ = relCut ns

relCut
    :: (Ord v)
    => [String]        -- ^ Term names
    -> AbMap (Rel v)   -- ^ Mapping relation to relation
relCut ns r = Right $ project indexCut ns r

relopPick :: (Ord v) => Relop v
relopPick use = do
  ns <- getTerms use "-term"
  Right $ relmapPick use ns

relmapPick :: (Ord v) => OpUse v -> [String] -> Relmap v
relmapPick use ns = Kit.relmapCalc use "pick" sub where
    sub _ = relPick ns

relPick
    :: (Ord v)
    => [String]        -- ^ Term names
    -> AbMap (Rel v)   -- ^ Mapping relation to relation
relPick ns r = Right $ project indexPick ns r

project :: (Ord v) => ([Int] -> Listmap v) -> [String] -> Map (Rel v)
project f ns (Rel h1 b1) = Rel h2 b2 where
    pos = List.sort $ Kit.posOf h1 (map singleton ns)
    pj  = f $ Kit.posPoss pos
    h2  = Kit.headChange pj h1
    b2  = unique $ map pj b1



-- ----------------------  rename

relopRename :: Relop v
relopRename use = do
  np <- getTermPairs use "-term"
  Right $ relmapRename use np

relmapRename :: OpUse v -> [(String, String)] -> Relmap v
relmapRename use np = Kit.relmapCalc use "rename" sub where
    sub _ = relRename np

{-| Change terms names -}
relRename
    :: [(String, String)]   -- ^ List of term name (/to/, /from/)
    -> AbMap (Rel v)        -- ^ Relation to relation
relRename np (Rel h1 b1) = Right $ Rel h2 b1 where
    h2 = Kit.headChange (map re) h1
    pn = map Tuple.swap np
    re p = Maybe.fromMaybe p $ lookup p pn

