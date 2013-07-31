{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

{-| Unary relational operators. -}

module Koshucode.Baala.Minimal.Relmap.Unary
( -- * reldee & reldum
  relopReldee, relopReldum,
  -- * source
  relopSource,
  -- * id
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

import Koshucode.Baala.Base
import Koshucode.Baala.Builtin as Kit
import qualified Data.List  as List
import qualified Data.Maybe as Maybe
import qualified Data.Tuple as Tuple



-- ----------------------  reldee & reldum

relopReldee, relopReldum :: Relop c
relopReldee use = Right $ relmapConst use "reldee" reldee
relopReldum use = Right $ relmapConst use "reldum" reldum


-- ----------------------  source

relopSource :: Relop c
relopSource use =
  do sign <- Kit.getWord  use "-sign"
     ns   <- Kit.getTerms use "-term"
     Right $ relmapSource use sign ns


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



-- ----------------------  pick

relopPick :: (Ord v) => Relop v
relopPick use =
  do ns <- getTerms use "-term"
     Right $ relmapPick use ns

relmapPick :: (Ord v) => OpUse v -> [String] -> Relmap v
relmapPick use ns = Kit.relmapCalc use "pick" sub where
    sub _ = relPick ns

relPick
    :: (Ord v)
    => [String]        -- ^ Term names
    -> AbMap (Rel v)   -- ^ Mapping relation to relation
relPick ns r = project indexPick ns r

project :: (Ord v) => ([Int] -> Listmap v) -> [String] -> AbMap (Rel v)
project f ns (Rel h1 b1)
    | null non   = Right $ Rel h2 b2
    | otherwise  = Left  $ AbortNoTerms non
    where
      non = headNonExistTerms h1 ns
      pos = List.sort $ Kit.posOf h1 (map singleton ns)
      pj  = f $ Kit.posPoss pos
      h2  = Kit.headChange pj h1
      b2  = unique $ map pj b1



-- ----------------------  cut

relopCut :: (Ord v) => Relop v
relopCut use =
  do ns <- getTerms use "-term"
     Right $ relmapCut use ns

relmapCut :: (Ord v) => OpUse v -> [String] -> Relmap v
relmapCut use ns = Kit.relmapCalc use "cut" sub where
    sub _ = relCut ns

relCut
    :: (Ord v)
    => [String]        -- ^ Term names
    -> AbMap (Rel v)   -- ^ Mapping relation to relation
relCut ns r = project indexCut ns r



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
relRename np (Rel h1 b1)
    | nsCheck /= []  =  Left  $ AbortReqNewTerms nsCheck
    | psCheck /= []  =  Left  $ AbortNoTerms psCheck
    | otherwise      =  Right $ Rel h2 b1
    where
      (ns, ps) = unzip np
      nsCheck  = headExistTerms    h1 ns
      psCheck  = headNonExistTerms h1 ps
      h2 = Kit.headChange (map re) h1
      pn = map Tuple.swap np
      re p = Maybe.fromMaybe p $ lookup p pn

