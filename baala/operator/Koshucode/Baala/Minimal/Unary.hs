{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

{-| Unary relational operators. -}

module Koshucode.Baala.Minimal.Unary
( -- * reldee & reldum
  ropConsReldee, ropConsReldum,
  -- * source
  ropConsSource,
  -- * id
  ropConsId, relmapId, relId,
  -- * empty
  ropConsEmpty, relmapEmpty, relEmpty,
  -- * cut
  ropConsCut, relmapCut, relCut,
  -- * pick
  ropConsPick, relmapPick, relPick,
  -- * rename
  ropConsRename, relmapRename, relRename
) where

import qualified Data.List  as List
import qualified Data.Maybe as Maybe
import qualified Data.Tuple as Tuple

import Koshucode.Baala.Base
import Koshucode.Baala.Core
import Koshucode.Baala.Builtin



-- ----------------------  reldee & reldum

ropConsReldee, ropConsReldum :: RopCons c
ropConsReldee use = Right $ relmapConst use "reldee" reldee
ropConsReldum use = Right $ relmapConst use "reldum" reldum


-- ----------------------  source

ropConsSource :: RopCons c
ropConsSource use =
  do sign <- getWord  use "-sign"
     ns   <- getTerms use "-term"
     Right $ relmapSource use sign ns


-- ----------------------  id

ropConsId :: RopCons c
ropConsId use = Right $ relmapId use

relmapId :: RopUse c -> Relmap c
relmapId use = relmapCalc use "id" sub where
    sub _ = relId

{-| Identity mapping, i.e., do nothing. -}
relId :: AbMap (Rel c)
relId = Right



-- ----------------------  empty

ropConsEmpty :: RopCons c
ropConsEmpty use = Right $ relmapEmpty use

relmapEmpty :: RopUse c -> Relmap c
relmapEmpty use = relmapCalc use "empty" sub where
    sub _ = relEmpty

{-| Throw away all tuples in a relation. -}
relEmpty :: AbMap (Rel c)  -- ^ Any relation to empty relation
relEmpty (Rel h1 _) = Right $ Rel h1 []



-- ----------------------  pick

ropConsPick :: (Ord c) => RopCons c
ropConsPick use =
  do ns <- getTerms use "-term"
     Right $ relmapPick use ns

relmapPick :: (Ord c) => RopUse c -> [String] -> Relmap c
relmapPick use ns = relmapCalc use "pick" sub where
    sub _ = relPick ns

relPick
    :: (Ord c)
    => [String]        -- ^ Term names
    -> AbMap (Rel c)   -- ^ Mapping relation to relation
relPick ns r = project indexPick ns r

project :: (Ord c) => ([Int] -> Listmap c) -> [String] -> AbMap (Rel c)
project f ns (Rel h1 b1)
    | null non   = Right $ Rel h2 b2
    | otherwise  = Left  $ AbortNoTerms non
    where
      non = headNonExistTerms h1 ns
      pos = List.sort $ posOf h1 (map singleton ns)
      pj  = f $ posPoss pos
      h2  = headChange pj h1
      b2  = unique $ map pj b1



-- ----------------------  cut

ropConsCut :: (Ord c) => RopCons c
ropConsCut use =
  do ns <- getTerms use "-term"
     Right $ relmapCut use ns

relmapCut :: (Ord c) => RopUse c -> [String] -> Relmap c
relmapCut use ns = relmapCalc use "cut" sub where
    sub _ = relCut ns

relCut
    :: (Ord c)
    => [String]        -- ^ Term names
    -> AbMap (Rel c)   -- ^ Mapping relation to relation
relCut ns r = project indexCut ns r



-- ----------------------  rename

ropConsRename :: RopCons c
ropConsRename use = do
  np <- getTermPairs use "-term"
  Right $ relmapRename use np

relmapRename :: RopUse c -> [(String, String)] -> Relmap c
relmapRename use np = relmapCalc use "rename" sub where
    sub _ = relRename np

{-| Change terms names -}
relRename
    :: [(String, String)]   -- ^ List of term name (/to/, /from/)
    -> AbMap (Rel c)        -- ^ Relation to relation
relRename np (Rel h1 b1)
    | nsCheck /= []  =  Left  $ AbortReqNewTerms nsCheck
    | psCheck /= []  =  Left  $ AbortNoTerms psCheck
    | otherwise      =  Right $ Rel h2 b1
    where
      (ns, ps) = unzip np
      nsCheck  = headExistTerms    h1 ns
      psCheck  = headNonExistTerms h1 ps
      h2 = headChange (map re) h1
      pn = map Tuple.swap np
      re p = Maybe.fromMaybe p $ lookup p pn

