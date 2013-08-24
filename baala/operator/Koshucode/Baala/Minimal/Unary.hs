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
  -- * pick
  ropConsPick, relmapPick, relPick,
  -- * cut
  ropConsCut, relmapCut, relCut,
  -- * front
  ropConsFront, relmapFront, relFront,
  -- * rename
  ropConsRename, relmapRename, relRename
) where

import qualified Data.Maybe as Maybe
import qualified Data.Tuple as Tuple

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C
import Koshucode.Baala.Builtin



-- ----------------------  reldee & reldum

ropConsReldee, ropConsReldum :: C.RopCons c
ropConsReldee use = Right $ C.relmapConst use "reldee" B.reldee
ropConsReldum use = Right $ C.relmapConst use "reldum" B.reldum


-- ----------------------  source

ropConsSource :: C.RopCons c
ropConsSource use =
  do sign <- getWord  use "-sign"
     ns   <- getTerms use "-term"
     Right $ C.relmapSource use sign ns


-- ----------------------  id

ropConsId :: C.RopCons c
ropConsId use = Right $ relmapId use

relmapId :: C.RopUse c -> C.Relmap c
relmapId use = C.relmapCalc use "id" sub where
    sub _ = relId

{-| Identity mapping, i.e., do nothing. -}
relId :: B.AbMap (B.Rel c)
relId = Right



-- ----------------------  empty

ropConsEmpty :: C.RopCons c
ropConsEmpty use = Right $ relmapEmpty use

relmapEmpty :: C.RopUse c -> C.Relmap c
relmapEmpty use = C.relmapCalc use "empty" sub where
    sub _ = relEmpty

{-| Throw away all tuples in a relation. -}
relEmpty :: B.AbMap (B.Rel c)  -- ^ Any relation to empty relation
relEmpty (B.Rel h1 _) = Right $ B.Rel h1 []



-- ----------------------  pick

ropConsPick :: (Ord c) => C.RopCons c
ropConsPick use =
  do ns <- getTerms use "-term"
     Right $ relmapPick use ns

relmapPick :: (Ord c) => C.RopUse c -> [String] -> C.Relmap c
relmapPick use ns = C.relmapCalc use "pick" sub where
    sub _ = relPick ns

relPick
    :: (Ord c)
    => [String]            -- ^ Term names
    -> B.AbMap (B.Rel c)   -- ^ Mapping relation to relation
relPick ns r = B.arrangeRel B.arrangePick B.arrangePick ns r



-- ----------------------  cut

ropConsCut :: (Ord c) => C.RopCons c
ropConsCut use =
  do ns <- getTerms use "-term"
     Right $ relmapCut use ns

relmapCut :: (Ord c) => C.RopUse c -> [String] -> C.Relmap c
relmapCut use ns = C.relmapCalc use "cut" sub where
    sub _ = relCut ns

relCut
    :: (Ord c)
    => [String]            -- ^ Term names
    -> B.AbMap (B.Rel c)   -- ^ Mapping relation to relation
relCut ns r = B.arrangeRel B.arrangeCut B.arrangeCut ns r



-- ----------------------  front

ropConsFront :: (Ord c) => C.RopCons c
ropConsFront use =
  do ns <- getTerms use "-term"
     Right $ relmapFront use ns

relmapFront :: (Ord c) => C.RopUse c -> [String] -> C.Relmap c
relmapFront use ns = C.relmapCalc use "front" sub where
    sub _ = relFront ns

relFront
    :: (Ord c)
    => [String]            -- ^ Term names
    -> B.AbMap (B.Rel c)   -- ^ Mapping relation to relation
relFront ns r = B.arrangeRel B.arrangeFore B.arrangeFore ns r



-- ----------------------  rename

ropConsRename :: C.RopCons c
ropConsRename use = do
  np <- getTermPairs use "-term"
  Right $ relmapRename use np

relmapRename :: C.RopUse c -> [(String, String)] -> C.Relmap c
relmapRename use np = C.relmapCalc use "rename" sub where
    sub _ = relRename np

{-| Change terms names -}
relRename
    :: [(String, String)]   -- ^ List of term name (/to/, /from/)
    -> B.AbMap (B.Rel c)    -- ^ Relation to relation
relRename np (B.Rel h1 b1)
    | nsCheck /= []  =  Left  $ B.AbortReqNewTerms nsCheck
    | psCheck /= []  =  Left  $ B.AbortNoTerms psCheck
    | otherwise      =  Right $ B.Rel h2 b1
    where
      (ns, ps) = unzip np
      nsCheck  = B.headExistTerms    h1 ns
      psCheck  = B.headNonExistTerms h1 ps
      h2 = B.headChange (map re) h1
      pn = map Tuple.swap np
      re p = Maybe.fromMaybe p $ lookup p pn

