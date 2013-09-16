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
  ropConsEmpty, relmapEmpty, relgenEmpty,
  -- * pick
  ropConsPick, relmapPick, relgenPick,
  -- * cut
  ropConsCut, relmapCut, relgenCut,
  -- * rename
  ropConsRename, relmapRename, relgenRename
) where

import qualified Data.List  as List
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
relmapId use = C.relmapCalc use "id" C.relgenId where

{-| Identity mapping, i.e., do nothing. -}
relId :: B.AbMap (B.Rel c)
relId = Right



-- ----------------------  empty

ropConsEmpty :: C.RopCons c
ropConsEmpty use = Right $ relmapEmpty use

relmapEmpty :: C.RopUse c -> C.Relmap c
relmapEmpty use = C.relmapCalc use "empty" gen where
    gen _ = relgenEmpty

{-| Throw away all tuples in a relation. -}
relgenEmpty :: B.Relhead -> B.Ab (C.Relgen c)
relgenEmpty h = Right $ C.Relgen h (C.RelgenConst [])



-- ----------------------  pick

ropConsPick :: (Ord c) => C.RopCons c
ropConsPick use =
  do ns <- getTerms use "-term"
     Right $ relmapPick use ns

relmapPick :: (Ord c) => C.RopUse c -> [String] -> C.Relmap c
relmapPick use ns = C.relmapCalc use "pick" gen where
    gen _ = relgenPick ns

relgenPick
    :: [String]            -- ^ Names of picking terms
    -> B.Relhead           -- ^ Heading of input relation
    -> B.Ab (C.Relgen c)   -- ^ Generator for output relation
relgenPick ns h = relgenArrange B.arrangePick B.arrangePick ns h



-- ----------------------  cut

ropConsCut :: (Ord c) => C.RopCons c
ropConsCut use =
  do ns <- getTerms use "-term"
     Right $ relmapCut use ns

relmapCut :: (Ord c) => C.RopUse c -> [String] -> C.Relmap c
relmapCut use ns = C.relmapCalc use "cut" gen where
    gen _ = relgenCut ns

relgenCut
    :: [String]            -- ^ Names of cutting terms
    -> B.Relhead           -- ^ Heading of input relation
    -> B.Ab (C.Relgen c)   -- ^ Generator for output relation
relgenCut ns h = relgenArrange B.arrangeCut B.arrangeCut ns h

relgenArrange
    :: B.Arrange String
    -> B.Arrange c
    -> [String]
    -> B.Relhead
    -> B.Ab (C.Relgen c)
relgenArrange ha ba ns h1
    | null non  = Right $ C.Relgen h2 (C.RelgenOneToOne $ ba ind)
    | otherwise = Left  $ B.AbortNoTerms non
    where
    non =  B.headNonExistTerms h1 ns
    pos :: [B.TermPos]
    pos =  List.sort $ h1 `B.posFlat` ns

    ind :: [Int]
    ind =  map B.posIndex pos

    h2  =  B.headChange (ha ind) h1



-- ----------------------  rename

ropConsRename :: C.RopCons c
ropConsRename use = do
  np <- getTermPairs use "-term"
  Right $ relmapRename use np

relmapRename :: C.RopUse c -> [(String, String)] -> C.Relmap c
relmapRename use np = C.relmapCalc use "rename" gen where
    gen _ = relgenRename np

{-| Change terms names -}
relgenRename
    :: [(String, String)]   -- ^ List of termnames (/to/, /from/)
    -> B.Relhead            -- ^ Heading of input relation
    -> B.Ab (C.Relgen c)    -- ^ Generator for output relation
relgenRename np h1
    | nsCheck /= [] = Left  $ B.AbortReqNewTerms nsCheck
    | psCheck /= [] = Left  $ B.AbortNoTerms psCheck
    | otherwise     = Right $ C.Relgen h2 (C.RelgenOneToOne id)
    where
      (ns, ps) = unzip np
      nsCheck  = B.headExistTerms    h1 ns
      psCheck  = B.headNonExistTerms h1 ps
      h2 = B.headChange (map re) h1
      pn = map Tuple.swap np
      re p = Maybe.fromMaybe p $ lookup p pn

