{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Minimal.Term
( -- * pick
  consPick, relmapPick,
  -- * cut
  consCut, relmapCut,
  -- * rename
  consRename, relmapRename,
) where

import qualified Data.List  as List
import qualified Data.Maybe as Maybe
import qualified Data.Tuple as Tuple
import qualified Koshucode.Baala.Base    as B
import qualified Koshucode.Baala.Core    as C
import qualified Koshucode.Baala.Builtin as Rop



-- ----------------------  pick

consPick :: (Ord c) => C.RopCons c
consPick use =
  do ns <- Rop.getTerms use "-term"
     Right $ relmapPick use ns

relmapPick :: (Ord c) => C.RopUse c -> [B.Termname] -> C.Relmap c
relmapPick use ns = C.relmapCalc use $ relkitPick ns

relkitPick
    :: [B.Termname]        -- ^ Names of picking terms
    -> B.Relhead           -- ^ Heading of input relation
    -> B.Ab (C.Relkit c)   -- ^ Relfier for output relation
relkitPick ns = relkitArrange B.arrangePick B.arrangePick ns



-- ----------------------  cut

consCut :: (Ord c) => C.RopCons c
consCut use =
  do ns <- Rop.getTerms use "-term"
     Right $ relmapCut use ns

relmapCut :: (Ord c) => C.RopUse c -> [B.Termname] -> C.Relmap c
relmapCut use ns = C.relmapCalc use $ relkitCut ns

relkitCut
    :: [B.Termname]        -- ^ Names of cutting terms
    -> B.Relhead           -- ^ Heading of input relation
    -> B.Ab (C.Relkit c)   -- ^ Relfier for output relation
relkitCut ns = relkitArrange B.arrangeCut B.arrangeCut ns

relkitArrange
    :: B.Arrange B.Termname
    -> B.Arrange c
    -> [B.Termname]
    -> B.Relhead
    -> B.Ab (C.Relkit c)
relkitArrange ha ba ns h1
    | null non  = Right $ C.relkit h2 (C.RelkitOneToOne True $ ba ind)
    | otherwise = Left $ B.AbortAnalysis [] $ B.AANoTerms non
    where
      non =  B.headDropTerms h1 ns
      pos :: [B.TermPos]
      pos =  List.sort $ h1 `B.posFor` ns

      ind :: [Int]
      ind =  map B.posIndex pos

      h2  =  B.headChange (ha ind) h1



-- ----------------------  rename

consRename :: C.RopCons c
consRename use =
  do np <- Rop.getTermPairs use "-term"
     Right $ relmapRename use np

relmapRename :: C.RopUse c -> [(B.Termname, B.Termname)] -> C.Relmap c
relmapRename use np = C.relmapCalc use $ relkitRename np

{-| Change terms names -}
relkitRename
    :: [(B.Termname, B.Termname)]  -- ^ List of termnames (/to/, /from/)
    -> B.Relhead                   -- ^ Heading of input relation
    -> B.Ab (C.Relkit c)           -- ^ Relfier for output relation
relkitRename np h1
    | nsCheck /= [] = Left  $ B.AbortAnalysis [] $ B.AAReqNewTerms nsCheck
    | psCheck /= [] = Left  $ B.AbortAnalysis [] $ B.AANoTerms psCheck
    | otherwise     = Right $ C.relkit h2 C.RelkitId
    where
      (ns, ps) = unzip np
      nsCheck  = B.headKeepTerms h1 ns
      psCheck  = B.headDropTerms h1 ps
      h2       = B.headChange (map rename) h1
      pn       = map Tuple.swap np
      rename p = Maybe.fromMaybe p $ lookup p pn

