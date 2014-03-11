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

import qualified Data.List               as List
import qualified Data.Maybe              as Maybe
import qualified Data.Tuple              as Tuple
import qualified Koshucode.Baala.Base    as B
import qualified Koshucode.Baala.Core    as C
import qualified Koshucode.Baala.Builtin as Rop



-- ----------------------  pick & cut

consPick :: (Ord c) => C.RopCons c
consPick use =
  do ns <- Rop.getTerms use "-term"
     Right $ relmapPick use ns

relmapPick :: (Ord c) => C.RopUse c -> [B.Termname] -> C.Relmap c
relmapPick use = C.relmapFlow use . relkitPick

relkitPick :: [B.Termname] -> C.RelkitCalc c
relkitPick = relkitArrange B.arrangePick B.arrangePick

consCut :: (Ord c) => C.RopCons c
consCut use =
  do ns <- Rop.getTerms use "-term"
     Right $ relmapCut use ns

relmapCut :: (Ord c) => C.RopUse c -> [B.Termname] -> C.Relmap c
relmapCut use = C.relmapFlow use . relkitCut

relkitCut :: [B.Termname] -> C.RelkitCalc c
relkitCut = relkitArrange B.arrangeCut B.arrangeCut

relkitArrange
    :: B.Arrange B.Termname
    -> B.Arrange c
    -> [B.Termname]
    -> C.RelkitCalc c
relkitArrange _ _ _ Nothing = Right C.relkitNothing
relkitArrange hearr boarr ns (Just he1)
    | null non  = Right $ C.relkitJust he2 $ C.RelkitOneToOne True $ boarr ind
    | otherwise = Left $ B.AbortAnalysis [] $ B.AANoTerms non
    where
      non =  B.headDropTerms he1 ns

      pos :: [B.TermPos]
      pos =  List.sort $ he1 `B.posFor` ns

      ind :: [Int]
      ind =  map B.posIndex pos

      he2 =  B.headChange (hearr ind) he1



-- ----------------------  rename

consRename :: C.RopCons c
consRename use =
  do np <- Rop.getTermPairs use "-term"
     Right $ relmapRename use np

relmapRename :: C.RopUse c -> [(B.Termname, B.Termname)] -> C.Relmap c
relmapRename use = C.relmapFlow use . relkitRename

{-| Change terms names -}
relkitRename :: [(B.Termname, B.Termname)] -> C.RelkitCalc c
relkitRename _ Nothing = Right C.relkitNothing
relkitRename np (Just he1)
    | nsCheck /= [] = Left  $ B.AbortAnalysis [] $ B.AAReqNewTerms nsCheck
    | psCheck /= [] = Left  $ B.AbortAnalysis [] $ B.AANoTerms     psCheck
    | otherwise     = Right $ C.relkitJust he2 C.RelkitId
    where
      (ns, ps) = unzip np
      nsCheck  = B.headKeepTerms he1 ns
      psCheck  = B.headDropTerms he1 ps
      he2      = B.headChange (map rename) he1
      pn       = map Tuple.swap np
      rename p = Maybe.fromMaybe p $ lookup p pn

