{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Minimal.Term
( -- * pick
  consPick, relmapPick, relkitPick,
  -- * cut
  consCut, relmapCut, relkitCut,
  -- * pick-term
  consPickTerm, relmapPickTerm, relkitPickTerm,
  -- * cut-term
  consCutTerm, relmapCutTerm, relkitCutTerm,
  -- * rename
  consRename, relmapRename, relkitRename,
) where

import qualified Data.Maybe                 as Maybe
import qualified Data.Tuple                 as Tuple
import qualified Koshucode.Baala.Base       as B
import qualified Koshucode.Baala.Core       as C
import qualified Koshucode.Baala.Op.Builtin as Op
import qualified Koshucode.Baala.Op.Message as Message



-- ----------------------  pick & cut

consPick :: C.RopCons c
consPick use =
  do ns <- Op.getTerms use "-term"
     Right $ relmapPick use ns

relmapPick :: C.RopUse c -> [B.Termname] -> C.Relmap c
relmapPick use = C.relmapFlow use . relkitPick

relkitPick :: [B.Termname] -> C.RelkitCalc c
relkitPick = relkitSnip B.snipFrom B.snipFrom

consCut :: C.RopCons c
consCut use =
  do ns <- Op.getTerms use "-term"
     Right $ relmapCut use ns

relmapCut :: C.RopUse c -> [B.Termname] -> C.Relmap c
relmapCut use = C.relmapFlow use . relkitCut

relkitCut :: [B.Termname] -> C.RelkitCalc c
relkitCut = relkitSnip B.snipOff B.snipOff


-- ----------------------  pick-term & cut-term

consPickTerm :: C.RopCons c
consPickTerm use =
  do rmap <- Op.getRelmap use
     Right $ relmapPickTerm use rmap

relmapPickTerm :: C.RopUse c -> C.Relmap c -> C.Relmap c
relmapPickTerm use = C.relmapBinary use relkitPickTerm

relkitPickTerm :: C.RelkitBinary c
relkitPickTerm = relkitSnipTerm B.snipFrom B.snipFrom

consCutTerm :: C.RopCons c
consCutTerm use =
  do rmap <- Op.getRelmap use
     Right $ relmapCutTerm use rmap

relmapCutTerm :: C.RopUse c -> C.Relmap c -> C.Relmap c
relmapCutTerm use = C.relmapBinary use relkitCutTerm

relkitCutTerm :: C.RelkitBinary c
relkitCutTerm = relkitSnipTerm B.snipOff B.snipOff


-- ----------------------  snip

relkitSnipTerm :: B.Snip B.Termname -> B.Snip c -> C.RelkitBinary c
relkitSnipTerm _ _ (C.Relkit Nothing _) = const $ Right C.relkitNothing
relkitSnipTerm heSnip boSnip (C.Relkit (Just he2) _) =
    relkitSnip heSnip boSnip $ B.headNames he2

relkitSnip :: B.Snip B.Termname -> B.Snip c -> [B.Termname] -> C.RelkitCalc c
relkitSnip _ _ _ Nothing = Right C.relkitNothing
relkitSnip heSnip boSnip ns (Just he1)
    | null non  = Right kit2
    | otherwise = Message.noTerm non
    where
      non  = B.headDropTerms he1 ns
      he2  = B.headChange (heSnip ind) he1
      kit2 = C.relkitJust he2 $ C.RelkitOneToOne True $ boSnip ind
      ind  = ns `B.snipIndex` B.headNames he1


-- ----------------------  rename

consRename :: C.RopCons c
consRename use =
  do np <- Op.getTermPairs use "-term"
     Right $ relmapRename use np

relmapRename :: C.RopUse c -> [(B.Termname, B.Termname)] -> C.Relmap c
relmapRename use = C.relmapFlow use . relkitRename

{-| Change terms names -}
relkitRename :: [(B.Termname, B.Termname)] -> C.RelkitCalc c
relkitRename _ Nothing = Right C.relkitNothing
relkitRename np (Just he1)
    | nsCheck /= [] = Message.reqNewTerm nsCheck
    | psCheck /= [] = Message.noTerm     psCheck
    | otherwise     = Right kit2
    where
      (ns, ps) = unzip np
      nsCheck  = B.headKeepTerms he1 ns
      psCheck  = B.headDropTerms he1 ps

      pn       = map Tuple.swap np
      rename p = Maybe.fromMaybe p $ lookup p pn

      he2      = B.headChange (map rename) he1
      kit2     = C.relkitJust he2 C.RelkitId

