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

  -- * move
  consMove, relmapMove, relkitMove,

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

relmapPick :: C.RopUse c -> [B.TermName] -> C.Relmap c
relmapPick use = C.relmapFlow use . relkitPick

relkitPick :: [B.TermName] -> C.RelkitCalc c
relkitPick = relkitSnip B.snipFrom B.snipFrom

consCut :: C.RopCons c
consCut use =
  do ns <- Op.getTerms use "-term"
     Right $ relmapCut use ns

relmapCut :: C.RopUse c -> [B.TermName] -> C.Relmap c
relmapCut use = C.relmapFlow use . relkitCut

relkitCut :: [B.TermName] -> C.RelkitCalc c
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

relkitSnipTerm :: B.Snip B.Term -> B.Snip c -> C.RelkitBinary c
relkitSnipTerm _ _ (C.Relkit Nothing _) = const $ Right C.relkitNothing
relkitSnipTerm heSnip boSnip (C.Relkit (Just he2) _) =
    relkitSnip heSnip boSnip $ B.headNames he2

relkitSnip :: B.Snip B.Term -> B.Snip c -> [B.TermName] -> C.RelkitCalc c
relkitSnip _ _ _ Nothing = Right C.relkitNothing
relkitSnip heSnip boSnip ns (Just he1)
    | B.sameLength ns ind1 = Right kit2
    | otherwise = Message.unkTerm non he1
    where
      he2   = B.headChange (heSnip ind1) he1
      kit2  = C.relkitJust he2 $ C.RelkitOneToOne True $ boSnip ind1
      ns1   = B.headNames he1
      non   = B.snipOff ind ns
      ind1  = ns  `B.snipIndex` ns1
      ind   = ns1 `B.snipIndex` ns


-- ----------------------  move

consMove :: C.RopCons c
consMove use =
  do ps <- Op.getTerms use "-term"
     ns <- Op.getTerms use "-to"
     Right $ relmapMove use (ps, ns)

relmapMove :: C.RopUse c -> ([B.TermName], [B.TermName]) -> C.Relmap c
relmapMove use = C.relmapFlow use . relkitMove

relkitMove :: ([B.TermName], [B.TermName]) -> C.RelkitCalc c
relkitMove _ Nothing = Right C.relkitNothing
relkitMove (ps, ns) (Just he1)
    | B.notSameLength ps ns = Message.oddAttr
    | dup     /= []         = Message.dupTerm dup he2
    | psLeft  /= []         = Message.unkTerm psLeft he1
    | otherwise             = Right kit2
    where
      ns1      = B.headNames he1
      ns2      = B.headNames he2
      dup      = B.duplicate ns2

      psLeft   = ps `B.snipLeft`  ns1
      psInd    = ps `B.snipIndex` ns1

      he2      = B.headChange move he1
      kit2     = C.relkitJust he2 C.RelkitId

      move ts  = foldr mv ts mvlist
      mv (i,n) = B.termChange (const n) `at` i
      mvlist   = zip psInd ns

at :: (B.Map a) -> Int -> B.Map [a]
at f = loop where
    loop 0 (x : xs) = f x : xs
    loop i (x : xs) = x : loop (i - 1) xs
    loop _ []       = []



-- ----------------------  rename

consRename :: C.RopCons c
consRename use =
  do np <- Op.getTermPairs use "-term"
     Right $ relmapRename use np

relmapRename :: C.RopUse c -> [B.TermName2] -> C.Relmap c
relmapRename use = C.relmapFlow use . relkitRename

relkitRename :: [B.TermName2] -> C.RelkitCalc c
relkitRename _ Nothing = Right C.relkitNothing
relkitRename np (Just he1)
    | nsShare /= [] = Message.reqNewTerm nsShare he1
    | psLeft  /= [] = Message.unkTerm    psLeft  he1
    | otherwise     = Right kit2
    where
      (ns, ps) = unzip np
      ns1      = B.headNames he1
      nsShare  = ns `B.snipShare` ns1
      psLeft   = ps `B.snipLeft`  ns1

      pn       = map Tuple.swap np
      ren p    = Maybe.fromMaybe p $ lookup p pn

      he2      = B.headRename ren he1
      kit2     = C.relkitJust he2 C.RelkitId


