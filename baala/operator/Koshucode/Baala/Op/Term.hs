{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Term
  ( ropsTerm,
    -- * pick
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
    consRename, relmapRename,
  ) where

import qualified Koshucode.Baala.Base       as B
import qualified Koshucode.Baala.Core       as C
import qualified Koshucode.Baala.Op.Builtin as Op
import qualified Koshucode.Baala.Op.Message as Msg


-- | Relmap operators for manipulating term names.
--
--   [@cut@]        Project relation to unspecified terms.
--
--   [@cut-term@]   Project relation to terms not in relmap output.
--
--   [@pick@]       Project relation to specified terms.
--
--   [@pick-term@]  Project relation to terms in relmap output.
--
--   [@rename@]     Change term name.
--
--   [@move@]       Change heading.
--
ropsTerm :: (Ord c) => [C.Rop c]
ropsTerm = Op.ropList "term"  -- GROUP
    --         CONSTRUCTOR        USAGE                      ATTRIBUTE
    [ Op.def   consCut            "cut /P ..."               "V -term"
    , Op.def   consCutTerm        "cut-term /R"              "1 -relmap/"
    , Op.def   consPick           "pick /P ..."              "V -term"
    , Op.def   consPickTerm       "pick-term /R"             "1 -relmap/"
    , Op.def   consRename         "rename /N /P ..."         "V -term"
    , Op.def   consMove           "move /P ... -to /N ..."   "V -from | -to"
    ]



-- ----------------------  pick & cut

consPick :: C.RopCons c
consPick use =
  do ns <- Op.getTerms use "-term"
     Right $ relmapPick use ns

relmapPick :: C.Intmed c -> [B.TermName] -> C.Relmap c
relmapPick use = C.relmapFlow use . relkitPick

relkitPick :: [B.TermName] -> C.RelkitFlow c
relkitPick = relkitSnip B.snipFrom B.snipFrom

consCut :: C.RopCons c
consCut use =
  do ns <- Op.getTerms use "-term"
     Right $ relmapCut use ns

relmapCut :: C.Intmed c -> [B.TermName] -> C.Relmap c
relmapCut use = C.relmapFlow use . relkitCut

relkitCut :: [B.TermName] -> C.RelkitFlow c
relkitCut = relkitSnip B.snipOff B.snipOff


-- ----------------------  pick-term & cut-term

consPickTerm :: C.RopCons c
consPickTerm use =
  do rmap <- Op.getRelmap use "-relmap"
     Right $ relmapPickTerm use rmap

relmapPickTerm :: C.Intmed c -> C.Relmap c -> C.Relmap c
relmapPickTerm use = C.relmapBinary use relkitPickTerm

relkitPickTerm :: C.RelkitBinary c
relkitPickTerm = relkitSnipTerm B.snipFrom B.snipFrom

consCutTerm :: C.RopCons c
consCutTerm use =
  do rmap <- Op.getRelmap use "-relmap"
     Right $ relmapCutTerm use rmap

relmapCutTerm :: C.Intmed c -> C.Relmap c -> C.Relmap c
relmapCutTerm use = C.relmapBinary use relkitCutTerm

relkitCutTerm :: C.RelkitBinary c
relkitCutTerm = relkitSnipTerm B.snipOff B.snipOff


-- ----------------------  snip

relkitSnipTerm :: B.Snip B.NamedType -> B.Snip c -> C.RelkitBinary c
relkitSnipTerm _ _ (C.Relkit Nothing _) = const $ Right C.relkitNothing
relkitSnipTerm heSnip boSnip (C.Relkit (Just he2) _) =
    relkitSnip heSnip boSnip $ B.headNames he2

relkitSnip :: B.Snip B.NamedType -> B.Snip c -> [B.TermName] -> C.RelkitFlow c
relkitSnip _ _ _ Nothing = Right C.relkitNothing
relkitSnip heSnip boSnip ns (Just he1)
    | B.sameLength ns ind1 = Right kit2
    | otherwise = Msg.unkTerm non he1
    where
      he2   =  B.headMap (heSnip ind1) he1
      kit2  =  C.relkitJust he2 $ C.RelkitOneToOne True $ boSnip ind1
      ns1   =  B.headNames he1
      non   =  B.snipOff ind ns
      ind1  =  ns  `B.snipIndex` ns1
      ind   =  ns1 `B.snipIndex` ns


-- ----------------------  rename

consRename :: C.RopCons c
consRename use =
  do np <- Op.getTermPairs use "-term"
     Right $ relmapRename use np

relmapRename :: C.Intmed c -> [B.TermName2] -> C.Relmap c
relmapRename use = C.relmapFlow use . relkitMove . unzip . map B.swap


-- ----------------------  move

consMove :: C.RopCons c
consMove use =
  do ps <- Op.getTerms use "-from"
     ns <- Op.getTerms use "-to"
     Right $ relmapMove use (ps, ns)

relmapMove :: C.Intmed c -> ([B.TermName], [B.TermName]) -> C.Relmap c
relmapMove use = C.relmapFlow use . relkitMove

relkitMove :: ([B.TermName], [B.TermName]) -> C.RelkitFlow c
relkitMove _ Nothing = Right C.relkitNothing
relkitMove (ps, ns) (Just he1)
    | B.notSameLength ps ns = Msg.oddAttr
    | psDup   /= []         = Msg.dupTerm psDup he1
    | ns2Dup  /= []         = Msg.dupTerm ns2Dup he2
    | psLeft  /= []         = Msg.unkTerm psLeft he1
    | otherwise             = Right kit2
    where
      ns1        =  B.headNames he1
      ns2        =  B.headNames he2

      ns2Dup     =  B.duplicates ns2
      psDup      =  B.duplicates ps

      psLeft     =  ps `B.snipLeft`  ns1
      psIndex    =  ps `B.snipIndex` ns1

      he2        =  B.headMap moveTerms he1
      kit2       =  C.relkitJust he2 C.RelkitId

      moveTerms ts       =  foldr moveTerm ts $ zip ns psIndex
      moveTerm (n, i)    =  moveName n `B.mapAt` i
      moveName n (_, t)  =  (n, t)

