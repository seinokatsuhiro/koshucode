{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Rop.Flat.Term
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
    -- * forward
    consForward, relmapForward,
    -- * backward
    consBackward, relmapBackward,
    -- * lexical
    consLexical, relmapLexical,
    -- * order
    consOrder, relmapOrder, relkitOrder,
  ) where

import qualified Koshucode.Baala.Base        as B
import qualified Koshucode.Baala.Data        as D
import qualified Koshucode.Baala.Core        as C
import qualified Koshucode.Baala.Rop.Base    as Op
import qualified Koshucode.Baala.Rop.Flat.Message as Msg


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
    [ Op.def   consBackward       "backward /P ..."          "V -term"
    , Op.def   consCut            "cut /P ..."               "V -term"
    , Op.def   consCutTerm        "cut-term /R"              "1 -relmap/"
    , Op.def   consForward        "forward /P ..."           "V -term"
    , Op.def   consLexical        "lexical"                  "0"
    , Op.def   consOrder          "order /P ..."             "V -term"
    , Op.def   consPick           "pick /P ..."              "V -term"
    , Op.def   consPickTerm       "pick-term /R"             "1 -relmap/"
    , Op.def   consRename         "rename /N /P ..."         "V -term"
    , Op.def   consMove           "move /P ... -to /N ..."   "V -from | -to"
    ]



-- ----------------------  pick & cut

consPick :: C.RopCons c
consPick med =
  do ns <- Op.getTerms med "-term"
     Right $ relmapPick med ns

relmapPick :: C.Intmed c -> [D.TermName] -> C.Relmap c
relmapPick med = C.relmapFlow med . relkitPick

relkitPick :: [D.TermName] -> C.RelkitFlow c
relkitPick = relkitProject (D.headRShare, D.headRShare)

consCut :: C.RopCons c
consCut med =
  do ns <- Op.getTerms med "-term"
     Right $ relmapCut med ns

relmapCut :: C.Intmed c -> [D.TermName] -> C.Relmap c
relmapCut med = C.relmapFlow med . relkitCut

relkitCut :: [D.TermName] -> C.RelkitFlow c
relkitCut = relkitProject (D.headRSide, D.headRSide)


-- ----------------------  pick-term & cut-term

consPickTerm :: C.RopCons c
consPickTerm med =
  do rmap <- Op.getRelmap med "-relmap"
     Right $ relmapPickTerm med rmap

relmapPickTerm :: C.Intmed c -> C.Relmap c -> C.Relmap c
relmapPickTerm med = C.relmapBinary med relkitPickTerm

relkitPickTerm :: C.RelkitBinary c
relkitPickTerm = relkitProjectTerm (D.headRShare, D.headRShare)

consCutTerm :: C.RopCons c
consCutTerm med =
  do rmap <- Op.getRelmap med "-relmap"
     Right $ relmapCutTerm med rmap

relmapCutTerm :: C.Intmed c -> C.Relmap c -> C.Relmap c
relmapCutTerm med = C.relmapBinary med relkitCutTerm

relkitCutTerm :: C.RelkitBinary c
relkitCutTerm = relkitProjectTerm (D.headRSide, D.headRSide)


-- ----------------------  project

relkitProjectTerm :: D.HeadLRMap2 D.NamedType c -> C.RelkitBinary c
relkitProjectTerm _ (C.Relkit _ Nothing _) = const $ Right C.relkitNothing
relkitProjectTerm lrMap (C.Relkit _ (Just he2) _) =
    relkitProject lrMap $ D.headNames he2

relkitProject :: D.HeadLRMap2 D.NamedType c -> [D.TermName] -> C.RelkitFlow c
relkitProject _ _ Nothing = Right C.relkitNothing
relkitProject (heMap, boMap) ns (Just he1)
    | null unk   = Right kit2
    | otherwise  = Msg.unkTerm unk he1
    where
      lr    = ns `D.headLROrd` D.headNames he1
      unk   = D.headLSideNames lr
      he2   = heMap lr `D.headMap` he1
      kit2  = C.relkitJust he2 $ C.RelkitOneToOne True $ boMap lr


-- ----------------------  rename

consRename :: C.RopCons c
consRename med =
  do np <- Op.getTermPairs med "-term"
     Right $ relmapRename med np

relmapRename :: C.Intmed c -> [D.TermName2] -> C.Relmap c
relmapRename med = C.relmapFlow med . relkitMove . unzip . map B.swap


-- ----------------------  move

consMove :: C.RopCons c
consMove med =
  do ps <- Op.getTerms med "-from"
     ns <- Op.getTerms med "-to"
     Right $ relmapMove med (ps, ns)

relmapMove :: C.Intmed c -> ([D.TermName], [D.TermName]) -> C.Relmap c
relmapMove med = C.relmapFlow med . relkitMove

relkitMove :: ([D.TermName], [D.TermName]) -> C.RelkitFlow c
relkitMove _ Nothing = Right C.relkitNothing
relkitMove (ps, ns) (Just he1)
    | B.notSameLength ps ns = Msg.oddAttr
    | psDup   /= []         = Msg.dupTerm psDup he1
    | ns2Dup  /= []         = Msg.dupTerm ns2Dup he2
    | psLeft  /= []         = Msg.unkTerm psLeft he1
    | otherwise             = Right kit2
    where
      ns1        =  D.headNames he1
      ns2        =  D.headNames he2

      ns2Dup     =  B.duplicates ns2
      psDup      =  B.duplicates ps

      psLeft     =  ps `B.snipLeft`  ns1
      psIndex    =  ps `B.snipIndex` ns1

      he2        =  D.headMap moveTerms he1
      kit2       =  C.relkitJust he2 C.RelkitId

      moveTerms ts       =  foldr moveTerm ts $ zip ns psIndex
      moveTerm (n, i)    =  moveName n `B.mapAt` i
      moveName n (_, t)  =  (n, t)


-- ----------------------  forward & backward

consForward :: C.RopCons c
consForward med =
  do ns <- Op.getTerms med "-term"
     Right $ relmapForward med ns

relmapForward :: C.Intmed c -> [D.TermName] -> C.Relmap c
relmapForward med = C.relmapFlow med . relkitToward (D.headRForward, D.headRForward)

consBackward :: C.RopCons c
consBackward med =
  do ns <- Op.getTerms med "-term"
     Right $ relmapBackward med ns

relmapBackward :: C.Intmed c -> [D.TermName] -> C.Relmap c
relmapBackward med = C.relmapFlow med . relkitToward (D.headRBackward, D.headRBackward)

relkitToward :: D.HeadLRMap2 D.NamedType c -> [D.TermName] -> C.RelkitFlow c
relkitToward _ _ Nothing = Right C.relkitNothing
relkitToward (heMap, boMap) ns (Just he1)
    | null unk   = Right kit2
    | otherwise  = Msg.unkTerm unk he1
    where
      lr    = ns `D.headLR` D.headNames he1
      unk   = D.headLSide lr ns
      he2   = D.headMap (heMap lr) he1
      kit2  = C.relkitJust he2 $ C.RelkitOneToOne False $ boMap lr


-- ----------------------  lexical

consLexical :: C.RopCons c
consLexical med = Right $ relmapLexical med

relmapLexical :: C.Intmed c -> C.Relmap c
relmapLexical med = C.relmapFlow med relkitLexical

relkitLexical :: C.RelkitFlow c
relkitLexical Nothing = Right C.relkitNothing
relkitLexical (Just he1) = Right kit2 where
    ns    = D.headNames he1
    lr    = B.sort ns `D.headLR` ns
    he2   = D.headMap (D.headRForward lr) he1
    kit2  = C.relkitJust he2 $ C.RelkitOneToOne False $ D.headRForward lr


-- ----------------------  order

consOrder :: (Ord c) => C.RopCons c
consOrder med =
    do ns <- Op.getOption [] Op.getTerms med "-term"
       Right $ relmapOrder med ns

relmapOrder :: (Ord c) => C.Intmed c -> [D.TermName] -> C.Relmap c
relmapOrder med = C.relmapFlow med . relkitOrder

relkitOrder :: (Ord c) => [D.TermName] -> C.RelkitFlow c
relkitOrder _ Nothing = Right C.relkitNothing
relkitOrder ns (Just he1) = Right kit2 where
    kit2  = C.relkitJust he1 $ C.RelkitFull False kitf2
    kitf2 = B.sortByName ords $ D.headNames he1
    ords  = map B.Asc ns
