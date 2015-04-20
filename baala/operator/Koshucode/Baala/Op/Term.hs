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
    -- * forward
    consForward, relmapForward,
    -- * backward
    consBackward, relmapBackward,
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
    [ Op.def   consBackward       "backward /P ..."          "V -term"
    , Op.def   consCut            "cut /P ..."               "V -term"
    , Op.def   consCutTerm        "cut-term /R"              "1 -relmap/"
    , Op.def   consForward        "forward /P ..."           "V -term"
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

relmapPick :: C.Intmed c -> [B.TermName] -> C.Relmap c
relmapPick med = C.relmapFlow med . relkitPick

relkitPick :: [B.TermName] -> C.RelkitFlow c
relkitPick = relkitSnip B.headRShare B.headRShare

consCut :: C.RopCons c
consCut med =
  do ns <- Op.getTerms med "-term"
     Right $ relmapCut med ns

relmapCut :: C.Intmed c -> [B.TermName] -> C.Relmap c
relmapCut med = C.relmapFlow med . relkitCut

relkitCut :: [B.TermName] -> C.RelkitFlow c
relkitCut = relkitSnip B.headRSide B.headRSide


-- ----------------------  pick-term & cut-term

consPickTerm :: C.RopCons c
consPickTerm med =
  do rmap <- Op.getRelmap med "-relmap"
     Right $ relmapPickTerm med rmap

relmapPickTerm :: C.Intmed c -> C.Relmap c -> C.Relmap c
relmapPickTerm med = C.relmapBinary med relkitPickTerm

relkitPickTerm :: C.RelkitBinary c
relkitPickTerm = relkitSnipTerm B.headRShare B.headRShare

consCutTerm :: C.RopCons c
consCutTerm med =
  do rmap <- Op.getRelmap med "-relmap"
     Right $ relmapCutTerm med rmap

relmapCutTerm :: C.Intmed c -> C.Relmap c -> C.Relmap c
relmapCutTerm med = C.relmapBinary med relkitCutTerm

relkitCutTerm :: C.RelkitBinary c
relkitCutTerm = relkitSnipTerm B.headRSide B.headRSide


-- ----------------------  snip

relkitSnipTerm :: B.HeadLRMap B.NamedType -> B.HeadLRMap c -> C.RelkitBinary c
relkitSnipTerm _ _ (C.Relkit _ Nothing _) = const $ Right C.relkitNothing
relkitSnipTerm heSnip boSnip (C.Relkit _ (Just he2) _) =
    relkitSnip heSnip boSnip $ B.headNames he2

relkitSnip :: B.HeadLRMap B.NamedType -> B.HeadLRMap c -> [B.TermName] -> C.RelkitFlow c
relkitSnip _ _ _ Nothing = Right C.relkitNothing
relkitSnip heSnip boSnip ns (Just he1)
    | null unk   = Right kit2
    | otherwise  = Msg.unkTerm unk he1
    where
      lr    = ns `B.headLROrd` B.headNames he1
      unk   = B.headLSideNames lr
      he2   = heSnip lr `B.headMap` he1
      kit2  = C.relkitJust he2 $ C.RelkitOneToOne True $ boSnip lr


-- ----------------------  rename

consRename :: C.RopCons c
consRename med =
  do np <- Op.getTermPairs med "-term"
     Right $ relmapRename med np

relmapRename :: C.Intmed c -> [B.TermName2] -> C.Relmap c
relmapRename med = C.relmapFlow med . relkitMove . unzip . map B.swap


-- ----------------------  move

consMove :: C.RopCons c
consMove med =
  do ps <- Op.getTerms med "-from"
     ns <- Op.getTerms med "-to"
     Right $ relmapMove med (ps, ns)

relmapMove :: C.Intmed c -> ([B.TermName], [B.TermName]) -> C.Relmap c
relmapMove med = C.relmapFlow med . relkitMove

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


-- ----------------------  forward & backward

consForward :: C.RopCons c
consForward med =
  do ns <- Op.getTerms med "-term"
     Right $ relmapForward med ns

relmapForward :: C.Intmed c -> [B.TermName] -> C.Relmap c
relmapForward med = C.relmapFlow med . relkitToward B.snipForward2

consBackward :: C.RopCons c
consBackward med =
  do ns <- Op.getTerms med "-term"
     Right $ relmapBackward med ns

relmapBackward :: C.Intmed c -> [B.TermName] -> C.Relmap c
relmapBackward med = C.relmapFlow med . relkitToward B.snipBackward2

relkitToward :: B.Snip2 B.NamedType c -> [B.TermName] -> C.RelkitFlow c
relkitToward _ _ Nothing = Right C.relkitNothing
relkitToward (heSnip, boSnip) ns (Just he1)
    | B.sameLength ns ind1 = Right kit2
    | otherwise = Msg.unkTerm non he1
    where
      he2   = B.headMap (heSnip ind1) he1
      kit2  = C.relkitJust he2 $ C.RelkitOneToOne False $ boSnip ind1
      ns1   = B.headNames he1
      non   = B.snipOff ind ns
      ind1  = ns  `B.snipIndex` ns1
      ind   = ns1 `B.snipIndex` ns
