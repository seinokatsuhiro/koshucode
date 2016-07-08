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
  ) where

import qualified Koshucode.Baala.Base             as B
import qualified Koshucode.Baala.Syntax           as S
import qualified Koshucode.Baala.Data             as D
import qualified Koshucode.Baala.Core             as C
import qualified Koshucode.Baala.Rop.Base         as Op
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
    [ Op.def   consCut            "cut /P ..."               "-term*"
    , Op.def   consCutTerm        "cut-term /R"              "-relmap/"
    , Op.def   consPick           "pick /P ..."              "-term*"
    , Op.def   consPickTerm       "pick-term /R"             "-relmap/"
    , Op.def   consRename         "rename /N /P ..."         "-term*"
    , Op.def   consMove           "move /P ... -to /N ..."   "-from* . -to"
    ]



-- ----------------------  pick & cut

consPick :: C.RopCons c
consPick med =
  do ns <- Op.getTerms med "-term"
     Right $ relmapPick med ns

relmapPick :: C.Intmed c -> [S.TermName] -> C.Relmap c
relmapPick med = C.relmapFlow med . relkitPick

relkitPick :: [S.TermName] -> C.RelkitFlow c
relkitPick = relkitProject (D.ssRShare, D.ssRShare)

consCut :: C.RopCons c
consCut med =
  do ns <- Op.getTerms med "-term"
     Right $ relmapCut med ns

relmapCut :: C.Intmed c -> [S.TermName] -> C.Relmap c
relmapCut med = C.relmapFlow med . relkitCut

relkitCut :: [S.TermName] -> C.RelkitFlow c
relkitCut = relkitProject (D.ssRSide, D.ssRSide)


-- ----------------------  pick-term & cut-term

consPickTerm :: C.RopCons c
consPickTerm med =
  do rmap <- Op.getRelmap med "-relmap"
     Right $ relmapPickTerm med rmap

relmapPickTerm :: C.Intmed c -> C.Relmap c -> C.Relmap c
relmapPickTerm med = C.relmapBinary med relkitPickTerm

relkitPickTerm :: C.RelkitBinary c
relkitPickTerm = relkitProjectTerm (D.ssRShare, D.ssRShare)

consCutTerm :: C.RopCons c
consCutTerm med =
  do rmap <- Op.getRelmap med "-relmap"
     Right $ relmapCutTerm med rmap

relmapCutTerm :: C.Intmed c -> C.Relmap c -> C.Relmap c
relmapCutTerm med = C.relmapBinary med relkitCutTerm

relkitCutTerm :: C.RelkitBinary c
relkitCutTerm = relkitProjectTerm (D.ssRSide, D.ssRSide)


-- ----------------------  project

relkitProjectTerm :: D.ShareSideMap2 D.NamedType c -> C.RelkitBinary c
relkitProjectTerm _ (C.Relkit _ Nothing _) = const $ Right C.relkitNothing
relkitProjectTerm lrMap (C.Relkit _ (Just he2) _) =
    relkitProject lrMap $ D.getTermNames he2

relkitProject :: D.ShareSideMap2 D.NamedType c -> [S.TermName] -> C.RelkitFlow c
relkitProject _ _ Nothing = Right C.relkitNothing
relkitProject (heMap, boMap) ns (Just he1)
    | null unk   = Right kit2
    | otherwise  = Msg.unkTerm unk he1
    where
      lr    = D.shareSideOrd ns he1
      unk   = D.ssLSideNames lr
      he2   = heMap lr `D.headMap` he1
      kit2  = C.relkitJust he2 $ C.RelkitOneToOne True $ boMap lr


-- ----------------------  rename

consRename :: C.RopCons c
consRename med =
  do np <- Op.getTermPairs med "-term"
     Right $ relmapRename med np

relmapRename :: C.Intmed c -> [S.TermName2] -> C.Relmap c
relmapRename med = C.relmapFlow med . relkitMove . unzip . map B.swap


-- ----------------------  move

consMove :: C.RopCons c
consMove med =
  do ps <- Op.getTerms med "-from"
     ns <- Op.getTerms med "-to"
     Right $ relmapMove med (ps, ns)

relmapMove :: C.Intmed c -> ([S.TermName], [S.TermName]) -> C.Relmap c
relmapMove med = C.relmapFlow med . relkitMove

relkitMove :: ([S.TermName], [S.TermName]) -> C.RelkitFlow c
relkitMove _ Nothing = Right C.relkitNothing
relkitMove (ps, ns) (Just he1)
    | B.notSameLength ps ns = Msg.oddAttr
    | psDup   /= []         = Msg.dupTerm psDup he1
    | ns2Dup  /= []         = Msg.dupTerm ns2Dup he2
    | psLeft  /= []         = Msg.unkTerm psLeft he1
    | otherwise             = Right kit2
    where
      ns1        =  D.getTermNames he1
      ns2        =  D.getTermNames he2

      ns2Dup     =  B.duplicates ns2
      psDup      =  B.duplicates ps

      psLeft     =  ps `B.snipLeft`  ns1
      psIndex    =  ps `B.snipIndex` ns1

      he2        =  D.headMap moveTerms he1
      kit2       =  C.relkitJust he2 C.RelkitId

      moveTerms ts       =  foldr moveTerm ts $ zip ns psIndex
      moveTerm (n, i)    =  moveName n `B.mapAt` i
      moveName n (_, t)  =  (n, t)

