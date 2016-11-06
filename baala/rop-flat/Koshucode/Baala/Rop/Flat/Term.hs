{-# OPTIONS_GHC -Wall #-}

-- | Relmap operators for manipulating term names.

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

-- | __pick \/P ...__
--
--   Construct @pick@ relmap operator
--   for relational projection by picking terms.
--
consPick :: C.RopCons c
consPick med =
  do ns <- Op.getTerms med "-term"
     Right $ relmapPick med ns

-- | Create @pick@ relmap.
relmapPick :: C.Intmed c -> [S.TermName] -> C.Relmap c
relmapPick med = C.relmapFlow med . relkitPick

-- | Create @pick@ relkit.
relkitPick :: [S.TermName] -> C.RelkitFlow c
relkitPick = relkitProject (D.ssRShare, D.ssRShare)

-- | __cut \/P ...__
--
--   Construct @cut@ relmap operator
--   for relational projection by cutting terms.
--   This operator is an inversion of @pick@.
--
consCut :: C.RopCons c
consCut med =
  do ns <- Op.getTerms med "-term"
     Right $ relmapCut med ns

-- | Create @cut@ relmap.
relmapCut :: C.Intmed c -> [S.TermName] -> C.Relmap c
relmapCut med = C.relmapFlow med . relkitCut

-- | Create @cut@ relkit.
relkitCut :: [S.TermName] -> C.RelkitFlow c
relkitCut = relkitProject (D.ssRSide, D.ssRSide)


-- ----------------------  pick-term & cut-term

-- | __pick-term R__
--
--   Construct @pick-term@ relmap operator.
--   This operator is similar to @pick@
--   instead of given terms but relation terms.
--
consPickTerm :: C.RopCons c
consPickTerm med =
  do rmap <- Op.getRelmap med "-relmap"
     Right $ relmapPickTerm med rmap

-- | Create @pick-term@ relmap.
relmapPickTerm :: C.Intmed c -> C.Relmap c -> C.Relmap c
relmapPickTerm med = C.relmapBinary med relkitPickTerm

-- | Create @pick-term@ relkit.
relkitPickTerm :: C.RelkitBinary c
relkitPickTerm = relkitProjectTerm (D.ssRShare, D.ssRShare)

-- | __cut-term R__
--
--   Construct @cut-term@ relmap operator.
--   This operator is similar to @cut@
--   instead of given terms but relation terms.
--
consCutTerm :: C.RopCons c
consCutTerm med =
  do rmap <- Op.getRelmap med "-relmap"
     Right $ relmapCutTerm med rmap

-- | Create @cut-term@ relmap.
relmapCutTerm :: C.Intmed c -> C.Relmap c -> C.Relmap c
relmapCutTerm med = C.relmapBinary med relkitCutTerm

-- | Create @cut-term@ relkit.
relkitCutTerm :: C.RelkitBinary c
relkitCutTerm = relkitProjectTerm (D.ssRSide, D.ssRSide)


-- ----------------------  project

relkitProjectTerm :: D.TermPick2 D.NamedType c -> C.RelkitBinary c
relkitProjectTerm _ (C.Relkit _ Nothing _) = const $ Right C.relkitNothing
relkitProjectTerm lrMap (C.Relkit _ (Just he2) _) =
    relkitProject lrMap $ D.getTermNames he2

relkitProject :: D.TermPick2 D.NamedType c -> [S.TermName] -> C.RelkitFlow c
relkitProject _ _ Nothing = Right C.relkitNothing
relkitProject (hePick, boPick) ns (Just he1)
    | D.newTermsExist pk  = Msg.unkTerm (D.newTerms pk) he1
    | otherwise           = Right kit2
    where
      pk    = D.termPicker ns he1
      he2   = hePick pk `D.headMap` he1
      kit2  = C.relkitJust he2 $ C.RelkitOneToOne True $ boPick pk


-- ----------------------  move

-- | __move \/P ... -to \/N ...__
--
--   Construct @move@ relmap operator for renaming term names.
--   Terms \/P ... of input relation are renamed corresponding terms \/N ....
--   The @move@ operator provides same functionality as @rename@.
--   Operator @move@ is suitable for renaming few terms,
--   operator @rename@ for many terms.
--   The following relmaps have same effect:
--
--   > rename /N /P /N /P ...
--   > move /P ... -to /N ...
--
consMove :: C.RopCons c
consMove med =
  do ps <- Op.getTerms med "-from"
     ns <- Op.getTerms med "-to"
     Right $ relmapMove med (ps, ns)

-- | Create @move@ relmap.
relmapMove :: C.Intmed c -> ([S.TermName], [S.TermName]) -> C.Relmap c
relmapMove med = C.relmapFlow med . relkitMove

-- | Create @move@ relkit.
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


-- ----------------------  rename

-- | __rename \/N \/P \/N \/P ...__
--
--   Construct @rename@ relmap operator for renaming term names.
--   Term \/P is renamed to term \/N for each pair of terms \/N \/P.
--   The @rename@ operator provides same functionality as @move@.
--
consRename :: C.RopCons c
consRename med =
  do np <- Op.getTermPairs med "-term"
     Right $ relmapRename med np

-- | Create @rename@ relmap.
relmapRename :: C.Intmed c -> [S.TermName2] -> C.Relmap c
relmapRename med = C.relmapFlow med . relkitMove . unzip . map B.swap
