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

import qualified Koshucode.Baala.DataPlus         as K
import qualified Koshucode.Baala.Core             as C
import qualified Koshucode.Baala.Rop.Base         as Rop
import qualified Koshucode.Baala.Rop.Flat.Message as Msg


-- | Relmap operators for manipulating term names.
ropsTerm :: (Ord c) => [C.Rop c]
ropsTerm = Rop.rops "term"
    [ consCut        K.& [ "cut /P ..."               K.& "-term*" ]
    , consCutTerm    K.& [ "cut-term R"               K.& "-relmap/" ]
    , consMove       K.& [ "move /P ... -to /N ..."   K.& "-from* . -to" ]
    , consPick       K.& [ "pick /P ..."              K.& "-term*" ]
    , consPickTerm   K.& [ "pick-term R"              K.& "-relmap/" ]
    , consRename     K.& [ "rename /N /P ..."         K.& "-term*" ]
    ]


-- ----------------------  pick & cut

-- | [pick \/P ...]
--    Project relation to specified terms.
consPick :: C.RopCons c
consPick med =
  do ns <- Rop.getTerms med "-term"
     Right $ relmapPick med ns

-- | Create @pick@ relmap.
relmapPick :: C.Intmed c -> [K.TermName] -> C.Relmap c
relmapPick med = C.relmapFlow med . relkitPick

-- | Create @pick@ relkit.
relkitPick :: [K.TermName] -> C.RelkitFlow c
relkitPick = relkitProj K.pickTerms2

-- | [cut \/P ...]
--    Project relation to unspecified terms.
--    This operator is an inversion of @pick@.
consCut :: C.RopCons c
consCut med =
  do ns <- Rop.getTerms med "-term"
     Right $ relmapCut med ns

-- | Create @cut@ relmap.
relmapCut :: C.Intmed c -> [K.TermName] -> C.Relmap c
relmapCut med = C.relmapFlow med . relkitCut

-- | Create @cut@ relkit.
relkitCut :: [K.TermName] -> C.RelkitFlow c
relkitCut = relkitProj K.cutTerms2


-- ----------------------  pick-term & cut-term

-- | [pick-term /R/]
--
--   Construct @pick-term@ relmap operator.
--   This operator is similar to @pick@
--   instead of given terms but relation terms.
--
consPickTerm :: C.RopCons c
consPickTerm med =
  do rmap <- Rop.getRelmap med "-relmap"
     Right $ relmapPickTerm med rmap

-- | Create @pick-term@ relmap.
relmapPickTerm :: C.Intmed c -> C.Relmap c -> C.Relmap c
relmapPickTerm med = C.relmapBinary med relkitPickTerm

-- | Create @pick-term@ relkit.
relkitPickTerm :: C.RelkitBinary c
relkitPickTerm = relkitProjTerm K.pickTerms2

-- | [cut-term /R/]
--
--   Construct @cut-term@ relmap operator.
--   This operator is similar to @cut@
--   instead of given terms but relation terms.
--
consCutTerm :: C.RopCons c
consCutTerm med =
  do rmap <- Rop.getRelmap med "-relmap"
     Right $ relmapCutTerm med rmap

-- | Create @cut-term@ relmap.
relmapCutTerm :: C.Intmed c -> C.Relmap c -> C.Relmap c
relmapCutTerm med = C.relmapBinary med relkitCutTerm

-- | Create @cut-term@ relkit.
relkitCutTerm :: C.RelkitBinary c
relkitCutTerm = relkitProjTerm K.cutTerms2

relkitProjTerm :: K.TermPick2 K.TypeTerm c -> C.RelkitBinary c
relkitProjTerm pk (C.RelkitOutput he2 _) = relkitProj pk $ K.getTermNames he2
relkitProjTerm _ _ = const $ Right C.relkitNothing

relkitProj :: K.TermPick2 K.TypeTerm c -> [K.TermName] -> C.RelkitFlow c
relkitProj _ _ Nothing = Right C.relkitNothing
relkitProj (hePick, boPick) ns (Just he1)
    | K.newTermsExist pk  = Msg.newTerm pk he1
    | otherwise           = Right kit2
    where
      pk    = K.termPicker ns he1
      he2   = hePick pk `K.headMap` he1
      kit2  = C.relkitLinear he2 True $ boPick pk


-- ----------------------  move

-- | [move \/P ... -to \/N ...]
--
--    Rename term names.
--    Terms \/P ... of input relation are renamed corresponding terms \/N ....
--    The @move@ operator provides same functionality as @rename@.
--    Operator @move@ is suitable for renaming few terms,
--    operator @rename@ for many terms.
--    The following relmaps have same effect:
--
--    > rename /N /P /N /P ...
--    > move /P ... -to /N ...
--
consMove :: C.RopCons c
consMove med =
  do ps <- Rop.getTerms med "-from"
     ns <- Rop.getTerms med "-to"
     Right $ relmapMove med (ps, ns)

-- | Create @move@ relmap.
relmapMove :: C.Intmed c -> ([K.TermName], [K.TermName]) -> C.Relmap c
relmapMove med = C.relmapFlow med . relkitMove

-- | Create @move@ relkit.
relkitMove :: ([K.TermName], [K.TermName]) -> C.RelkitFlow c
relkitMove _ Nothing = Right C.relkitNothing
relkitMove (ps, ns) (Just he1)
    | K.notSameLength ps ns  = Msg.unevenTerms ps ns
    | K.duplicated ps        = Msg.dupTerm ps   -- from terms
    | K.duplicated ns        = Msg.dupTerm ns   -- to terms
    | K.duplicated ns2       = Msg.dupTerm ns2  -- output names
    | K.newTermsExist pk     = Msg.newTerm pk he1
    | otherwise              = Right kit2
    where
      pk             = K.termPicker ps he1
      he2            = K.headMap terms he1
      ns2            = K.getTermNames he2
      kit2           = C.relkitId (Just he2)
      ni             = zip ns $ K.termsIndex pk
      terms nt       = foldr term nt ni
      term (n, i)    = move n `K.mapAt` i
      move n (_, t)  = (n, t) -- name and type


-- ----------------------  rename

-- | [rename \/N \/P \/N \/P ...]
--
--    Rename term names.
--    Term \/P is renamed to term \/N for each pair of terms \/N \/P.
--    The @rename@ operator provides same functionality as @move@.
--
consRename :: C.RopCons c
consRename med =
  do np <- Rop.getTermPairs med "-term"
     Right $ relmapRename med np

-- | Create @rename@ relmap.
relmapRename :: C.Intmed c -> [K.TermName2] -> C.Relmap c
relmapRename med = C.relmapFlow med . relkitMove . unzip . map K.swap
