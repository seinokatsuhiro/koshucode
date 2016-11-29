{-# OPTIONS_GHC -Wall #-}

-- | Derived operators for nested relations.

module Koshucode.Baala.Rop.Nest.Deriv
  ( -- * opp-group
    consOppGroup,
    -- * self-group
    consSelfGroup, consNest, relmapSelfGroup,
    -- * ungroup
    consUngroup, relmapUngroup,
    -- * join-up
    consJoinUp, relmapJoinUp,
  ) where

import qualified Koshucode.Baala.Overture        as O
import qualified Koshucode.Baala.Syntax          as S
import qualified Koshucode.Baala.Data            as D
import qualified Koshucode.Baala.Core            as C
import qualified Koshucode.Baala.Rop.Base        as Rop
import qualified Koshucode.Baala.Rop.Flat        as Rop
import qualified Koshucode.Baala.Rop.Nest.Confl  as Rop


-- ----------------------  opp-group

-- | __opp-group R -to \/N [-share \/P ...]__
--
--   Opposite operand version of @group@ -- grouping relation by relmap output.
--
consOppGroup :: (Ord c, D.CRel c) => C.RopCons c
consOppGroup med =
  do rmap <- Rop.getRelmap med "-relmap"
     n    <- Rop.getTerm   med "-to"
     sh   <- Rop.getMaybe Rop.getTerms med "-share"
     Right $ relmapOppGroup med sh n rmap

-- | Create @odd-group@ relmap.
relmapOppGroup :: (Ord c, D.CRel c) => C.Intmed c -> Rop.SharedTerms -> S.TermName -> O.Map (C.Relmap c)
relmapOppGroup med sh n rmap = C.relmapCopy med n' rmapGroup where
    rmapGroup  = rmap O.++ Rop.relmapGroup med sh n rmapLocal
    rmapLocal  = C.relmapLocalSymbol med n'
    n'         = S.termNameContent n


-- ----------------------  self-group

-- | __nest [~] \/P ... -to \/N__
consNest :: (Ord c, D.CRel c) => C.RopCons c
consNest med =
  do (co, ns) <- Rop.getTermsCo med "-term"
     to       <- Rop.getTerm    med "-to"
     Right $ relmapSelfGroup med (co, ns, to)

-- | __self-group \/P ... -to \/N__
--
--   Group input relation into the term \/N per \/P ....
--
consSelfGroup :: (Ord c, D.CRel c) => C.RopCons c
consSelfGroup med =
  do ns <- Rop.getTerms med "-term"
     to <- Rop.getTerm  med "-to"
     Right $ relmapSelfGroup med (True, ns, to)

-- | Create @self-group@ relmap.
relmapSelfGroup :: (Ord c, D.CRel c) => C.Intmed c -> (Bool, [S.TermName], S.TermName) -> C.Relmap c
relmapSelfGroup med (co, ns, to) = group O.++ for where
    group  = relmapOppGroup med Nothing to key
    for    = Rop.relmapFor med to nest
    key    = if co then pick else cut
    nest   = if co then cut  else pick
    pick   = Rop.relmapPick med ns
    cut    = Rop.relmapCut  med ns


-- ----------------------  ungroup

-- | __ungroup \/P__
--
--   Lift up nested relation \/P and meet with non-nested terms.
--
consUngroup :: (Ord c, D.CRel c) => C.RopCons c
consUngroup med =
  do n <- Rop.getTerm med "-term"
     Right $ relmapUngroup med n

-- | Create @ungroup@ relmap.
relmapUngroup :: (Ord c, D.CRel c) => C.Intmed c -> S.TermName -> C.Relmap c
relmapUngroup med n = ungroup where
    ungroup = slice O.++ cut
    slice   = Rop.relmapSliceUp med meet
    meet    = Rop.relmapMeet med Nothing $ C.relmapLocalNest med n
    cut     = Rop.relmapCut  med [n]


-- ----------------------  join-up

-- | __join-up \/P ...__
--
--   Join nested relations \/P ... and up to output relation.
--
consJoinUp :: (Ord c) => C.RopCons c
consJoinUp med =
  do nest <- Rop.getTerms med "-term"
     Right $ relmapJoinUp med nest

-- | Create @join-up@ relmap.
relmapJoinUp :: (Ord c) => C.Intmed c -> [S.TermName] -> C.Relmap c
relmapJoinUp med nest = C.relmapNest med $ Rop.relmapJoinList med rmaps where
    rmaps   = link `map` nest
    link n  = C.relmapLocalNest med n
