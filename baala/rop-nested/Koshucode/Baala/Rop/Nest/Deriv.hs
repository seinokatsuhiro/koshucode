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
import qualified Koshucode.Baala.Base            as B
import qualified Koshucode.Baala.Syntax          as S
import qualified Koshucode.Baala.Data            as D
import qualified Koshucode.Baala.Core            as C
import qualified Koshucode.Baala.Rop.Base        as Op
import qualified Koshucode.Baala.Rop.Flat        as Op
import qualified Koshucode.Baala.Rop.Nest.Confl  as Op


-- ----------------------  opp-group

-- | __opp-group R -to \/N [-share \/P ...]__
--
--   Opposite operand version of @group@ -- grouping relation by relmap output.
--
consOppGroup :: (Ord c, D.CRel c) => C.RopCons c
consOppGroup med =
  do rmap <- Op.getRelmap med "-relmap"
     n    <- Op.getTerm   med "-to"
     sh   <- Op.getMaybe Op.getTerms med "-share"
     Right $ relmapOppGroup med sh n rmap

-- | Create @odd-group@ relmap.
relmapOppGroup :: (Ord c, D.CRel c) => C.Intmed c -> Op.SharedTerms -> S.TermName -> O.Map (C.Relmap c)
relmapOppGroup med sh n rmap = C.relmapCopy med n rmapGroup where
    rmapGroup  = rmap B.<> Op.relmapGroup med sh n rmapLocal
    rmapLocal  = C.relmapLocalSymbol med n


-- ----------------------  self-group

-- | __nest [~] \/P ... -to \/N__
consNest :: (Ord c, D.CRel c) => C.RopCons c
consNest med =
  do (co, ns) <- Op.getTermsCo med "-term"
     to       <- Op.getTerm    med "-to"
     Right $ relmapSelfGroup med (co, ns, to)

-- | __self-group \/P ... -to \/N__
--
--   Group input relation into the term \/N per \/P ....
--
consSelfGroup :: (Ord c, D.CRel c) => C.RopCons c
consSelfGroup med =
  do ns <- Op.getTerms med "-term"
     to <- Op.getTerm  med "-to"
     Right $ relmapSelfGroup med (True, ns, to)

-- | Create @self-group@ relmap.
relmapSelfGroup :: (Ord c, D.CRel c) => C.Intmed c -> (Bool, [S.TermName], S.TermName) -> C.Relmap c
relmapSelfGroup med (co, ns, to) = group B.<> for where
    group  = relmapOppGroup med Nothing to key
    for    = Op.relmapFor med to nest
    key    = if co then pick else cut
    nest   = if co then cut  else pick
    pick   = Op.relmapPick med ns
    cut    = Op.relmapCut  med ns


-- ----------------------  ungroup

-- | __ungroup \/P__
--
--   Lift up nested relation \/P and meet with non-nested terms.
--
consUngroup :: (Ord c, D.CRel c) => C.RopCons c
consUngroup med =
  do n <- Op.getTerm med "-term"
     Right $ relmapUngroup med n

-- | Create @ungroup@ relmap.
relmapUngroup :: (Ord c, D.CRel c) => C.Intmed c -> S.TermName -> C.Relmap c
relmapUngroup med n = ungroup where
    ungroup = slice B.<> cut
    slice   = Op.relmapSliceUp med meet
    meet    = Op.relmapMeet med Nothing $ C.relmapLocalNest med n
    cut     = Op.relmapCut  med [n]


-- ----------------------  join-up

-- | __join-up \/P ...__
--
--   Join nested relations \/P ... and up to output relation.
--
consJoinUp :: (Ord c) => C.RopCons c
consJoinUp med =
  do nest <- Op.getTerms med "-term"
     Right $ relmapJoinUp med nest

-- | Create @join-up@ relmap.
relmapJoinUp :: (Ord c) => C.Intmed c -> [S.TermName] -> C.Relmap c
relmapJoinUp med nest = C.relmapNest med $ Op.relmapJoinList med rmaps where
    rmaps   = link `map` nest
    link n  = C.relmapLocalNest med n
