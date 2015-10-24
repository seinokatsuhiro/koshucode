{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Derived operators for nested relations.

module Koshucode.Baala.Rop.Nest.Deriv
  ( 
    -- * opp-group
    consOppGroup,
    -- $OppGroupExample
  
    -- * join-up
    consJoinUp, relmapJoinUp,
  
    -- * nest / pick-group
    consPickGroup, consNest, relmapNest,
    -- $Nest
  
    -- * ungroup
    consUngroup, relmapUngroup,
  ) where

import qualified Koshucode.Baala.Base            as B
import qualified Koshucode.Baala.Data            as D
import qualified Koshucode.Baala.Core            as C
import qualified Koshucode.Baala.Rop.Builtin     as Op
import qualified Koshucode.Baala.Rop.Flat        as Op
import qualified Koshucode.Baala.Rop.Nest.Confl  as Op



-- ----------------------  opp-group

-- $OppGroupExample
--
--  Opposite operand version of group -- grouping relation by relmap output.
--
--   > source P /a /b | opp-group ( pick /a ) -to /g

consOppGroup :: (Ord c, D.CRel c) => C.RopCons c
consOppGroup med =
  do rmap <- Op.getRelmap med "-relmap"
     n    <- Op.getTerm   med "-to"
     Right $ relmapOppGroup med n rmap

relmapOppGroup :: (Ord c, D.CRel c) => C.Intmed c -> D.TermName -> B.Map (C.Relmap c)
relmapOppGroup med n rmap = C.relmapCopy med n rmapGroup where
    rmapGroup  = rmap `B.mappend` Op.relmapGroup med n rmapLocal
    rmapLocal  = C.relmapLocalSymbol med n


-- ----------------------  join-up

consJoinUp :: (Ord c) => C.RopCons c
consJoinUp med =
  do nest <- Op.getTerms med "-term"
     Right $ relmapJoinUp med nest

relmapJoinUp :: (Ord c) => C.Intmed c -> [D.TermName] -> C.Relmap c
relmapJoinUp med nest = C.relmapNest med $ Op.relmapJoinList med rmaps where
    rmaps   = link `map` nest
    link n  = C.relmapLocalNest med n


-- ----------------------  nest / pick-group

-- $Nest
--
--  Make nested relation @\/g@ that has term @\/y@ and @\/z@.
--
--    > nest /y /z -to /g
--
--  Make nested relation @\/g@ for each term @\/x@.
--
--    > pick-group /x -to /g

consNest :: (Ord c, D.CRel c) => C.RopCons c
consNest med =
  do (co, ns) <- Op.getTermsCo med "-term"
     to       <- Op.getTerm    med "-to"
     Right $ relmapNest med (co, ns, to)

relmapNest :: (Ord c, D.CRel c) => C.Intmed c -> (Bool, [D.TermName], D.TermName) -> C.Relmap c
relmapNest med (co, ns, to) = group `B.mappend` for where
    group  = relmapOppGroup med to key
    for    = Op.relmapFor med to nest
    key    = if co then pick else cut
    nest   = if co then cut  else pick
    pick   = Op.relmapPick med ns
    cut    = Op.relmapCut  med ns

consPickGroup :: (Ord c, D.CRel c) => C.RopCons c
consPickGroup med =
  do ns <- Op.getTerms med "-term"
     to <- Op.getTerm  med "-to"
     Right $ relmapNest med (True, ns, to)


-- ----------------------  ungroup

-- $Ungroup
--
--  > ungroup /g
--  > slice-up ( meet ^/g ) | cut /g

consUngroup :: (Ord c, D.CRel c) => C.RopCons c
consUngroup med =
  do n <- Op.getTerm med "-term"
     Right $ relmapUngroup med n

relmapUngroup :: (Ord c, D.CRel c) => C.Intmed c -> D.TermName -> C.Relmap c
relmapUngroup med n = ungroup where
    ungroup = slice `B.mappend` cut
    slice   = Op.relmapSliceUp med meet
    meet    = Op.relmapMeet med $ C.relmapLocalNest med n
    cut     = Op.relmapCut  med [n]

