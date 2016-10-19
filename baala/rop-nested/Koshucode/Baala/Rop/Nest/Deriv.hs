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

import qualified Koshucode.Baala.Overture        as O
import qualified Koshucode.Baala.Base            as B
import qualified Koshucode.Baala.Syntax          as S
import qualified Koshucode.Baala.Data            as D
import qualified Koshucode.Baala.Core            as C
import qualified Koshucode.Baala.Rop.Base        as Op
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
     sh   <- Op.getMaybe Op.getTerms med "-share"
     Right $ relmapOppGroup med sh n rmap

-- | Create @odd-group@ relmap.
relmapOppGroup :: (Ord c, D.CRel c) => C.Intmed c -> Op.SharedTerms -> S.TermName -> O.Map (C.Relmap c)
relmapOppGroup med sh n rmap = C.relmapCopy med n rmapGroup where
    rmapGroup  = rmap B.<> Op.relmapGroup med sh n rmapLocal
    rmapLocal  = C.relmapLocalSymbol med n


-- ----------------------  join-up

consJoinUp :: (Ord c) => C.RopCons c
consJoinUp med =
  do nest <- Op.getTerms med "-term"
     Right $ relmapJoinUp med nest

-- | Create @join-up@ relmap.
relmapJoinUp :: (Ord c) => C.Intmed c -> [S.TermName] -> C.Relmap c
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

consPickGroup :: (Ord c, D.CRel c) => C.RopCons c
consPickGroup med =
  do ns <- Op.getTerms med "-term"
     to <- Op.getTerm  med "-to"
     Right $ relmapNest med (True, ns, to)

-- | Create @self-group@ relmap.
relmapNest :: (Ord c, D.CRel c) => C.Intmed c -> (Bool, [S.TermName], S.TermName) -> C.Relmap c
relmapNest med (co, ns, to) = group B.<> for where
    group  = relmapOppGroup med Nothing to key
    for    = Op.relmapFor med to nest
    key    = if co then pick else cut
    nest   = if co then cut  else pick
    pick   = Op.relmapPick med ns
    cut    = Op.relmapCut  med ns


-- ----------------------  ungroup

-- $Ungroup
--
--  > ungroup /g
--  > slice-up ( meet ^/g ) | cut /g

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

