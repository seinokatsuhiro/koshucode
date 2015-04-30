{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Derived operators for nested relations.

module Koshucode.Baala.Op.Nest.Deriv
  ( 
    -- * group-by
    consGroupBy,
    -- $GroupByExample
  
    -- * join-up
    consJoinUp, relmapJoinUp,
  
    -- * nest / pick-group
    consPickGroup, consNest, relmapNest,
    -- $Nest
  
    -- * unnest
    consUnnest, relmapUnnest,
  ) where

import qualified Koshucode.Baala.Base          as B
import qualified Koshucode.Baala.Core          as C
import qualified Koshucode.Baala.Op.Builtin    as Op
import qualified Koshucode.Baala.Op.Lattice    as Op
import qualified Koshucode.Baala.Op.Term       as Op
import qualified Koshucode.Baala.Op.Nest.Confl as Op



-- ----------------------  group-by

-- $GroupByExample
--
--  Grouping relation by relmap output.
--
--   > source P /a /b | group-by /g ( pick /a )

consGroupBy :: (Ord c, C.CRel c) => C.RopCons c
consGroupBy med =
  do n    <- Op.getTerm   med "-term"
     rmap <- Op.getRelmap med "-relmap"
     Right $ relmapGroupBy med n rmap

relmapGroupBy :: (Ord c, C.CRel c) => C.Intmed c -> B.TermName -> B.Map (C.Relmap c)
relmapGroupBy med n rmap = C.relmapCopy med n rmapGroup where
    rmapGroup  = rmap `B.mappend` Op.relmapGroup med n rmapLocal
    rmapLocal  = C.relmapLocalSymbol med n


-- ----------------------  join-up

consJoinUp :: (Ord c) => C.RopCons c
consJoinUp med =
  do nest <- Op.getTerms med "-term"
     Right $ relmapJoinUp med nest

relmapJoinUp :: (Ord c) => C.Intmed c -> [B.TermName] -> C.Relmap c
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

consNest :: (Ord c, C.CRel c) => C.RopCons c
consNest med =
  do (co, ns) <- Op.getTermsCo med "-term"
     to       <- Op.getTerm    med "-to"
     Right $ relmapNest med (co, ns, to)

relmapNest :: (Ord c, C.CRel c) => C.Intmed c -> (Bool, [B.TermName], B.TermName) -> C.Relmap c
relmapNest med (co, ns, to) = group `B.mappend` for where
    group  = relmapGroupBy med to key
    for    = Op.relmapFor med to nest
    key    = if co then pick else cut
    nest   = if co then cut  else pick
    pick   = Op.relmapPick med ns
    cut    = Op.relmapCut  med ns

consPickGroup :: (Ord c, C.CRel c) => C.RopCons c
consPickGroup med =
  do ns <- Op.getTerms med "-term"
     to <- Op.getTerm  med "-to"
     Right $ relmapNest med (True, ns, to)


-- ----------------------  unnest

-- $Unnest
--
--  > unnest /g
--  > slice-up ( meet ^/g ) | cut /g

consUnnest :: (Ord c, C.CRel c) => C.RopCons c
consUnnest med =
  do n <- Op.getTerm med "-term"
     Right $ relmapUnnest med n

relmapUnnest :: (Ord c, C.CRel c) => C.Intmed c -> B.TermName -> C.Relmap c
relmapUnnest med n = unnest where
    unnest  = slice `B.mappend` cut
    slice   = Op.relmapSliceUp med meet
    meet    = Op.relmapMeet med $ C.relmapLocalNest med n
    cut     = Op.relmapCut  med [n]

