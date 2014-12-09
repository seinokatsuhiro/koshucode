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
  
    -- * nest / hang
    consHang, consNest, relmapNest,
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
consGroupBy use =
  do n    <- Op.getTerm   use "-term"
     rmap <- Op.getRelmap use "-relmap"
     Right $ relmapGroupBy use n rmap

relmapGroupBy :: (Ord c, C.CRel c) => C.RopUse c -> B.TermName -> B.Map (C.Relmap c)
relmapGroupBy use n rmap = C.relmapCopy use n rmapGroup where
    rmapGroup = rmap `B.mappend` Op.relmapGroup use n rmapCopy
    rmapCopy  = C.relmapNestVar use n


-- ----------------------  join-up

consJoinUp :: (Ord c) => C.RopCons c
consJoinUp use =
  do nest <- Op.getNestTerms use "-term"
     Right $ relmapJoinUp use nest

relmapJoinUp :: (Ord c) => C.RopUse c -> [B.Terminal String] -> C.Relmap c
relmapJoinUp use nest = C.relmapNest use nest $ Op.relmapJoinList use rmaps where
    rmaps = link `map` map snd nest
    link v = C.relmapLink use v []


-- ----------------------  nest / hang

-- $Nest
--
--  Make nested relation @\/g@ that has term @\/y@ and @\/z@.
--
--    > nest /y /z -to /g
--
--  Hang nested relation @\/g@ on term @\/x@.
--
--    > hang /g -on /x

consNest :: (Ord c, C.CRel c) => C.RopCons c
consNest use =
  do (co, ns) <- Op.getTermsCo use "-term"
     to       <- Op.getTerm    use "-to"
     Right $ relmapNest use (co, ns, to)

relmapNest :: (Ord c, C.CRel c) => C.RopUse c -> (Bool, [B.TermName], B.TermName) -> C.Relmap c
relmapNest use (co, ns, to) = group `B.mappend` for where
    group  =  relmapGroupBy use to key
    for    =  Op.relmapFor use [] to nest
    key    =  if co then pick else cut
    nest   =  if co then cut  else pick
    pick   =  Op.relmapPick use ns
    cut    =  Op.relmapCut  use ns

consHang :: (Ord c, C.CRel c) => C.RopCons c
consHang use =
  do ns <- Op.getTerms use "-on"
     to <- Op.getTerm  use "-term"
     Right $ relmapNest use (True, ns, to)


-- ----------------------  unnest

-- $Unnest
--
--  > unnest /g
--  > slice-up ( meet g ) -< /g | cut /g

consUnnest :: (Ord c, C.CRel c) => C.RopCons c
consUnnest use =
  do n <- Op.getTerm use "-term"
     Right $ relmapUnnest use n

relmapUnnest :: (Ord c, C.CRel c) => C.RopUse c -> B.TermName -> C.Relmap c
relmapUnnest use n = unnest where
    unnest  =  slice `B.mappend` cut
    slice   =  Op.relmapSliceUp use [(n, n)] meet
    meet    =  Op.relmapMeet use $ C.relmapNestVar use n
    cut     =  Op.relmapCut  use [n]

