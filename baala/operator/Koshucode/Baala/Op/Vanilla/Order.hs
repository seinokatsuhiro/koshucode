{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Vanilla.Order
( 
  -- * number
  consNumber, relmapNumber, relkitNumber,
  -- * rank
  consRank,
  relmapGapRank, relkitGapRank,
  relmapDenseRank, relkitDenseRank,
) where

import qualified Koshucode.Baala.Base         as B
import qualified Koshucode.Baala.Core         as C
import qualified Koshucode.Baala.Op.Builtin   as Op



-- ----------------------  number

consNumber :: (Ord c, C.CDec c) => C.RopCons c
consNumber use =
    do n  <- Op.getTerm use "-term"
       ns <- Op.getOption [] Op.getTerms use "-order"
       Right $ relmapNumber use (n, ns)

relmapNumber :: (C.CDec c, Ord c) => C.RopUse c -> (B.Termname, [B.Termname]) -> C.Relmap c
relmapNumber use = C.relmapFlow use . relkitNumber

relkitNumber :: (Ord c, C.CDec c) => (B.Termname, [B.Termname]) -> C.RelkitCalc c
relkitNumber = relkitRanking B.sortByNameNumbering

relkitRanking
    :: (Ord c, C.CDec c)
    => B.Ranking B.Termname c
    -> (B.Termname, [B.Termname]) -> C.RelkitCalc c
relkitRanking _ _ Nothing = Right C.relkitNothing
relkitRanking ranking (n, ns) (Just he1) = Right kit2 where
    he2   = B.headCons n he1
    kit2  = C.relkitJust he2 $ C.RelkitFull False kitf2
    kitf2 bo1 = let (rank, bo2) = ranking 0 ords (B.headNames he1) bo1
                in zipWith (:) (map C.pDecFromInt rank) bo2
    ords  = map B.Asc ns



-- ----------------------  rank

consRank :: (Ord c, C.CDec c) => C.RopCons c
consRank use =
    do n     <- Op.getTerm   use "-term"
       ns    <- Op.getTerms  use "-order"
       dense <- Op.getSwitch use "-dense"
       let relmapRank = if dense
                        then relmapDenseRank
                        else relmapGapRank
       Right $ relmapRank use (n, ns)

relmapDenseRank :: (C.CDec c, Ord c) =>
   C.RopUse c -> (B.Termname, [B.Termname]) -> C.Relmap c
relmapDenseRank use = C.relmapFlow use . relkitDenseRank

relkitDenseRank :: (Ord c, C.CDec c) => (B.Termname, [B.Termname]) -> C.RelkitCalc c
relkitDenseRank = relkitRanking B.sortByNameDenseRank

relmapGapRank :: (C.CDec c, Ord c) =>
   C.RopUse c -> (B.Termname, [B.Termname]) -> C.Relmap c
relmapGapRank use (n, ns) = C.relmapFlow use $ relkitGapRank (n, ns)

relkitGapRank :: (Ord c, C.CDec c) => (B.Termname, [B.Termname]) -> C.RelkitCalc c
relkitGapRank = relkitRanking B.sortByNameGapRank

