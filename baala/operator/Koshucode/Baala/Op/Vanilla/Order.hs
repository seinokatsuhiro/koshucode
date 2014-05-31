{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Vanilla.Order
( 
  -- * number
  consNumber, relmapNumber, relkitNumber,
  -- * rank
  consRank,
  relmapGapRank, relkitGapRank,
  relmapDenseRank, relkitDenseRank,

  -- * chunk
  consChunk, relmapChunk, relkitChunk,
) where

import qualified Koshucode.Baala.Base         as B
import qualified Koshucode.Baala.Core         as C
import qualified Koshucode.Baala.Op.Builtin   as Op



-- ----------------------  number

consNumber :: (Ord c, C.CDec c) => C.RopCons c
consNumber use =
    do n    <- Op.getTerm use "-term"
       ns   <- Op.getOption [] Op.getTerms use "-order"
       from <- Op.getOption 0  Op.getInt   use "-from"
       Right $ relmapNumber use (n, ns, from)

relmapNumber :: (C.CDec c, Ord c) => C.RopUse c -> (B.TermName, [B.TermName], Int) -> C.Relmap c
relmapNumber use = C.relmapFlow use . relkitNumber

relkitNumber :: (Ord c, C.CDec c) => (B.TermName, [B.TermName], Int) -> C.RelkitCalc c
relkitNumber = relkitRanking B.sortByNameNumbering

relkitRanking
    :: (Ord c, C.CDec c)
    => B.Ranking B.TermName c
    -> (B.TermName, [B.TermName], Int) -> C.RelkitCalc c
relkitRanking _ _ Nothing = Right C.relkitNothing
relkitRanking ranking (n, ns, from) (Just he1) = Right kit2 where
    he2   = B.headCons n he1
    kit2  = C.relkitJust he2 $ C.RelkitFull False kitf2
    kitf2 bo1 = let (rank, bo2) = ranking from ords (B.headNames he1) bo1
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
       Right $ relmapRank use (n, ns, 0)

relmapDenseRank :: (C.CDec c, Ord c) =>
   C.RopUse c -> (B.TermName, [B.TermName], Int) -> C.Relmap c
relmapDenseRank use = C.relmapFlow use . relkitDenseRank

relkitDenseRank :: (Ord c, C.CDec c) => (B.TermName, [B.TermName], Int) -> C.RelkitCalc c
relkitDenseRank = relkitRanking B.sortByNameDenseRank

relmapGapRank :: (C.CDec c, Ord c) =>
   C.RopUse c -> (B.TermName, [B.TermName], Int) -> C.Relmap c
relmapGapRank use = C.relmapFlow use . relkitGapRank

relkitGapRank :: (Ord c, C.CDec c) => (B.TermName, [B.TermName], Int) -> C.RelkitCalc c
relkitGapRank = relkitRanking B.sortByNameGapRank



-- ----------------------  chunk

--  > chunk /a /b /c

consChunk :: (Ord c, C.CRel c) => C.RopCons c
consChunk use =
  do ns  <- Op.getTerms use "-term"
     ord <- Op.getOption [] Op.getTerms use "-order"
     Right $ relmapChunk use ns ord

relmapChunk :: (Ord c, C.CRel c) => C.RopUse c -> [B.TermName] -> [B.TermName] -> C.Relmap c
relmapChunk use ns ord = C.relmapFlow use $ relkitChunk ns ord

relkitChunk :: (Ord c, C.CRel c) => [B.TermName] -> [B.TermName] -> C.RelkitCalc c
relkitChunk _ _ Nothing = Right C.relkitNothing
relkitChunk ns ord (Just he1) = Right kit2 where
    he2     = B.Relhead $ map nest ns
    nest n  = B.TermNest n $ B.headTerms he1
    kit2    = C.relkitJust he2 $ C.RelkitFull False f2
    f2 bo1  = let deg    = length bo1 `B.ceilingRem` length ns
                  bo1'   = B.sortByName (map B.Asc ord) ns bo1
                  ch     = B.chunks deg bo1'
                  rels   = (C.pRel . B.Rel he1) `map` ch
              in [rels]
