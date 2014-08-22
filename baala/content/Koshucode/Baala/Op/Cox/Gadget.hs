{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Koshucode.Baala.Op.Cox.Gadget
( ropsCoxGadget,

  -- * number
  consNumber, relmapNumber, relkitNumber,

  -- * rank
  consRank,
  relmapGapRank, relkitGapRank,
  relmapDenseRank, relkitDenseRank,
) where

import Prelude hiding (getContents)
import qualified Koshucode.Baala.Base        as B
import qualified Koshucode.Baala.Core        as C
import qualified Koshucode.Baala.Op.Builtin  as Op
import qualified Koshucode.Baala.Op.Cox.Get  as Op


-- | Implementation of relational operators.
ropsCoxGadget :: (C.CContent c) => [C.Rop c]
ropsCoxGadget = Op.ropList "cox-gadget"
    --          CONSTRUCTOR         USAGE         ATTRIBUTE
    [ Op.ropI consNumber    "number /N -order /N ..."  "-term | -order -from"
    , Op.ropI consRank      "rank /N -order /N ..."    "-term | -order -dense"
    ]


-- ----------------------  number

consNumber :: (Ord c, C.CContent c) => C.RopCons c
consNumber use =
    do n    <- Op.getTerm use "-term"
       ns   <- Op.getOption [] Op.getTerms use "-order"
       from <- Op.getOption 0  Op.getInt2 use "-from"
       Right $ relmapNumber use (n, ns, from)

relmapNumber :: (C.CDec c, Ord c) => C.RopUse c -> (B.TermName, [B.TermName], Int) -> C.Relmap c
relmapNumber use = C.relmapFlow use . relkitNumber

relkitNumber :: (Ord c, C.CDec c) => (B.TermName, [B.TermName], Int) -> C.RelkitFlow c
relkitNumber = relkitRanking B.sortByNameNumbering

relkitRanking
    :: (Ord c, C.CDec c)
    => B.Ranking B.TermName c
    -> (B.TermName, [B.TermName], Int) -> C.RelkitFlow c
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

relkitDenseRank :: (Ord c, C.CDec c) => (B.TermName, [B.TermName], Int) -> C.RelkitFlow c
relkitDenseRank = relkitRanking B.sortByNameDenseRank

relmapGapRank :: (C.CDec c, Ord c) =>
   C.RopUse c -> (B.TermName, [B.TermName], Int) -> C.Relmap c
relmapGapRank use = C.relmapFlow use . relkitGapRank

relkitGapRank :: (Ord c, C.CDec c) => (B.TermName, [B.TermName], Int) -> C.RelkitFlow c
relkitGapRank = relkitRanking B.sortByNameGapRank
