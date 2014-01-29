{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Vanilla.Rop.Order
( 
  -- * number
  consNumber, relmapNumber, relkitNumber,
  -- * rank
  consRank,
  relmapGapRank, relkitGapRank,
  relmapDenseRank, relkitDenseRank,
) where

import qualified Koshucode.Baala.Base          as B
import qualified Koshucode.Baala.Core          as C
import qualified Koshucode.Baala.Builtin       as Rop



-- ----------------------  number

consNumber :: (Ord c, C.CDec c) => C.RopCons c
consNumber use =
    do n  <- Rop.getTerm use "-term"
       ns <- Rop.getOption [] Rop.getTerms use "-order"
       Right $ relmapNumber use n ns

relmapNumber :: (C.CDec c, Ord c) => C.RopUse c -> B.Termname -> [B.Termname] -> C.Relmap c
relmapNumber use n ns = C.relmapCalc use $ relkitNumber n ns

relkitNumber :: (Ord c, C.CDec c) => B.Termname -> [B.Termname] -> B.Relhead -> B.Ab (C.Relkit c)
relkitNumber = relkitRanking B.sortByNameNumbering

relkitRanking
    :: (Ord c, C.CDec c)
    => B.Ranking B.Termname c
    -> B.Termname -> [B.Termname] -> B.Relhead -> B.Ab (C.Relkit c)
relkitRanking ranking n ns h1 = Right $ C.relkit h2 (C.RelkitFull False f2) where
    h2    = B.headCons n h1
    f2 b1 = let (rank, b2) = ranking 0 ords (B.headNames h1) b1
            in zipWith (:) (map C.putDecFromInt rank) b2
    ords  = map B.Asc ns



-- ----------------------  rank

consRank :: (Ord c, C.CDec c) => C.RopCons c
consRank use =
    do n     <- Rop.getTerm   use "-term"
       ns    <- Rop.getTerms  use "-order"
       dense <- Rop.getSwitch use "-dense"
       let relmapRank = if dense
                        then relmapDenseRank
                        else relmapGapRank
       Right $ relmapRank use n ns

relmapDenseRank :: (C.CDec c, Ord c) =>
   C.RopUse c -> B.Termname -> [B.Termname] -> C.Relmap c
relmapDenseRank use n ns = C.relmapCalc use $ relkitDenseRank n ns

relkitDenseRank :: (Ord c, C.CDec c) =>
   B.Termname -> [B.Termname] -> B.Relhead -> B.Ab (C.Relkit c)
relkitDenseRank = relkitRanking B.sortByNameDenseRank

relmapGapRank :: (C.CDec c, Ord c) =>
   C.RopUse c -> B.Termname -> [B.Termname] -> C.Relmap c
relmapGapRank use n ns = C.relmapCalc use $ relkitGapRank n ns

relkitGapRank :: (Ord c, C.CDec c) =>
   B.Termname -> [B.Termname] -> B.Relhead -> B.Ab (C.Relkit c)
relkitGapRank = relkitRanking B.sortByNameGapRank


-- -- | Keep leading tuples.
-- limit :: (Ord c) => C.RopUse c -> Int -> String -> C.Relmap c
-- limit use c ns = C.relmapCalc use $ limit2 c ns

-- limit2 :: (Ord c) => Int -> String -> a -> B.AbMap (B.Rel c)
-- limit2 c ns _ (B.Rel h1 b1) = Right $ B.Rel h1 b2 where
--     b2   = List.take c $ sortByName ords (B.headNames h1) b1
--     ords = orders ns

