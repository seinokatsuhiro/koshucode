{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Vanilla.Rop.Order
( 
  -- * number
  ropConsNumber, relmapNumber, relfyNumber,
  -- * rank
  ropConsRank,
  relmapGapRank, relfyGapRank,
  relmapDenseRank, relfyDenseRank,
) where

import qualified Koshucode.Baala.Base          as B
import qualified Koshucode.Baala.Core          as C
import qualified Koshucode.Baala.Builtin       as Rop
import qualified Koshucode.Baala.Vanilla.Type  as Rop



-- ----------------------  number

ropConsNumber :: Rop.VRopCons
ropConsNumber use =
    do n  <- Rop.getTerm use "-term"
       ns <- Rop.getOption [] Rop.getTerms use "-order"
       Right $ relmapNumber use n ns

relmapNumber :: (C.CDec c, Ord c) => C.RopUse c -> B.Termname -> [B.Termname] -> C.Relmap c
relmapNumber use n ns = C.relmapCalc use $ relfyNumber n ns

relfyNumber :: (Ord c, C.CDec c) => B.Termname -> [B.Termname] -> B.Relhead -> B.Ab (C.Relfy c)
relfyNumber = relfyRanking B.sortByNameNumbering

relfyRanking
    :: (Ord c, C.CDec c)
    => B.Ranking B.Termname c
    -> B.Termname -> [B.Termname] -> B.Relhead -> B.Ab (C.Relfy c)
relfyRanking ranking n ns h1 = Right $ C.relfy h2 (C.RelfyFull False f2) where
    h2    = B.headCons n h1
    f2 b1 = let (rank, b2) = ranking 0 ords (B.headNames h1) b1
            in zipWith (:) (map C.putDecFromInt rank) b2
    ords  = map B.Asc ns



-- ----------------------  rank

ropConsRank :: Rop.VRopCons
ropConsRank use =
    do n     <- Rop.getTerm   use "-term"
       ns    <- Rop.getTerms  use "-order"
       dense <- Rop.getSwitch use "-dense"
       let relmapRank = if dense
                        then relmapDenseRank
                        else relmapGapRank
       Right $ relmapRank use n ns

relmapDenseRank :: (C.CDec c, Ord c) =>
   C.RopUse c -> B.Termname -> [B.Termname] -> C.Relmap c
relmapDenseRank use n ns = C.relmapCalc use $ relfyDenseRank n ns

relfyDenseRank :: (Ord c, C.CDec c) =>
   B.Termname -> [B.Termname] -> B.Relhead -> B.Ab (C.Relfy c)
relfyDenseRank = relfyRanking B.sortByNameDenseRank

relmapGapRank :: (C.CDec c, Ord c) =>
   C.RopUse c -> B.Termname -> [B.Termname] -> C.Relmap c
relmapGapRank use n ns = C.relmapCalc use $ relfyGapRank n ns

relfyGapRank :: (Ord c, C.CDec c) =>
   B.Termname -> [B.Termname] -> B.Relhead -> B.Ab (C.Relfy c)
relfyGapRank = relfyRanking B.sortByNameGapRank


-- -- | Keep leading tuples.
-- limit :: (Ord c) => C.RopUse c -> Int -> String -> C.Relmap c
-- limit use c ns = C.relmapCalc use $ limit2 c ns

-- limit2 :: (Ord c) => Int -> String -> a -> B.AbMap (B.Rel c)
-- limit2 c ns _ (B.Rel h1 b1) = Right $ B.Rel h1 b2 where
--     b2   = List.take c $ sortByName ords (B.headNames h1) b1
--     ords = orders ns

