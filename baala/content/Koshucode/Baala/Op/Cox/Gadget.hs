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

  -- * repeat
  consRepeat,
) where

import Prelude hiding (getContents)
import qualified Koshucode.Baala.Base        as B
import qualified Koshucode.Baala.Core        as C
import qualified Koshucode.Baala.Op.Builtin  as Op
import qualified Koshucode.Baala.Op.Cox.Get  as Op
import qualified Koshucode.Baala.Op.Message  as Msg


-- | Implementation of relational operators.
ropsCoxGadget :: (C.CContent c) => [C.Rop c]
ropsCoxGadget = Op.ropList "cox-gadget"
    --         CONSTRUCTOR   USAGE                      ATTRIBUTE
    [ Op.ropI  consConst     "const R"                  "-lit"
    , Op.ropI  consNumber    "number /N -order /N ..."  "-term | -order -from"
    , Op.ropI  consRank      "rank /N -order /N ..."    "-term | -order -from -dense"
    , Op.ropII consRepeat    "repeat N R"               "-count -relmap/"
    ]


-- ----------------------  const

-- $const
--
--  Same as relmap @dee@
--  
--    > const {| | |}
--
--  Same as relmap @dum@
--  
--    > const {| |}

consConst :: (C.CContent c) => C.RopCons c
consConst use =
    do lit <- Op.getContent use "-lit"
       case C.isRel lit of
         True  -> Right $ relmapConst use $ C.gRel lit
         False -> Msg.reqRel

relmapConst :: C.RopUse c -> B.Rel c -> C.Relmap c
relmapConst use = C.relmapFlow use . relkitConst

relkitConst :: B.Rel c -> C.RelkitFlow c
relkitConst _ Nothing = Right C.relkitNothing
relkitConst (B.Rel he bo) _ = Right kit2 where
    kit2 = C.relkitJust he $ C.RelkitConst bo


-- ----------------------  number

consNumber :: (Ord c, C.CContent c) => C.RopCons c
consNumber use =
    do n    <- Op.getTerm use "-term"
       ns   <- Op.getOption [] Op.getTerms use "-order"
       from <- Op.getOption 0  Op.getInt   use "-from"
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

consRank :: (Ord c, C.CContent c) => C.RopCons c
consRank use =
    do n     <- Op.getTerm   use "-term"
       ns    <- Op.getTerms  use "-order"
       from  <- Op.getOption 0 Op.getInt use "-from"
       dense <- Op.getSwitch use "-dense"
       let relmapRank = if dense
                        then relmapDenseRank
                        else relmapGapRank
       Right $ relmapRank use (n, ns, from)

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


-- ----------------------  repeat

consRepeat :: (Ord c, C.CContent c) => C.RopCons c
consRepeat use =
  do cnt  <- Op.getInt    use "-count"
     rmap <- Op.getRelmap use "-relmap"
     Right $ relmapRepeat use cnt rmap

relmapRepeat :: (Ord c) => C.RopUse c -> Int -> B.Map (C.Relmap c)
relmapRepeat use cnt = C.relmapBinary use $ relkitRepeat cnt

relkitRepeat :: forall c. (Ord c) => Int -> C.RelkitBinary c
relkitRepeat cnt (C.Relkit (Just he2) kitb2) (Just he1)
    | B.headEquiv he1 he2 = Right $ kit3
    | otherwise = Msg.diffHead [he1, he2]
    where
    kit3 = C.relkitJust he1 $ C.RelkitAbFull True kitf3 [kitb2]
    kitf3 bmaps bo1 =
        do let [bmap2] = bmaps
               bmap2'  = C.bmapAlign he2 he1 bmap2
           bo2 <- rep bmap2' cnt bo1
           Right bo2

    rep bmap2' = loop where
        loop c bo | c > 0     = loop (c - 1) =<< bmap2' bo
                  | otherwise = Right bo

relkitRepeat _ _ _ = Right C.relkitNothing
