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
    --        CONSTRUCTOR    USAGE                        ATTRIBUTE
    [ Op.def  consConst      "const R"                    "1 -lit"
    , Op.def  consInterp     "interp E"                   "1 -interp | -x"
    , Op.def  consNumber     "number /N -order /N ..."    "1 -term | -order -from"
    , Op.def  consRank       "rank /N -order /N ..."      "1 -term | -order -from -dense"
    , Op.def  consRepeat     "repeat N R"                 "2 -count -relmap/"
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
consConst med =
    do lit <- Op.getContent med "-lit"
       case C.isRel lit of
         True  -> Right $ relmapConst med $ C.gRel lit
         False -> Msg.reqRel

relmapConst :: C.Intmed c -> B.Rel c -> C.Relmap c
relmapConst med = C.relmapFlow med . relkitConst

relkitConst :: B.Rel c -> C.RelkitFlow c
relkitConst _ Nothing = Right C.relkitNothing
relkitConst (B.Rel he bo) _ = Right kit2 where
    kit2 = C.relkitJust he $ C.RelkitConst bo


-- ----------------------  interp

consInterp :: (C.CContent c) => C.RopCons c
consInterp med =
    do skip <- Op.getSwitch med "-x"
       case skip of
         True  -> Right $ Op.relmapId med
         False -> consInterp2 med

consInterp2 :: (C.CContent c) => C.RopCons c
consInterp2 med =
    do c <- Op.getContent med "-interp"
       case C.isInterp c of
         True  -> Right $ relmapInterp med $ C.gInterp c
         False -> Msg.reqInterp

relmapInterp :: (C.CContent c) => C.Intmed c -> B.Interp -> C.Relmap c
relmapInterp med = C.relmapFlow med . relkitInterp

relkitInterp :: (C.CContent c) => B.Interp -> C.RelkitFlow c
relkitInterp _ Nothing = Right C.relkitNothing
relkitInterp interp (Just he1)
    | interpMatch interp he1 = Right $ C.relkitJust he1 C.RelkitId
    | otherwise              = Msg.unkTerm (B.interpTerms interp) he1

interpMatch :: B.Interp -> B.Head -> Bool
interpMatch interp he = ns1 == ns2 where
    ns1 = B.sort $ B.interpTerms interp
    ns2 = B.sort $ B.headNames he


-- ----------------------  number

consNumber :: (Ord c, C.CContent c) => C.RopCons c
consNumber med =
    do n    <- Op.getTerm med "-term"
       ns   <- Op.getOption [] Op.getTerms med "-order"
       from <- Op.getOption 0  Op.getInt   med "-from"
       Right $ relmapNumber med (n, ns, from)

relmapNumber :: (C.CDec c, Ord c) => C.Intmed c -> (B.TermName, [B.TermName], Int) -> C.Relmap c
relmapNumber med = C.relmapFlow med . relkitNumber

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
consRank med =
    do n     <- Op.getTerm   med "-term"
       ns    <- Op.getTerms  med "-order"
       from  <- Op.getOption 0 Op.getInt med "-from"
       dense <- Op.getSwitch med "-dense"
       let relmapRank = if dense
                        then relmapDenseRank
                        else relmapGapRank
       Right $ relmapRank med (n, ns, from)

relmapDenseRank :: (C.CDec c, Ord c) =>
   C.Intmed c -> (B.TermName, [B.TermName], Int) -> C.Relmap c
relmapDenseRank med = C.relmapFlow med . relkitDenseRank

relkitDenseRank :: (Ord c, C.CDec c) => (B.TermName, [B.TermName], Int) -> C.RelkitFlow c
relkitDenseRank = relkitRanking B.sortByNameDenseRank

relmapGapRank :: (C.CDec c, Ord c) =>
   C.Intmed c -> (B.TermName, [B.TermName], Int) -> C.Relmap c
relmapGapRank med = C.relmapFlow med . relkitGapRank

relkitGapRank :: (Ord c, C.CDec c) => (B.TermName, [B.TermName], Int) -> C.RelkitFlow c
relkitGapRank = relkitRanking B.sortByNameGapRank


-- ----------------------  repeat

consRepeat :: (Ord c, C.CContent c) => C.RopCons c
consRepeat med =
  do cnt  <- Op.getInt    med "-count"
     rmap <- Op.getRelmap med "-relmap"
     Right $ relmapRepeat med cnt rmap

relmapRepeat :: (Ord c) => C.Intmed c -> Int -> B.Map (C.Relmap c)
relmapRepeat med cnt = C.relmapBinary med $ relkitRepeat cnt

relkitRepeat :: forall c. (Ord c) => Int -> C.RelkitBinary c
relkitRepeat cnt (C.Relkit _ (Just he2) kitb2) (Just he1)
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
