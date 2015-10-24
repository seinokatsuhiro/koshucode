{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Koshucode.Baala.Rop.Cox.Gadget
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
import qualified Koshucode.Baala.Base               as B
import qualified Koshucode.Baala.Data               as D
import qualified Koshucode.Baala.Core               as C
import qualified Koshucode.Baala.Rop.Base           as Op
import qualified Koshucode.Baala.Rop.Cox.Get        as Op
import qualified Koshucode.Baala.Rop.Cox.GeoDatumJp as Op
import qualified Koshucode.Baala.Rop.Flat.Message        as Msg


-- | Implementation of relational operators.
ropsCoxGadget :: (D.CContent c) => [C.Rop c]
ropsCoxGadget = Op.ropList "cox-gadget"
    --       CONSTRUCTOR    USAGE                            ATTRIBUTE
    [ Op.def consConst      "const R"                        "1 -lit"
    , Op.def consGeoDatumJp "geo-datum-jp E E E -to /N /N"   "3 -n -x -y | -to"
    , Op.def consGeoDegree  "geo-degree /N /P /P /P"         "4 -real -deg -min -sec"
    , Op.def consInterp     "interp E"                       "1 -interp | -x"
    , Op.def consNumber     "number /N -order /N ..."        "1 -term | -order -from"
    , Op.def consRank       "rank /N -order /N ..."          "1 -term | -order -from -dense"
    , Op.def consRepeat     "repeat N R"                     "2 -count -relmap/"
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

consConst :: (D.CContent c) => C.RopCons c
consConst med =
    do lit <- Op.getContent med "-lit"
       case D.isRel lit of
         True  -> Right $ relmapConst med $ D.gRel lit
         False -> Msg.reqRel

relmapConst :: C.Intmed c -> D.Rel c -> C.Relmap c
relmapConst med = C.relmapFlow med . relkitConst

relkitConst :: D.Rel c -> C.RelkitFlow c
relkitConst _ Nothing = Right C.relkitNothing
relkitConst (D.Rel he bo) _ = Right kit2 where
    kit2 = C.relkitJust he $ C.RelkitConst bo


-- ----------------------  geo-datum-jp

--  geo-datum-jp /n /x /y -to /lat /long

consGeoDatumJp :: (Ord c, D.CContent c) => C.RopCons c
consGeoDatumJp med =
    do n  <- Op.getCox med "-n"
       x  <- Op.getCox med "-x"
       y  <- Op.getCox med "-y"
       (lat, long) <- Op.getTerm2 med "-to"
       let cops = C.globalCopset $ C.ropGlobal med
       Right $ relmapGeoDatumJp med (cops, (n,x,y), (lat,long))

relmapGeoDatumJp :: (Ord c, D.CContent c) => C.Intmed c -> (D.CopSet c, D.Cox3 c, D.TermName2) -> C.Relmap c
relmapGeoDatumJp med = C.relmapFlow med . relkitGeoDatumJp

relkitGeoDatumJp :: (Ord c, D.CContent c) => (D.CopSet c, D.Cox3 c, D.TermName2) -> C.RelkitFlow c
relkitGeoDatumJp _ Nothing = Right C.relkitNothing
relkitGeoDatumJp (cops, (coxn,coxx,coxy), (lat,long)) (Just he1) = Right kit2 where
    he2       = D.headAppend [lat, long] he1
    kit2      = C.relkitJust he2 $ C.RelkitOneToAbOne False f2 []
    pReal     = D.pDec . D.decimalFromRealFloat 4

    f2 _ cs   = do cn    <- D.coxRunCox cops he1 cs coxn
                   cx    <- D.coxRunCox cops he1 cs coxx
                   cy    <- D.coxRunCox cops he1 cs coxy

                   decn  <- D.getDec $ Right cn
                   decx  <- D.getDec $ Right cx
                   decy  <- D.getDec $ Right cy

                   let n  = fromInteger $ D.decimalNum decn
                       dx = D.decimalToRealFloat decx :: Double
                       dy = D.decimalToRealFloat decy :: Double
                       (dlat, dlong) = Op.convDegree n (dx, dy)

                   Right $ pReal dlat : pReal dlong : cs


-- ----------------------  geo-degree

--  geo-degree /deg-real /deg /min /sec

consGeoDegree :: (Ord c, D.CContent c) => C.RopCons c
consGeoDegree med =
    do real <- Op.getTerm med "-real"
       deg  <- Op.getTerm med "-deg"
       mnt  <- Op.getTerm med "-min"
       sec  <- Op.getTerm med "-sec"
       Right $ relmapGeoDegree med (real, deg, mnt, sec)

relmapGeoDegree :: (Ord c, D.CContent c) => C.Intmed c -> D.TermName4 -> C.Relmap c
relmapGeoDegree med = C.relmapFlow med . relkitGeoDegree

relkitGeoDegree :: (Ord c, D.CContent c) => D.TermName4 -> C.RelkitFlow c
relkitGeoDegree _ Nothing = Right C.relkitNothing
relkitGeoDegree (real, deg, mnt, sec) (Just he1) = Right kit2 where
    he2       = D.headCons real he1
    kit2      = C.relkitJust he2 $ C.RelkitOneToAbOne False f2 []
    pick      = Op.picker he1 [deg, mnt, sec]
    pReal     = D.pDec . D.decimalFromRealFloat 4

    f2 _ cs   = do let [cdeg, cmnt, csec] = pick cs

                   hdeg <- D.getDec $ Right cdeg
                   hmnt <- D.getDec $ Right cmnt
                   hsec <- D.getDec $ Right csec

                   let ddeg = D.decimalToRealFloat hdeg :: Double
                       dmnt = D.decimalToRealFloat hmnt :: Double
                       dsec = D.decimalToRealFloat hsec :: Double
                       dmnt' = dmnt + dsec  / 60
                       ddeg' = ddeg + dmnt' / 60

                   Right $ pReal ddeg' : cs


-- ----------------------  interp

consInterp :: (D.CContent c) => C.RopCons c
consInterp med =
    do skip <- Op.getSwitch med "-x"
       case skip of
         True  -> Right $ Op.relmapId med
         False -> consInterp2 med

consInterp2 :: (D.CContent c) => C.RopCons c
consInterp2 med =
    do c <- Op.getContent med "-interp"
       case D.isInterp c of
         True  -> Right $ relmapInterp med $ D.gInterp c
         False -> Msg.reqInterp

relmapInterp :: (D.CContent c) => C.Intmed c -> D.Interp -> C.Relmap c
relmapInterp med = C.relmapFlow med . relkitInterp

relkitInterp :: (D.CContent c) => D.Interp -> C.RelkitFlow c
relkitInterp _ Nothing = Right C.relkitNothing
relkitInterp interp (Just he1)
    | interpMatch interp he1 = Right $ C.relkitJust he1 C.RelkitId
    | otherwise              = Msg.unkTerm (D.interpTerms interp) he1

interpMatch :: D.Interp -> D.Head -> Bool
interpMatch interp he = ns1 == ns2 where
    ns1 = B.sort $ D.interpTerms interp
    ns2 = B.sort $ D.headNames he


-- ----------------------  number

consNumber :: (Ord c, D.CContent c) => C.RopCons c
consNumber med =
    do n    <- Op.getTerm med "-term"
       ns   <- Op.getOption [] Op.getTerms med "-order"
       from <- Op.getOption 0  Op.getInt   med "-from"
       Right $ relmapNumber med (n, ns, fromInteger from)

relmapNumber :: (D.CDec c, Ord c) => C.Intmed c -> (D.TermName, [D.TermName], Int) -> C.Relmap c
relmapNumber med = C.relmapFlow med . relkitNumber

relkitNumber :: (Ord c, D.CDec c) => (D.TermName, [D.TermName], Int) -> C.RelkitFlow c
relkitNumber = relkitRanking B.sortByNameNumbering

relkitRanking
    :: (Ord c, D.CDec c)
    => B.Ranking D.TermName c
    -> (D.TermName, [D.TermName], Int) -> C.RelkitFlow c
relkitRanking _ _ Nothing = Right C.relkitNothing
relkitRanking ranking (n, ns, from) (Just he1) = Right kit2 where
    he2   = D.headCons n he1
    kit2  = C.relkitJust he2 $ C.RelkitFull False kitf2
    kitf2 bo1 = let (rank, bo2) = ranking from ords (D.headNames he1) bo1
                in zipWith (:) (map D.pInt rank) bo2
    ords  = map B.Asc ns


-- ----------------------  rank

consRank :: (Ord c, D.CContent c) => C.RopCons c
consRank med =
    do n     <- Op.getTerm   med "-term"
       ns    <- Op.getTerms  med "-order"
       from  <- Op.getOption 0 Op.getInt med "-from"
       dense <- Op.getSwitch med "-dense"
       let relmapRank = if dense
                        then relmapDenseRank
                        else relmapGapRank
       Right $ relmapRank med (n, ns, fromInteger from)

relmapDenseRank :: (D.CDec c, Ord c) =>
   C.Intmed c -> (D.TermName, [D.TermName], Int) -> C.Relmap c
relmapDenseRank med = C.relmapFlow med . relkitDenseRank

relkitDenseRank :: (Ord c, D.CDec c) => (D.TermName, [D.TermName], Int) -> C.RelkitFlow c
relkitDenseRank = relkitRanking B.sortByNameDenseRank

relmapGapRank :: (D.CDec c, Ord c) =>
   C.Intmed c -> (D.TermName, [D.TermName], Int) -> C.Relmap c
relmapGapRank med = C.relmapFlow med . relkitGapRank

relkitGapRank :: (Ord c, D.CDec c) => (D.TermName, [D.TermName], Int) -> C.RelkitFlow c
relkitGapRank = relkitRanking B.sortByNameGapRank


-- ----------------------  repeat

consRepeat :: (Ord c, D.CContent c) => C.RopCons c
consRepeat med =
  do cnt  <- Op.getInt    med "-count"
     rmap <- Op.getRelmap med "-relmap"
     Right $ relmapRepeat med cnt rmap

relmapRepeat :: (Ord c) => C.Intmed c -> Integer -> B.Map (C.Relmap c)
relmapRepeat med cnt = C.relmapBinary med $ relkitRepeat cnt

relkitRepeat :: forall c. (Ord c) => Integer -> C.RelkitBinary c
relkitRepeat cnt (C.Relkit _ (Just he2) kitb2) (Just he1)
    | D.headEquiv he1 he2 = Right $ kit3
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
