{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Gadgets with content calculation.

module Koshucode.Baala.Rop.Cox.Gadget
  ( ropsCoxGadget,
  
    -- * number
    consConst, relmapConst, relkitConst,

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
import qualified Koshucode.Baala.Overture           as O
import qualified Koshucode.Baala.Base               as B
import qualified Koshucode.Baala.Syntax             as S
import qualified Koshucode.Baala.Data               as D
import qualified Koshucode.Baala.Core               as C
import qualified Koshucode.Baala.Rop.Base           as Op
import qualified Koshucode.Baala.Rop.Cox.Get        as Op
import qualified Koshucode.Baala.Rop.Cox.GeoDatumJp as Op
import qualified Koshucode.Baala.Rop.Cox.Message    as Msg


-- | Implementation of relational operators.
ropsCoxGadget :: (D.CContent c) => [C.Rop c]
ropsCoxGadget = Op.ropList "cox-gadget"
    --       CONSTRUCTOR    USAGE                            ATTRIBUTE
    [ Op.def consConst      "const R"                        "-lit"
    , Op.def consGeoDatumJp "geo-datum-jp E E E -to /N /N"   "-n -x -y . -to"
    , Op.def consGeoDegree  "geo-degree /N /P /P /P"         "-real -deg -min -sec"
    , Op.def consInterp     "interp E"                       "-interp . -x?"
    , Op.def consNumber     "number /N -order /P ..."        "-term . -order? -from?"
    , Op.def consRank       "rank /N -order /P ..."          "-term . -order? -from? -dense?"
    , Op.def consRepeat     "repeat I R"                     "-count -relmap/"
    ]


-- ----------------------  const

-- | __const E__
--
--   Output the constant relation E.
--   Especially, @const {= [] =}@ is equivalent to @dee@,
--   @const {= =}@ is equivalent to @dum@.
--
consConst :: (D.CContent c) => C.RopCons c
consConst med =
    do lit <- Op.getContent med "-lit"
       case D.isRel lit of
         True  -> Right $ relmapConst med $ D.gRel lit
         False -> Msg.reqRel

-- | Create @const@ relmap.
relmapConst :: C.Intmed c -> D.Rel c -> C.Relmap c
relmapConst med = C.relmapFlow med . relkitConst

-- | Create @const@ relkit.
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

-- | Create @geo-datum-jp@ relmap.
relmapGeoDatumJp :: (Ord c, D.CContent c) => C.Intmed c -> (D.CopSet c, D.Cox3 c, S.TermName2) -> C.Relmap c
relmapGeoDatumJp med = C.relmapFlow med . relkitGeoDatumJp

-- | Create @geo-datum-jp@ relkit.
relkitGeoDatumJp :: (Ord c, D.CContent c) => (D.CopSet c, D.Cox3 c, S.TermName2) -> C.RelkitFlow c
relkitGeoDatumJp _ Nothing = Right C.relkitNothing
relkitGeoDatumJp (cops, (coxn,coxx,coxy), (lat,long)) (Just he1) = Right kit2 where
    he2       = D.headAppend [lat, long] he1
    kit2      = C.relkitJust he2 $ C.RelkitOneToAbOne False f2 []
    pReal     = D.pDec . D.realDecimal 4

    f2 _ cs   = do cn    <- D.coxRunCox cops he1 cs coxn
                   cx    <- D.coxRunCox cops he1 cs coxx
                   cy    <- D.coxRunCox cops he1 cs coxy

                   decn  <- D.getDec $ Right cn
                   decx  <- D.getDec $ Right cx
                   decy  <- D.getDec $ Right cy

                   let n  = fromInteger $ D.decimalNum decn
                       dx = D.decimalFractional decx :: Double
                       dy = D.decimalFractional decy :: Double
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

-- | Create @geo-degree@ relmap.
relmapGeoDegree :: (Ord c, D.CContent c) => C.Intmed c -> S.TermName4 -> C.Relmap c
relmapGeoDegree med = C.relmapFlow med . relkitGeoDegree

-- | Create @geo-degree@ relkit.
relkitGeoDegree :: (Ord c, D.CContent c) => S.TermName4 -> C.RelkitFlow c
relkitGeoDegree _ Nothing = Right C.relkitNothing
relkitGeoDegree (real, deg, mnt, sec) (Just he1) = Right kit2 where
    he2       = D.headCons real he1
    kit2      = C.relkitJust he2 $ C.RelkitOneToAbOne False f2 []
    pick      = D.picker [deg, mnt, sec] he1
    pReal     = D.pDec . D.realDecimal 4

    f2 _ cs   = do let [cdeg, cmnt, csec] = pick cs

                   hdeg <- D.getDec $ Right cdeg
                   hmnt <- D.getDec $ Right cmnt
                   hsec <- D.getDec $ Right csec

                   let ddeg = D.decimalFractional hdeg :: Double
                       dmnt = D.decimalFractional hmnt :: Double
                       dsec = D.decimalFractional hsec :: Double
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

-- | Create @interp@ relmap.
relmapInterp :: (D.CContent c) => C.Intmed c -> D.Interp -> C.Relmap c
relmapInterp med = C.relmapFlow med . relkitInterp

-- | Create @interp@ relkit.
relkitInterp :: (D.CContent c) => D.Interp -> C.RelkitFlow c
relkitInterp _ Nothing = Right C.relkitNothing
relkitInterp interp (Just he1)
    | interpMatch interp he1 = Right $ C.relkitJust he1 C.RelkitId
    | otherwise              = Msg.unkTerm (D.interpTerms interp) he1

interpMatch :: D.Interp -> D.Head -> Bool
interpMatch interp he = ns1 == ns2 where
    ns1 = B.sort $ D.interpTerms interp
    ns2 = B.sort $ D.getTermNames he


-- ----------------------  number

-- | __number \/N -from I -order \/P ...__
consNumber :: (Ord c, D.CContent c) => C.RopCons c
consNumber med =
    do n    <- Op.getTerm                        med "-term"
       ns   <- Op.getOption [] Op.getSignedTerms med "-order"
       from <- Op.getOption 0  Op.getInt         med "-from"
       Right $ relmapNumber med (n, ns, fromInteger from)

-- | Create @number@ relmap.
relmapNumber :: (D.CDec c, Ord c) => C.Intmed c -> (S.TermName, [S.SignedTermName], Int) -> C.Relmap c
relmapNumber med = C.relmapFlow med . relkitNumber

-- | Create @number@ relkit.
relkitNumber :: (Ord c, D.CDec c) => (S.TermName, [S.SignedTermName], Int) -> C.RelkitFlow c
relkitNumber = relkitRanking B.sortByNameNumbering

relkitRanking
    :: (Ord c, D.CDec c)
    => B.Ranking S.TermName c
    -> (S.TermName, [S.SignedTermName], Int) -> C.RelkitFlow c
relkitRanking _ _ Nothing = Right C.relkitNothing
relkitRanking ranking (n, ns, from) (Just he1) = Right kit2 where
    he2   = D.headCons n he1
    kit2  = C.relkitJust he2 $ C.RelkitFull False kitf2
    kitf2 bo1 = let (rank, bo2) = ranking from ords (D.getTermNames he1) bo1
                in zipWith (:) (map D.pInt rank) bo2
    ords  = map B.orderingCap ns


-- ----------------------  rank

-- | [rank \/N -from I -order \/P ...]
--     Calculate standard competition ranking (like 1224).
--
--   [rank \/N -dense -from I -order \/P...]
--     Calculate dense ranking (like 1223).
--
consRank :: (Ord c, D.CContent c) => C.RopCons c
consRank med =
    do n     <- Op.getTerm               med "-term"
       ns    <- Op.getSignedTerms        med "-order"
       from  <- Op.getOption 0 Op.getInt med "-from"
       dense <- Op.getSwitch             med "-dense"
       let relmapRank = if dense
                        then relmapDenseRank
                        else relmapGapRank
       Right $ relmapRank med (n, ns, fromInteger from)

-- | Create @rank -dense@ relmap.
relmapDenseRank :: (D.CDec c, Ord c) =>
   C.Intmed c -> (S.TermName, [S.SignedTermName], Int) -> C.Relmap c
relmapDenseRank med = C.relmapFlow med . relkitDenseRank

-- | Create @rank -dense@ relkit.
relkitDenseRank :: (Ord c, D.CDec c) => (S.TermName, [S.SignedTermName], Int) -> C.RelkitFlow c
relkitDenseRank = relkitRanking B.sortByNameDenseRank

-- | Create @rank@ relmap.
relmapGapRank :: (D.CDec c, Ord c) =>
   C.Intmed c -> (S.TermName, [S.SignedTermName], Int) -> C.Relmap c
relmapGapRank med = C.relmapFlow med . relkitGapRank

-- | Create @rank@ relkit.
relkitGapRank :: (Ord c, D.CDec c) => (S.TermName, [S.SignedTermName], Int) -> C.RelkitFlow c
relkitGapRank = relkitRanking B.sortByNameGapRank


-- ----------------------  repeat

-- | __repeat I R__
consRepeat :: (Ord c, D.CContent c) => C.RopCons c
consRepeat med =
  do cnt  <- Op.getInt    med "-count"
     rmap <- Op.getRelmap med "-relmap"
     Right $ relmapRepeat med cnt rmap

-- | Create @repeat@ relmap.
relmapRepeat :: (Ord c) => C.Intmed c -> Integer -> O.Map (C.Relmap c)
relmapRepeat med cnt = C.relmapBinary med $ relkitRepeat cnt

-- | Create @repeat@ relkit.
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
