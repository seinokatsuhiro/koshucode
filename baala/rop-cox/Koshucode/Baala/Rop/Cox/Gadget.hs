{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Gadgets with content calculation.

module Koshucode.Baala.Rop.Cox.Gadget
  ( ropsCoxGadget,
  
    -- * const
    consConst, relmapConst, relkitConst,

    -- * number
    consNumber, relmapNumber, relkitNumber,
  
    -- * rank
    consRank,
    relmapGapRank, relkitGapRank,
    relmapDenseRank, relkitDenseRank,
  
    -- * repeat
    consRepeat,
  
    -- * array
    consArray, relmapArray, relkitArray,
    -- * unarray
    consUnarray, relmapUnarray, relkitUnarray,
  ) where

import Prelude hiding (getContents)
import qualified Koshucode.Baala.DataPlus           as K
import qualified Koshucode.Baala.Core               as C
import qualified Koshucode.Baala.Rop.Base           as Rop
import qualified Koshucode.Baala.Rop.Cox.GeoDatumJp as Rop
import qualified Koshucode.Baala.Rop.Base.Message   as Msg


-- | Implementation of relational operators.
ropsCoxGadget :: (K.CContent c) => [C.Rop c]
ropsCoxGadget = Rop.rops "cox-gadget"
    [ consConst      K.& [ "const R"                       K.& "-lit" ]
    , consGeoDatumJp K.& [ "geo-datum-jp E E E -to /N /N"  K.& "-n -x -y . -to" ]
    , consGeoDegree  K.& [ "geo-degree /N /P /P /P"        K.& "-real -deg -min -sec" ]
    , consInterp     K.& [ "interp E"                      K.& "-interp . -x?" ]
    , consNumber     K.& [ "number /N -order /P ..."       K.& "-term . -order? -from?" ]
    , consRank       K.& [ "rank /N -order /P ..."         K.& "-term . -order? -from? -dense?" ]
    , consRepeat     K.& [ "repeat I R"                    K.& "-count -relmap/" ]
    , consArray      K.& [ "array /P /P ... -to /N [E] ..."
                           K.& "to : -term* . -to" ]
    , consUnarray    K.& [ "unarray /N /N ... -from /P [E] ..."
                           K.& "from : -term* . -from" ]
    ]


-- ----------------------  const

-- | [const /E/]
--     Output the constant relation E.
--     Especially, @const {= [] =}@ is equivalent to @dee@,
--     @const {= =}@ is equivalent to @dum@.
--
consConst :: (K.CContent c) => C.RopCons c
consConst med =
    do lit <- Rop.getContent med "-lit"
       case K.isRel lit of
         True  -> Right $ relmapConst med $ K.gRel lit
         False -> Msg.reqRel

-- | Create @const@ relmap.
relmapConst :: C.Intmed c -> K.Rel c -> C.Relmap c
relmapConst med = C.relmapFlow med . relkitConst

-- | Create @const@ relkit.
relkitConst :: K.Rel c -> C.RelkitFlow c
relkitConst _ Nothing = C.relkitUnfixed
relkitConst r _ = Right $ C.relkitConstRel r


-- ----------------------  geo-datum-jp

--  geo-datum-jp /n /x /y -to /lat /long

consGeoDatumJp :: (Ord c, K.CContent c) => C.RopCons c
consGeoDatumJp med =
    do n  <- Rop.getCox med "-n"
       x  <- Rop.getCox med "-x"
       y  <- Rop.getCox med "-y"
       (lat, long) <- Rop.getTerm2 med "-to"
       let cops = K.getCops $ C.getGlobal med
       Right $ relmapGeoDatumJp med (cops, (n,x,y), (lat,long))

-- | Create @geo-datum-jp@ relmap.
relmapGeoDatumJp :: (Ord c, K.CContent c) => C.Intmed c -> (K.CopSet c, K.Cox3 c, K.TermName2) -> C.Relmap c
relmapGeoDatumJp med = C.relmapFlow med . relkitGeoDatumJp

-- | Create @geo-datum-jp@ relkit.
relkitGeoDatumJp :: (Ord c, K.CContent c) => (K.CopSet c, K.Cox3 c, K.TermName2) -> C.RelkitFlow c
relkitGeoDatumJp _ Nothing = C.relkitUnfixed
relkitGeoDatumJp (cops, (coxn,coxx,coxy), (lat,long)) (Just he1) = Right kit2 where
    he2       = K.headAppend [lat, long] he1
    kit2      = C.relkitLineAb False he2 flow
    pReal     = K.pDec . K.realDecimal 4

    flow cs   = do cn    <- K.calcCox cops he1 cs coxn
                   cx    <- K.calcCox cops he1 cs coxx
                   cy    <- K.calcCox cops he1 cs coxy

                   decn  <- K.getDec $ Right cn
                   decx  <- K.getDec $ Right cx
                   decy  <- K.getDec $ Right cy

                   let n  = fromInteger $ K.decimalNum decn
                       dx = K.decimalFractional decx :: Double
                       dy = K.decimalFractional decy :: Double
                       (dlat, dlong) = Rop.convDegree n (dx, dy)

                   Right $ pReal dlat : pReal dlong : cs


-- ----------------------  geo-degree

--  geo-degree /deg-real /deg /min /sec

consGeoDegree :: (Ord c, K.CContent c) => C.RopCons c
consGeoDegree med =
    do real <- Rop.getTerm med "-real"
       deg  <- Rop.getTerm med "-deg"
       mnt  <- Rop.getTerm med "-min"
       sec  <- Rop.getTerm med "-sec"
       Right $ relmapGeoDegree med (real, deg, mnt, sec)

-- | Create @geo-degree@ relmap.
relmapGeoDegree :: (Ord c, K.CContent c) => C.Intmed c -> K.TermName4 -> C.Relmap c
relmapGeoDegree med = C.relmapFlow med . relkitGeoDegree

-- | Create @geo-degree@ relkit.
relkitGeoDegree :: (Ord c, K.CContent c) => K.TermName4 -> C.RelkitFlow c
relkitGeoDegree _ Nothing = C.relkitUnfixed
relkitGeoDegree (real, deg, mnt, sec) (Just he1) = Right kit2 where
    he2       = K.headCons real he1
    kit2      = C.relkitLineAb False he2 flow
    pick      = K.pickDirect [deg, mnt, sec] he1
    pReal     = K.pDec . K.realDecimal 4

    flow cs   = do let [cdeg, cmnt, csec] = pick cs

                   hdeg <- K.getDec $ Right cdeg
                   hmnt <- K.getDec $ Right cmnt
                   hsec <- K.getDec $ Right csec

                   let ddeg = K.decimalFractional hdeg :: Double
                       dmnt = K.decimalFractional hmnt :: Double
                       dsec = K.decimalFractional hsec :: Double
                       dmnt' = dmnt + dsec  / 60
                       ddeg' = ddeg + dmnt' / 60

                   Right $ pReal ddeg' : cs


-- ----------------------  interp

consInterp :: (K.CContent c) => C.RopCons c
consInterp med =
    do skip <- Rop.getSwitch med "-x"
       case skip of
         True  -> Right $ Rop.relmapId med
         False -> consInterp2 med

consInterp2 :: (K.CContent c) => C.RopCons c
consInterp2 med =
    do c <- Rop.getContent med "-interp"
       case K.isInterp c of
         True  -> Right $ relmapInterp med $ K.gInterp c
         False -> Msg.reqInterp

-- | Create @interp@ relmap.
relmapInterp :: (K.CContent c) => C.Intmed c -> K.Interp -> C.Relmap c
relmapInterp med = C.relmapFlow med . relkitInterp

-- | Create @interp@ relkit.
relkitInterp :: (K.CContent c) => K.Interp -> C.RelkitFlow c
relkitInterp _ Nothing = C.relkitUnfixed
relkitInterp interp (Just he1)
    | interpMatch interp he1 = Right $ C.relkitId (Just he1)
    | otherwise              = Msg.unkTerm (K.interpTerms interp) he1

interpMatch :: K.Interp -> K.Head -> Bool
interpMatch interp he = ns1 == ns2 where
    ns1 = K.sort $ K.interpTerms interp
    ns2 = K.sort $ K.getTermNames he


-- ----------------------  number

-- | __number \/N -from I -order \/P ...__
consNumber :: (Ord c, K.CContent c) => C.RopCons c
consNumber med =
    do n    <- Rop.getTerm                med "-term"
       ns   <- Rop.getOpt [] Rop.getTerms med "-order"
       from <- Rop.getOpt 0  Rop.getInt   med "-from"
       Right $ relmapNumber med (n, ns, from)

-- | Create @number@ relmap.
relmapNumber :: (K.CDec c, Ord c) => C.Intmed c -> (K.TermName, [K.TermName], Int) -> C.Relmap c
relmapNumber med = C.relmapFlow med . relkitNumber

-- | Create @number@ relkit.
relkitNumber :: (Ord c, K.CDec c) => (K.TermName, [K.TermName], Int) -> C.RelkitFlow c
relkitNumber = relkitRanking K.sortByNameNumbering

relkitRanking
    :: (Ord c, K.CDec c)
    => K.Ranking K.TermName c
    -> (K.TermName, [K.TermName], Int) -> C.RelkitFlow c
relkitRanking _ _ Nothing = C.relkitUnfixed
relkitRanking ranking (n, ns, from) (Just he1) = Right kit2 where
    he2   = K.headCons n he1
    kit2  = C.relkitWhole False he2 flow
    flow bo1  = let (rank, bo2) = ranking from ords (K.getTermNames he1) bo1
                in zipWith (:) (map K.pInt rank) bo2
    ords  = (K.orderingCap . K.orderingTermName) <$> ns


-- ----------------------  rank

-- | [rank \/N -from I -order \/P ...]
--     Calculate standard competition ranking (like 1224).
--
--   [rank \/N -dense -from I -order \/P...]
--     Calculate dense ranking (like 1223).
--
consRank :: (Ord c, K.CContent c) => C.RopCons c
consRank med =
    do n     <- Rop.getTerm             med "-term"
       ns    <- Rop.getTerms            med "-order"
       from  <- Rop.getOpt 0 Rop.getInt med "-from"
       dense <- Rop.getSwitch           med "-dense"
       let relmapRank = if dense
                        then relmapDenseRank
                        else relmapGapRank
       Right $ relmapRank med (n, ns, from)

-- | Create @rank -dense@ relmap.
relmapDenseRank :: (K.CDec c, Ord c) =>
   C.Intmed c -> (K.TermName, [K.TermName], Int) -> C.Relmap c
relmapDenseRank med = C.relmapFlow med . relkitDenseRank

-- | Create @rank -dense@ relkit.
relkitDenseRank :: (Ord c, K.CDec c) => (K.TermName, [K.TermName], Int) -> C.RelkitFlow c
relkitDenseRank = relkitRanking K.sortByNameDenseRank

-- | Create @rank@ relmap.
relmapGapRank :: (K.CDec c, Ord c) =>
   C.Intmed c -> (K.TermName, [K.TermName], Int) -> C.Relmap c
relmapGapRank med = C.relmapFlow med . relkitGapRank

-- | Create @rank@ relkit.
relkitGapRank :: (Ord c, K.CDec c) => (K.TermName, [K.TermName], Int) -> C.RelkitFlow c
relkitGapRank = relkitRanking K.sortByNameGapRank


-- ----------------------  repeat

-- | __repeat I R__
consRepeat :: (Ord c, K.CContent c) => C.RopCons c
consRepeat med =
  do cnt  <- Rop.getInteger med "-count"
     rmap <- Rop.getRelmap med "-relmap"
     Right $ relmapRepeat med cnt rmap

-- | Create @repeat@ relmap.
relmapRepeat :: (Ord c) => C.Intmed c -> Integer -> K.Map (C.Relmap c)
relmapRepeat med cnt = C.relmapBinary med $ relkitRepeat cnt

-- | Create @repeat@ relkit.
relkitRepeat :: forall c. (Ord c) => Integer -> C.RelkitBinary c
relkitRepeat cnt (C.RelkitOutput he2 kitb2) (Just he1)
    | K.headEquiv he1 he2 = Right $ kit3
    | otherwise = Msg.diffHead [he1, he2]
    where
    kit3 = C.relkitConflWhole True he1 f [kitb2]
    f bmaps bo1 =
        do let [bmap2] = bmaps
               bmap2'  = C.bmapAlign he2 he1 bmap2
           bo2 <- rep bmap2' cnt bo1
           Right bo2

    rep bmap2' = loop where
        loop c bo | c > 0     = loop (c - 1) =<< bmap2' bo
                  | otherwise = Right bo

relkitRepeat _ _ _ = C.relkitUnfixed


-- ----------------------  array

-- | [array /\/P \/P/ ... -to /\/N E/ ...]
--     Convert discrete tuples of /\/P \/P/ ...
--     to term array /\/N/ ....
--     The first /\/P/ of /\/P \/P/ is a key term
--     of which content is equal to /E/,
--     and the second is a value of the key.
--     When /E/ is omitted, term name content is used,
--     e.g., @-to \/1 \/2@ means @-to \/1 '\/1 \/2 '\/2@.
--     The length of /\/P \/P/ ... must be even.
--     Relmap like @array \/k \/v \/ka \/va -to \/1 \/1a \/2 \/2a ...@
--     works for multiple key-value pairs.
--
--   > RELMAP
--   >   array /key /val  -to /1 1  /2 2  /3 3  /4 4
--   >                           :     :     :     :
--   >                           :.....:.....:.....:.... /key
--   >
--   > INPUT                    OUTPUT
--   >   /group  /key  /val       /group  /1    /2    /3    /4
--   >   ======= ===== -----      ======= ----- ----- ----- -----
--   >   'a      1     10         'a      10    20    30    ()
--   >   'a      2     20         'b      40    50    ()    ()
--   >   'a      3     30                 .                  .
--   >   'b      1     40                 :..................:
--   >   'b      2     50                 term array
--
consArray :: (K.CContent c) => C.RopCons c
consArray med =
  do ps   <- Rop.getTermPairs med "-term"
     ts   <- Rop.getNamedContentTerms med "-to"
     let l = length ps
     Right $ relmapArray med (ps, l, divide l ts)

-- | Create @array@ relmap.
relmapArray :: (K.CContent c) => C.Intmed c -> ([K.TermName2], Int, [[K.Term c]]) -> C.Relmap c
relmapArray med = C.relmapFlow med . relkitArray

-- | Create @array@ relkit.
relkitArray :: forall c. (K.CContent c) => ([K.TermName2], Int, [[K.Term c]]) -> C.RelkitFlow c
relkitArray _ Nothing = C.relkitUnfixed
relkitArray (unzip -> (names, vals), l, to) (Just he1) = check kit where
    pre      = K.termPicker (names ++ vals) he1
    name     = K.termPicker names he1
    val      = K.termPicker vals he1
    split cs = (K.cutTerms pre cs, (K.pickTerms name cs, K.pickTerms val cs))

    news     = fst <$> concat to
    keys     = snd K.<$$> to
    pk       = K.termPicker news he1
    he2      = K.headAppend news $ K.headMap (K.cutTerms pre) he1
    check    = Rop.preCheck name . Rop.preCheck val . Rop.newCheck pk
    kit      = Right $ C.relkitWhole False he2 flow
    flow bo  = array <$> K.gatherToAssoc (split <$> bo)

    array :: K.Pair [c] [K.Twin [c]] -> [c]
    array (group, body) = (values body K.<++> keys) ++ group

    values :: [K.Twin [c]] -> [c] -> [c]
    values body key = case select key `K.mapMaybe` body of
                        [vs] -> vs           -- single key
                        _    -> K.empties l  -- no keys or multiple keys

    select :: [c] -> K.Pair [c] [c] -> Maybe [c]
    select key (ks, vs) | key == ks  = Just vs
                        | otherwise  = Nothing

-- >>> divide 2 "abcdef"
-- ["ab","cd","ef"]
divide :: Int -> [a] -> [[a]]
divide n = loop where
    loop [] = []
    loop xs = take n xs : loop (drop n xs)


-- ----------------------  unarray

-- | [unarray /\/N \/N/ ... -from /\/P E/ ...]
--     Convert term array /\/P/ ... 
--     to discrete tuples of /\/N \/N/ ....
--
--   > RELMAP
--   >   unarray /key /val  -from /1 1  /2 2  /3 3  /4 4
--   >                               :     :     :     :
--   >                               :.....:.....:.....:.... /key
--   >
--   > INPUT                                 OUTPUT
--   >   /group  /1    /2    /3    /4          /group  /key  /val
--   >   ======= ----- ----- ----- -----       ======= ===== -----
--   >   'a      10    20    30    ()          'a      1     10
--   >   'b      40    50    ()    ()          'a      2     20
--   >           .                  .          'a      3     30
--   >           :..................:          'a      4     ()
--   >           term array                    'b      1     40
--   >                                         'b      2     50
--   >                                         'b      3     ()
--   >                                         'b      4     ()
--
consUnarray :: (K.CContent c) => C.RopCons c
consUnarray med =
  do ps   <- Rop.getTermPairs med "-term"
     ts   <- Rop.getNamedContentTerms med "-from"
     let l = length ps
     Right $ relmapUnarray med (ps, l, divide l ts)

-- | Create @unarray@ relmap.
relmapUnarray :: (K.CContent c) => C.Intmed c -> ([K.TermName2], Int, [[K.Term c]]) -> C.Relmap c
relmapUnarray med = C.relmapFlow med . relkitUnarray

-- | Create @unarray@ relkit.
relkitUnarray :: forall c. (K.CContent c) => ([K.TermName2], Int, [[K.Term c]]) -> C.RelkitFlow c
relkitUnarray _ Nothing = C.relkitUnfixed
relkitUnarray (unzip -> (names, vals), _, from) (Just he1) = check kit where
    news     = names ++ vals
    new      = K.termPicker news he1

    ns       = fst K.<$$> from
    keys     = snd K.<$$> from
    pre      = K.termPicker (concat ns) he1
    pks      = (`K.termPicker` he1) <$> ns
    he2      = K.headAppend news $ K.headMap (K.cutTerms pre) he1
    check    = Rop.newCheck new . Rop.preCheck pre
    kit      = Right $ C.relkitMany False he2 flow

    flow :: [c] -> [[c]]
    flow cs = zipWith (f cs) keys pks
    f cs ks pk = ks ++ K.pickTerms pk cs ++ K.cutTerms pre cs

