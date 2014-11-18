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
    --         CONSTRUCTOR    USAGE                        ATTRIBUTE
    [ Op.ropI  consConst      "const R"                    "-lit"
    , Op.ropI  consInterp     "interp E"                   "-interp | -x"
    , Op.ropI  consNumber     "number /N -order /N ..."    "-term | -order -from"
    , Op.ropI  consRank       "rank /N -order /N ..."      "-term | -order -from -dense"
    , Op.ropII consRepeat     "repeat N R"                 "-count -relmap/"
    , Op.ropI  consClock      "clock /N -PROP E ..."
               "-clock | -times -day -hour -min -sec"
    , Op.ropV  consClockGet   "clock-get E -PROP /N ..."
               "-clock | -sign -day -hour -min -sec"
    , Op.ropI  consClockAlter "clock-alter /P -PROP E ..."
               "-clock | -sign -day -hour -min -sec "
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


-- ----------------------  interp

consInterp :: (C.CContent c) => C.RopCons c
consInterp use =
    do skip <- Op.getSwitch use "-x"
       case skip of
         True  -> Right $ Op.relmapId use
         False -> consInterp2 use

consInterp2 :: (C.CContent c) => C.RopCons c
consInterp2 use =
    do c <- Op.getContent use "-interp"
       case C.isInterp c of
         True  -> Right $ relmapInterp use $ C.gInterp c
         False -> Msg.reqInterp

relmapInterp :: (C.CContent c) => C.RopUse c -> B.Interp -> C.Relmap c
relmapInterp use = C.relmapFlow use . relkitInterp

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


-- ----------------------  clock

type MaybeCox c  = Maybe (C.Cox c)

consClock :: (C.CContent c) => C.RopCons c
consClock use =
    do cops     <- Op.getWhere    use "-where"
       clock    <- Op.getTerm     use "-clock"
       times    <- Op.getOptionCox (C.pDecFromInt 1) use "-times"
       day      <- Op.getOptionCox (C.pDecFromInt 0) use "-day"
       hour     <- Op.getMaybeCox use "-hour"
       minute   <- Op.getMaybeCox use "-min"
       sec      <- Op.getMaybeCox use "-sec"
       let hms   = fill (hour, minute, sec)
       Right $ relmapClock use (cops, clock, (times, day, hms))
    where
      zero = C.coxLit $ C.pDecFromInt 0
      fill (h, Nothing, Just s) = fill (h, Just zero, Just s)
      fill (Nothing, Just m, s) = fill (Just zero, Just m, s)
      fill hms = hms

relmapClock :: (C.CContent c) =>
  C.RopUse c -> (C.CopSet c, B.TermName, (C.Cox c, C.Cox c, (MaybeCox c, MaybeCox c, MaybeCox c))) -> C.Relmap c
relmapClock use = C.relmapFlow use . relkitClock

relkitClock :: (C.CContent c)
  => (C.CopSet c, B.TermName, (C.Cox c, C.Cox c, (MaybeCox c, MaybeCox c, MaybeCox c)))
  -> C.RelkitFlow c
relkitClock _ Nothing = Right C.relkitNothing
relkitClock (cops, n, (times, day, (hour, minute, sec))) (Just he1) = Right kit2 where
      he2       = n `B.headCons` he1
      kit2      = C.relkitJust he2 $ C.RelkitOneToAbOne False f2 []
      f2 _ cs1  = do let run = C.coxRunCox cops he1 cs1
                     t <- getInt     $ run times
                     d <- getInteger $ run day
                     h <- getMaybe (getInt . run) hour
                     m <- getMaybe (getInt . run) minute
                     s <- getMaybe (getInt . run) sec
                     let clock = B.clockTimes t $ clockFrom d h m s
                     Right $ C.pClock clock : cs1

clockFrom :: Integer -> Maybe Int -> Maybe Int -> Maybe Int -> B.Clock
clockFrom d (Just h)  (Just m)  (Just s)   = B.clockFromDhms d h m s
clockFrom d (Just h)  (Just m)  (Nothing)  = B.clockFromDhm  d h m
clockFrom d (Just h)  (Nothing) (Nothing)  = B.clockFromDh   d h
clockFrom d (Nothing) (Nothing) (Nothing)  = B.clockFromD    d
clockFrom _ _ _ _ = B.bug "clockFrom"

getMaybe :: (c -> B.Ab a) -> Maybe c -> B.Ab (Maybe a)
getMaybe f (Just c)  = return . Just =<< f c
getMaybe _ _         = return Nothing

getInt :: (C.CContent c) => B.Ab c -> B.Ab Int
getInt c = do d <- C.getDec c
              return $ B.decimalNum d

getInteger :: (C.CContent c) => B.Ab c -> B.Ab Integer
getInteger c = do i <- getInt c
                  return $ toInteger i


-- ----------------------  clock-get

consClockGet :: (C.CContent c) => C.RopCons c
consClockGet use =
  do cops     <- Op.getWhere   use "-where"
     clock    <- Op.getCox     use "-clock"
     sign     <- Op.getTermOpt use "-sign"
     day      <- Op.getTermOpt use "-day"
     hour     <- Op.getTermOpt use "-hour"
     minute   <- Op.getTermOpt use "-min"
     sec      <- Op.getTermOpt use "-sec"
     let ns    = [sign, day, hour, minute, sec]
     Right $ relmapClockGet use (cops, clock, ns)

relmapClockGet :: (C.CContent c) =>
  C.RopUse c -> (C.CopSet c, C.Cox c, [Maybe B.TermName]) -> C.Relmap c
relmapClockGet use = C.relmapFlow use . relkitClockGet

relkitClockGet :: (C.CContent c) =>
  (C.CopSet c, C.Cox c, [Maybe B.TermName]) -> C.RelkitFlow c
relkitClockGet _ Nothing = Right C.relkitNothing
relkitClockGet (cops, cox, ns) (Just he1) = Right kit2 where
      he2       = B.catMaybes ns `B.headAppend` he1
      kit2      = C.relkitJust he2 $ C.RelkitOneToAbOne False f2 []
      f2 _ cs1  = do clock <- C.getClock $ C.coxRunCox cops he1 cs1 cox
                     let cs2 = B.zipMaybe2 ns $ clockProps clock
                     Right $ cs2 ++ cs1

clockProps :: (C.CContent c) => B.Clock -> [c]
clockProps clock = [sign, day, hour, minute, sec] where
    sign          = C.pDecFromInt $ B.clockSign clock
    day           = C.pDecFromInteger d
    hour          = C.maybeEmpty C.pDecFromInt h
    minute        = C.maybeEmpty C.pDecFromInt m
    sec           = C.maybeEmpty C.pDecFromInt s
    (d, h, m, s)  = B.clockDhms clock


-- ----------------------  clock-alter

consClockAlter :: (C.CContent c) => C.RopCons c
consClockAlter use =
    do cops     <- Op.getWhere    use "-where"
       clock    <- Op.getTerm     use "-clock"
       day      <- Op.getMaybeCox use "-day"
       hour     <- Op.getMaybeCox use "-hour"
       minute   <- Op.getMaybeCox use "-min"
       sec      <- Op.getMaybeCox use "-sec"
       Right $ relmapClockAlter use (cops, clock, (day, hour, minute, sec))

relmapClockAlter :: (C.CContent c) =>
  C.RopUse c -> (C.CopSet c, B.TermName, (MaybeCox c, MaybeCox c, MaybeCox c, MaybeCox c)) -> C.Relmap c
relmapClockAlter use = C.relmapFlow use . relkitClockAlter

relkitClockAlter :: (C.CContent c)
  => (C.CopSet c, B.TermName, (MaybeCox c, MaybeCox c, MaybeCox c, MaybeCox c))
  -> C.RelkitFlow c
relkitClockAlter _ Nothing = Right C.relkitNothing
relkitClockAlter (cops, n, (day, hour, minute, sec)) (Just he1) = Right kit2 where
      ns1       = B.headNames he1
      ind       = [n] `B.snipIndex` ns1
      pick      = B.snipFrom ind
      cut       = B.snipOff ind
      fore      = B.snipFore ind
      he2       = B.headMap fore he1
      kit2      = C.relkitJust he2 $ C.RelkitOneToAbOne False f2 []
      f2 _ cs1  = do let run    = C.coxRunCox cops he1 cs1
                         [c]    = pick cs1
                         clock  = C.gClock c
                     d <- getMaybe (getInteger . run) day
                     h <- getMaybe (getInt . run) hour
                     m <- getMaybe (getInt . run) minute
                     s <- getMaybe (getInt . run) sec
                     let clock' = B.clockAlter d h m s clock
                     Right $ C.pClock clock' : cut cs1
