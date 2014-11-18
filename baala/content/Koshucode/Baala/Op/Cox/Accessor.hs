{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Koshucode.Baala.Op.Cox.Accessor
  ( ropsCoxAccessor,
    -- * clock
    relmapClock, relkitClock,
    -- * clock-get
    relmapClockGet, relkitClockGet,
    -- * clock-alter
    relmapClockAlter, relkitClockAlter,
  ) where

import qualified Koshucode.Baala.Base        as B
import qualified Koshucode.Baala.Core        as C
import qualified Koshucode.Baala.Op.Builtin  as Op
import qualified Koshucode.Baala.Op.Cox.Get  as Op


-- | Implementation of relational operators.
ropsCoxAccessor :: (C.CContent c) => [C.Rop c]
ropsCoxAccessor = Op.ropList "cox-accessor"
    --         CONSTRUCTOR    USAGE
    --                        ATTRIBUTE
    [ Op.ropI  consClock      "clock /N -PROP E ..."
                              "-clock | -times -day -hour -min -sec"
    , Op.ropV  consClockGet   "clock-get E -PROP /N ..."
                              "-clock | -sign -day -hour -min -sec"
    , Op.ropI  consClockAlter "clock-alter /P -PROP E ..."
                              "-clock | -sign -day -hour -min -sec "
    ]


-- ----------------------  clock

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
  C.RopUse c -> (C.CopSet c, B.TermName, (C.Cox c, C.Cox c, (C.MaybeCox c, C.MaybeCox c, C.MaybeCox c))) -> C.Relmap c
relmapClock use = C.relmapFlow use . relkitClock

relkitClock :: (C.CContent c)
  => (C.CopSet c, B.TermName, (C.Cox c, C.Cox c, (C.MaybeCox c, C.MaybeCox c, C.MaybeCox c)))
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
  C.RopUse c -> (C.CopSet c, B.TermName, (C.MaybeCox c, C.MaybeCox c, C.MaybeCox c, C.MaybeCox c)) -> C.Relmap c
relmapClockAlter use = C.relmapFlow use . relkitClockAlter

relkitClockAlter :: (C.CContent c)
  => (C.CopSet c, B.TermName, (C.MaybeCox c, C.MaybeCox c, C.MaybeCox c, C.MaybeCox c))
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
