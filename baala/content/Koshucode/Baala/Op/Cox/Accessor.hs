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
    --       CONSTRUCTOR     USAGE
    --                       ATTRIBUTE
    [ Op.def consClock       "clock /N -PROP E ..."
                             "1 -clock | -times -day -hour -min -sec"
    , Op.def consClockGet    "clock-get E -PROP /N ..."
                             "V -clock | -sign -day -hour -min -sec"
    , Op.def consClockAlter  "clock-alter /P -PROP E ..."
                             "1 -clock | -sign -day -hour -min -sec "
    ]


-- ----------------------  clock

consClock :: (C.CContent c) => C.RopCons c
consClock med =
    do cops     <- Op.getWhere    med "-where"
       clock    <- Op.getTerm     med "-clock"
       times    <- Op.getOptionCox (C.pInt 1) med "-times"
       day      <- Op.getOptionCox (C.pInt 0) med "-day"
       hour     <- Op.getMaybeCox med "-hour"
       minute   <- Op.getMaybeCox med "-min"
       sec      <- Op.getMaybeCox med "-sec"
       let hms   = fill (hour, minute, sec)
       Right $ relmapClock med (cops, clock, (times, day, hms))
    where
      zero = C.coxLit $ C.pInt 0
      fill (h, Nothing, Just s) = fill (h, Just zero, Just s)
      fill (Nothing, Just m, s) = fill (Just zero, Just m, s)
      fill hms = hms

relmapClock :: (C.CContent c) =>
  C.Intmed c -> (C.CopSet c, B.TermName, (C.Cox c, C.Cox c, (C.MaybeCox c, C.MaybeCox c, C.MaybeCox c))) -> C.Relmap c
relmapClock med = C.relmapFlow med . relkitClock

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
consClockGet med =
  do cops     <- Op.getWhere   med "-where"
     clock    <- Op.getCox     med "-clock"
     sign     <- Op.getTermOpt med "-sign"
     day      <- Op.getTermOpt med "-day"
     hour     <- Op.getTermOpt med "-hour"
     minute   <- Op.getTermOpt med "-min"
     sec      <- Op.getTermOpt med "-sec"
     let ns    = [sign, day, hour, minute, sec]
     Right $ relmapClockGet med (cops, clock, ns)

relmapClockGet :: (C.CContent c) =>
  C.Intmed c -> (C.CopSet c, C.Cox c, [Maybe B.TermName]) -> C.Relmap c
relmapClockGet med = C.relmapFlow med . relkitClockGet

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
    sign          = C.pInt $ B.clockSign clock
    day           = C.pInteger d
    hour          = C.maybeEmpty C.pInt h
    minute        = C.maybeEmpty C.pInt m
    sec           = C.maybeEmpty C.pInt s
    (d, h, m, s)  = B.clockDhms clock


-- ----------------------  clock-alter

consClockAlter :: (C.CContent c) => C.RopCons c
consClockAlter med =
    do cops     <- Op.getWhere    med "-where"
       clock    <- Op.getTerm     med "-clock"
       day      <- Op.getMaybeCox med "-day"
       hour     <- Op.getMaybeCox med "-hour"
       minute   <- Op.getMaybeCox med "-min"
       sec      <- Op.getMaybeCox med "-sec"
       Right $ relmapClockAlter med (cops, clock, (day, hour, minute, sec))

relmapClockAlter :: (C.CContent c) =>
  C.Intmed c -> (C.CopSet c, B.TermName, (C.MaybeCox c, C.MaybeCox c, C.MaybeCox c, C.MaybeCox c)) -> C.Relmap c
relmapClockAlter med = C.relmapFlow med . relkitClockAlter

relkitClockAlter :: (C.CContent c)
  => (C.CopSet c, B.TermName, (C.MaybeCox c, C.MaybeCox c, C.MaybeCox c, C.MaybeCox c))
  -> C.RelkitFlow c
relkitClockAlter _ Nothing = Right C.relkitNothing
relkitClockAlter (cops, n, (day, hour, minute, sec)) (Just he1) = Right kit2 where
      ns1       = B.headNames he1
      ind       = [n] `B.snipIndex` ns1
      pick      = B.snipFrom ind
      cut       = B.snipOff ind
      fore      = B.snipForward ind
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
