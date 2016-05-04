{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Koshucode.Baala.Rop.Cox.Accessor
  ( ropsCoxAccessor,
    -- * clock
    relmapClock, relkitClock,
    -- * clock-get
    relmapClockGet, relkitClockGet,
    -- * clock-alter
    relmapClockAlter, relkitClockAlter,
  ) where

import qualified Koshucode.Baala.Base         as B
import qualified Koshucode.Baala.Syntax       as S
import qualified Koshucode.Baala.Data         as D
import qualified Koshucode.Baala.Core         as C
import qualified Koshucode.Baala.Rop.Base     as Op
import qualified Koshucode.Baala.Rop.Cox.Get  as Op


-- | Implementation of relational operators.
ropsCoxAccessor :: (D.CContent c) => [C.Rop c]
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

consClock :: (D.CContent c) => C.RopCons c
consClock med =
    do cops     <- Op.getWhere    med "-where"
       clock    <- Op.getTerm     med "-clock"
       times    <- Op.getOptionCox (D.pInt 1) med "-times"
       day      <- Op.getOptionCox (D.pInt 0) med "-day"
       hour     <- Op.getMaybeCox med "-hour"
       minute   <- Op.getMaybeCox med "-min"
       sec      <- Op.getMaybeCox med "-sec"
       let hms   = fill (hour, minute, sec)
       Right $ relmapClock med (cops, clock, (times, day, hms))
    where
      zero = D.coxLit $ D.pInt 0
      fill (h, Nothing, Just s) = fill (h, Just zero, Just s)
      fill (Nothing, Just m, s) = fill (Just zero, Just m, s)
      fill hms = hms

relmapClock :: (D.CContent c) =>
  C.Intmed c -> (D.CopSet c, S.TermName, (D.Cox c, D.Cox c, (D.MaybeCox c, D.MaybeCox c, D.MaybeCox c))) -> C.Relmap c
relmapClock med = C.relmapFlow med . relkitClock

relkitClock :: (D.CContent c)
  => (D.CopSet c, S.TermName, (D.Cox c, D.Cox c, (D.MaybeCox c, D.MaybeCox c, D.MaybeCox c)))
  -> C.RelkitFlow c
relkitClock _ Nothing = Right C.relkitNothing
relkitClock (cops, n, (times, day, (hour, minute, sec))) (Just he1) = Right kit2 where
      he2       = n `D.headCons` he1
      kit2      = C.relkitJust he2 $ C.RelkitOneToAbOne False f2 []
      f2 _ cs1  = do let run = D.coxRunCox cops he1 cs1
                     t <- getInt     $ run times
                     d <- getInteger $ run day
                     h <- getMaybe (getInt . run) hour
                     m <- getMaybe (getInt . run) minute
                     s <- getMaybe (getInt . run) sec
                     let clock = D.clockTimes t $ clockFrom d h m s
                     Right $ D.pClock clock : cs1

clockFrom :: Integer -> Maybe Int -> Maybe Int -> Maybe Int -> D.Clock
clockFrom d (Just h)  (Just m)  (Just s)   = D.clockFromDhms d h m s
clockFrom d (Just h)  (Just m)  (Nothing)  = D.clockFromDhm  d h m
clockFrom d (Just h)  (Nothing) (Nothing)  = D.clockFromDh   d h
clockFrom d (Nothing) (Nothing) (Nothing)  = D.clockFromD    d
clockFrom _ _ _ _ = B.bug "clockFrom"

getMaybe :: (c -> B.Ab a) -> Maybe c -> B.Ab (Maybe a)
getMaybe f (Just c)  = return . Just =<< f c
getMaybe _ _         = return Nothing

getInt :: (D.CContent c) => B.Ab c -> B.Ab Int
getInt c = do d <- D.getDec c
              return $ fromInteger $ D.decimalNum d

getInteger :: (D.CContent c) => B.Ab c -> B.Ab Integer
getInteger c = do i <- getInt c
                  return $ toInteger i


-- ----------------------  clock-get

consClockGet :: (D.CContent c) => C.RopCons c
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

relmapClockGet :: (D.CContent c) =>
  C.Intmed c -> (D.CopSet c, D.Cox c, [Maybe S.TermName]) -> C.Relmap c
relmapClockGet med = C.relmapFlow med . relkitClockGet

relkitClockGet :: (D.CContent c) =>
  (D.CopSet c, D.Cox c, [Maybe S.TermName]) -> C.RelkitFlow c
relkitClockGet _ Nothing = Right C.relkitNothing
relkitClockGet (cops, cox, ns) (Just he1) = Right kit2 where
      he2       = B.catMaybes ns `D.headAppend` he1
      kit2      = C.relkitJust he2 $ C.RelkitOneToAbOne False f2 []
      f2 _ cs1  = do clock <- D.getClock $ D.coxRunCox cops he1 cs1 cox
                     let cs2 = B.zipMaybe2 ns $ clockProps clock
                     Right $ cs2 ++ cs1

clockProps :: (D.CContent c) => D.Clock -> [c]
clockProps clock = [sign, day, hour, minute, sec] where
    sign          = D.pInt $ D.clockSign clock
    day           = D.pInteger d
    hour          = D.maybeEmpty D.pInt h
    minute        = D.maybeEmpty D.pInt m
    sec           = D.maybeEmpty D.pInt s
    (d, h, m, s)  = D.clockDhms clock


-- ----------------------  clock-alter

consClockAlter :: (D.CContent c) => C.RopCons c
consClockAlter med =
    do cops     <- Op.getWhere    med "-where"
       clock    <- Op.getTerm     med "-clock"
       day      <- Op.getMaybeCox med "-day"
       hour     <- Op.getMaybeCox med "-hour"
       minute   <- Op.getMaybeCox med "-min"
       sec      <- Op.getMaybeCox med "-sec"
       Right $ relmapClockAlter med (cops, clock, (day, hour, minute, sec))

relmapClockAlter :: (D.CContent c) =>
  C.Intmed c -> (D.CopSet c, S.TermName, (D.MaybeCox c, D.MaybeCox c, D.MaybeCox c, D.MaybeCox c)) -> C.Relmap c
relmapClockAlter med = C.relmapFlow med . relkitClockAlter

relkitClockAlter :: (D.CContent c)
  => (D.CopSet c, S.TermName, (D.MaybeCox c, D.MaybeCox c, D.MaybeCox c, D.MaybeCox c))
  -> C.RelkitFlow c
relkitClockAlter _ Nothing = Right C.relkitNothing
relkitClockAlter (cops, n, (day, hour, minute, sec)) (Just he1) = Right kit2 where
      ns1       = D.headNames he1
      ind       = [n] `B.snipIndex` ns1
      pick      = B.snipFrom ind
      cut       = B.snipOff ind
      fore      = B.snipForward ind
      he2       = D.headMap fore he1
      kit2      = C.relkitJust he2 $ C.RelkitOneToAbOne False f2 []
      f2 _ cs1  = do let run    = D.coxRunCox cops he1 cs1
                         [c]    = pick cs1
                         clock  = D.gClock c
                     d <- getMaybe (getInteger . run) day
                     h <- getMaybe (getInt . run) hour
                     m <- getMaybe (getInt . run) minute
                     s <- getMaybe (getInt . run) sec
                     let clock' = D.clockAlter d h m s clock
                     Right $ D.pClock clock' : cut cs1
