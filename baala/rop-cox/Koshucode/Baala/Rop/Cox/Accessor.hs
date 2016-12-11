{-# OPTIONS_GHC -Wall #-}

-- | Accessor relmap operators.

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
import qualified Koshucode.Baala.Rop.Base     as Rop
import qualified Koshucode.Baala.Rop.Cox.Get  as Rop


-- | Implementation of relational operators.
ropsCoxAccessor :: (D.CContent c) => [C.Rop c]
ropsCoxAccessor = Rop.ropList "cox-accessor"
    --        CONSTRUCTOR    USAGE
    --                       ATTRIBUTE
    [ Rop.def consClock      "clock /N -PROP E ..."
                             "-clock . -times? -day? -hour? -min? -sec?"
    , Rop.def consClockGet   "clock-get E -PROP /N ..."
                             "-clock* . -sign? -day? -hour? -min? -sec?"
    , Rop.def consClockAlter "clock-alter /P -PROP E ..."
                             "-clock . -sign? -day? -hour? -min? -sec?"
    ]


-- ----------------------  clock

consClock :: (D.CContent c) => C.RopCons c
consClock med =
    do cops     <- Rop.getWhere    med "-where"
       clock    <- Rop.getTerm     med "-clock"
       times    <- Rop.getOptionCox (D.pInt 1) med "-times"
       day      <- Rop.getOptionCox (D.pInt 0) med "-day"
       hour     <- Rop.getMaybeCox med "-hour"
       minute   <- Rop.getMaybeCox med "-min"
       sec      <- Rop.getMaybeCox med "-sec"
       let hms   = fill (hour, minute, sec)
       Right $ relmapClock med (cops, clock, (times, day, hms))
    where
      zero = D.coxLit $ D.pInt 0
      fill (h, Nothing, Just s) = fill (h, Just zero, Just s)
      fill (Nothing, Just m, s) = fill (Just zero, Just m, s)
      fill hms = hms

-- | Create @clock@ relmap.
relmapClock :: (D.CContent c) =>
  C.Intmed c -> (D.CopSet c, S.TermName, (D.Cox c, D.Cox c, (D.MaybeCox c, D.MaybeCox c, D.MaybeCox c))) -> C.Relmap c
relmapClock med = C.relmapFlow med . relkitClock

-- | Create @clock@ relkit.
relkitClock :: (D.CContent c)
  => (D.CopSet c, S.TermName, (D.Cox c, D.Cox c, (D.MaybeCox c, D.MaybeCox c, D.MaybeCox c)))
  -> C.RelkitFlow c
relkitClock _ Nothing = Right C.relkitNothing
relkitClock (cops, n, (times, day, (hour, minute, sec))) (Just he1) = Right kit2 where
      he2       = n `D.headCons` he1
      kit2      = C.relkitJust he2 $ C.RelkitAbLinear False f2 []
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
  do cops     <- Rop.getWhere   med "-where"
     clock    <- Rop.getCox     med "-clock"
     sign     <- Rop.getTermOpt med "-sign"
     day      <- Rop.getTermOpt med "-day"
     hour     <- Rop.getTermOpt med "-hour"
     minute   <- Rop.getTermOpt med "-min"
     sec      <- Rop.getTermOpt med "-sec"
     let ns    = [sign, day, hour, minute, sec]
     Right $ relmapClockGet med (cops, clock, ns)

-- | Create @clock-get@ relmap.
relmapClockGet :: (D.CContent c) =>
  C.Intmed c -> (D.CopSet c, D.Cox c, [Maybe S.TermName]) -> C.Relmap c
relmapClockGet med = C.relmapFlow med . relkitClockGet

-- | Create @clock-get@ relkit.
relkitClockGet :: (D.CContent c) =>
  (D.CopSet c, D.Cox c, [Maybe S.TermName]) -> C.RelkitFlow c
relkitClockGet _ Nothing = Right C.relkitNothing
relkitClockGet (cops, cox, ns) (Just he1) = Right kit2 where
      he2       = B.catMaybes ns `D.headAppend` he1
      kit2      = C.relkitJust he2 $ C.RelkitAbLinear False f2 []
      f2 _ cs1  = do clock <- D.getClock $ D.coxRunCox cops he1 cs1 cox
                     let cs2 = B.zipMaybe2 ns $ clockContents clock
                     Right $ cs2 ++ cs1

clockContents :: (D.CContent c) => D.Clock -> [c]
clockContents clock = [sign, day, hour, minute, sec] where
    sign          = D.pInt $ D.clockSign clock
    day           = D.pInteger d
    hour          = D.maybeEmpty D.pInt h
    minute        = D.maybeEmpty D.pInt m
    sec           = D.maybeEmpty D.pInt s
    (d, h, m, s)  = D.clockAtts clock


-- ----------------------  clock-alter

consClockAlter :: (D.CContent c) => C.RopCons c
consClockAlter med =
    do cops     <- Rop.getWhere    med "-where"
       clock    <- Rop.getTerm     med "-clock"
       day      <- Rop.getMaybeCox med "-day"
       hour     <- Rop.getMaybeCox med "-hour"
       minute   <- Rop.getMaybeCox med "-min"
       sec      <- Rop.getMaybeCox med "-sec"
       Right $ relmapClockAlter med (cops, clock, (day, hour, minute, sec))

-- | Create @clock-alter@ relmap.
relmapClockAlter :: (D.CContent c) =>
  C.Intmed c -> (D.CopSet c, S.TermName, (D.MaybeCox c, D.MaybeCox c, D.MaybeCox c, D.MaybeCox c)) -> C.Relmap c
relmapClockAlter med = C.relmapFlow med . relkitClockAlter

-- | Create @clock-alter@ relkit.
relkitClockAlter :: (D.CContent c)
  => (D.CopSet c, S.TermName, (D.MaybeCox c, D.MaybeCox c, D.MaybeCox c, D.MaybeCox c))
  -> C.RelkitFlow c
relkitClockAlter _ Nothing = Right C.relkitNothing
relkitClockAlter (cops, n, (day, hour, minute, sec)) (Just he1) = Right kit2 where
      ns1       = D.getTermNames he1
      ind       = [n] `B.selectIndex` ns1
      pick      = B.selectElems    ind
      cut       = B.selectOthers   ind
      fore      = B.permuteForward ind
      he2       = D.headMap fore he1
      kit2      = C.relkitJust he2 $ C.RelkitAbLinear False f2 []
      f2 _ cs1  = do let run    = D.coxRunCox cops he1 cs1
                         [c]    = pick cs1
                         clock  = D.gClock c
                     d <- getMaybe (getInteger . run) day
                     h <- getMaybe (getInt . run) hour
                     m <- getMaybe (getInt . run) minute
                     s <- getMaybe (getInt . run) sec
                     let clock' = D.clockAlter d h m s clock
                     Right $ D.pClock clock' : cut cs1
