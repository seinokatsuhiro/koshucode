{-# OPTIONS_GHC -Wall #-}

-- | Relmap operators for clock type.

module Koshucode.Baala.Rop.Cox.Type.Clock
  ( ropsTypeClock,
    -- * add-clock
    consAddClock, relmapAddClock, relkitAddClock,
    -- * of-clock
    consOfClock, relmapOfClock, relkitOfClock,
    -- * alt-clock
    consAltClock, relmapAltClock, relkitAltClock,
  ) where

import qualified Koshucode.Baala.Overture     as O
import qualified Koshucode.Baala.Base         as B
import qualified Koshucode.Baala.Syntax       as S
import qualified Koshucode.Baala.Data         as D
import qualified Koshucode.Baala.Core         as C
import qualified Koshucode.Baala.Rop.Base     as Rop
import qualified Koshucode.Baala.Rop.Cox.Get  as Rop

-- | Implementation of relational operators.
ropsTypeClock :: (D.CContent c) => [C.Rop c]
ropsTypeClock = Rop.rops "type"
    [ consAddClock
      O.& [ "add-clock /N [-day E] [-hour E] [-min E] [-sec E]"
            O.& "-clock . -times? -day? -hour? -min? -sec?" ]
    , consOfClock
      O.& [ "of-clock E [-sign /N] [-day /N] [-hour /N] [-min /N] [-sec /N]"
            O.& "-content . -sign? -day? -hour? -min? -sec?" ]
    , consAltClock
      O.&  [ "alt-clock /P [-sign E] [-day E] [-hour E] [-min E] [-sec E]"
             O.& "-clock . -sign? -day? -hour? -min? -sec?" ]
    ]


-- ----------------------  clock

-- | [add-clock \/c -day /D/]   Add term \/c as a clock of /D/ days, i.e., @|D'|@.
--   [add-clock \/c -hour /H/]  Add term \/c as a clock of /H/ hours, i.e., @|H|@.
--   [add-clock \/c -min /M/]   Add term \/c as a clock of /M/ minutes, i.e., @|00:M|@.
--   [add-clock \/c -sec /S/]   Add term \/c as a clock of /S/ seconds, i.e., @|00:00:S|@.
--
consAddClock :: (D.CContent c) => C.RopCons c
consAddClock med =
    do cops     <- Rop.getWhere    med "-where"
       clock    <- Rop.getTerm     med "-clock"
       times    <- Rop.getOptionCox (D.pInt 1) med "-times"
       day      <- Rop.getOptionCox (D.pInt 0) med "-day"
       hour     <- Rop.getMaybeCox med "-hour"
       minute   <- Rop.getMaybeCox med "-min"
       sec      <- Rop.getMaybeCox med "-sec"
       let hms   = fill (hour, minute, sec)
       Right $ relmapAddClock med (cops, clock, (times, day, hms))
    where
      zero = D.coxLit $ D.pInt 0
      fill (h, Nothing, Just s) = fill (h, Just zero, Just s)
      fill (Nothing, Just m, s) = fill (Just zero, Just m, s)
      fill hms = hms

-- | Create @clock@ relmap.
relmapAddClock :: (D.CContent c) =>
  C.Intmed c -> (D.CopSet c, S.TermName, (D.Cox c, D.Cox c, (D.MaybeCox c, D.MaybeCox c, D.MaybeCox c))) -> C.Relmap c
relmapAddClock med = C.relmapFlow med . relkitAddClock

-- | Create @clock@ relkit.
relkitAddClock :: (D.CContent c)
  => (D.CopSet c, S.TermName, (D.Cox c, D.Cox c, (D.MaybeCox c, D.MaybeCox c, D.MaybeCox c)))
  -> C.RelkitFlow c
relkitAddClock _ Nothing = Right C.relkitNothing
relkitAddClock (cops, n, (times, day, (hour, minute, sec))) (Just he1) = Right kit2 where
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


-- ----------------------  of-clock

-- | [of-clock /E/ -day \/N]    Add term \/N as decimal days of clock.
--   [of-clock /E/ -hour \/N]   Add term \/N as decimal hours of clock.
--   [of-clock /E/ -min \/N]    Add term \/N as decimal minutes of clock.
--   [of-clock /E/ -sec \/N]    Add term \/N as decimal seconds of clock.
--   [of-clock /E/ -sign \/N]   Add term \/N as decimal sign of clock, i.e., -1, 0, or 1.
--
consOfClock :: (D.CContent c) => C.RopCons c
consOfClock med =
  do cops     <- Rop.getWhere   med "-where"
     content  <- Rop.getCox     med "-content"
     sign     <- Rop.getTermOpt med "-sign"
     day      <- Rop.getTermOpt med "-day"
     hour     <- Rop.getTermOpt med "-hour"
     minute   <- Rop.getTermOpt med "-min"
     sec      <- Rop.getTermOpt med "-sec"
     Right $ relmapOfClock med (cops, content, [sign, day, hour, minute, sec])

-- | Create @of-clockt@ relmap.
relmapOfClock :: (D.CContent c) =>
  C.Intmed c -> (D.CopSet c, D.Cox c, [Maybe S.TermName]) -> C.Relmap c
relmapOfClock med = C.relmapFlow med . relkitOfClock

-- | Create @of-clock@ relkit.
relkitOfClock :: (D.CContent c) =>
  (D.CopSet c, D.Cox c, [Maybe S.TermName]) -> C.RelkitFlow c
relkitOfClock _ Nothing = Right C.relkitNothing
relkitOfClock (cops, cox, ns) (Just he1) = Right kit2 where
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


-- ----------------------  alt-clock

-- | [alt-clock \/P -day /D/]    Alter days of clock to /D/ in term \/P.
--   [alt-clock \/P -hour /H/]   Alter hours of clock to /H/ in term \/P.
--   [alt-clock \/P -min /M/]    Alter minutes of clock to /M/ in term \/P.
--   [alt-clock \/P -sec /S/]    Alter seconds of clock to /S/ in term \/P.
--
consAltClock :: (D.CContent c) => C.RopCons c
consAltClock med =
    do cops     <- Rop.getWhere    med "-where"
       clock    <- Rop.getTerm     med "-clock"
       day      <- Rop.getMaybeCox med "-day"
       hour     <- Rop.getMaybeCox med "-hour"
       minute   <- Rop.getMaybeCox med "-min"
       sec      <- Rop.getMaybeCox med "-sec"
       Right $ relmapAltClock med (cops, clock, (day, hour, minute, sec))

-- | Create @alt-clock@ relmap.
relmapAltClock :: (D.CContent c) =>
  C.Intmed c -> (D.CopSet c, S.TermName, (D.MaybeCox c, D.MaybeCox c, D.MaybeCox c, D.MaybeCox c)) -> C.Relmap c
relmapAltClock med = C.relmapFlow med . relkitAltClock

-- | Create @alt-clock@ relkit.
relkitAltClock :: (D.CContent c)
  => (D.CopSet c, S.TermName, (D.MaybeCox c, D.MaybeCox c, D.MaybeCox c, D.MaybeCox c))
  -> C.RelkitFlow c
relkitAltClock _ Nothing = Right C.relkitNothing
relkitAltClock (cops, n, (day, hour, minute, sec)) (Just he1) = Right kit2 where
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
