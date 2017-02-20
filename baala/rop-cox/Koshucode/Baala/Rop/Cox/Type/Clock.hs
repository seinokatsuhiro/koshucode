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
    -- * to-clock
    consToClock, relmapToClock, relkitToClock,
  ) where

import qualified Koshucode.Baala.DataPlus          as K
import qualified Koshucode.Baala.Core              as C
import qualified Koshucode.Baala.Rop.Base          as Rop
import qualified Koshucode.Baala.Rop.Base.Message  as Msg

-- | Implementation of relational operators.
ropsTypeClock :: (K.CContent c) => [C.Rop c]
ropsTypeClock = Rop.rops "type"
    [ consAddClock
      K.& [ "add-clock /N [-day E] [-hour E] [-min E] [-sec E]"
            K.& "-clock . -times? -day? -hour? -min? -sec? -let?" ]
    , consOfClock
      K.& [ "of-clock E [-sign /N] [-day /N] [-hour /N] [-min /N] [-sec /N]"
            K.& "-content . -sign? -day? -hour? -min? -sec? -let?" ]
    , consAltClock
      K.&  [ "alt-clock /P ... [-sign E] [-day E] [-hour E] [-min E] [-sec E]"
             K.& "-clock* . -sign? -day? -hour? -min? -sec? -let?" ]
    , consToClock
      K.&  [ "to-clock /P ... [-replace E]"
             K.& "-term* . -replace?" ]
    ]


-- ----------------------  clock

-- | [add-clock \/c -day /D/]   Add term \/c as a clock of /D/ days, i.e., @|D'|@.
--   [add-clock \/c -hour /H/]  Add term \/c as a clock of /H/ hours, i.e., @|H|@.
--   [add-clock \/c -min /M/]   Add term \/c as a clock of /M/ minutes, i.e., @|00:M|@.
--   [add-clock \/c -sec /S/]   Add term \/c as a clock of /S/ seconds, i.e., @|00:00:S|@.
--
consAddClock :: (K.CContent c) => C.RopCons c
consAddClock med =
    do cops     <- Rop.getLetR     med
       clock    <- Rop.getTerm     med "-clock"
       times    <- Rop.getOptCox (K.pInt 1) med "-times"
       day      <- Rop.getOptCox (K.pInt 0) med "-day"
       hour     <- Rop.getMaybeCox med "-hour"
       minute   <- Rop.getMaybeCox med "-min"
       sec      <- Rop.getMaybeCox med "-sec"
       let hms   = fill (hour, minute, sec)
       Right $ relmapAddClock med (cops, clock, (times, day, hms))
    where
      zero = K.coxLit $ K.pInt 0
      fill (h, Nothing, Just s) = fill (h, Just zero, Just s)
      fill (Nothing, Just m, s) = fill (Just zero, Just m, s)
      fill hms = hms

-- | Create @clock@ relmap.
relmapAddClock :: (K.CContent c) =>
  C.Intmed c -> (K.CopSet c, K.TermName, (K.Cox c, K.Cox c, (K.MaybeCox c, K.MaybeCox c, K.MaybeCox c))) -> C.Relmap c
relmapAddClock med = C.relmapFlow med . relkitAddClock

-- | Create @clock@ relkit.
relkitAddClock :: (K.CContent c)
  => (K.CopSet c, K.TermName, (K.Cox c, K.Cox c, (K.MaybeCox c, K.MaybeCox c, K.MaybeCox c)))
  -> C.RelkitFlow c
relkitAddClock _ Nothing = C.relkitUnfixed
relkitAddClock (cops, n, (times, day, (hour, minute, sec))) (Just he1) = kit where
    pk     = K.termPicker [n] he1
    he2    = n `K.headCons` he1
    kit    = Rop.newCheck pk $ Right $ C.relkitLineAb False he2 f
    f cs1  = do let run = K.calcCox cops he1 cs1
                t <- getInt     $ run times
                d <- getInteger $ run day
                h <- getMaybe (getInt . run) hour
                m <- getMaybe (getInt . run) minute
                s <- getMaybe (getInt . run) sec
                let clock = K.clockTimes t $ clockFrom d h m s
                Right $ K.pClock clock : cs1

clockFrom :: Integer -> Maybe Int -> Maybe Int -> Maybe Int -> K.Clock
clockFrom d (Just h)  (Just m)  (Just s)   = K.clockFromDhms d h m s
clockFrom d (Just h)  (Just m)  (Nothing)  = K.clockFromDhm  d h m
clockFrom d (Just h)  (Nothing) (Nothing)  = K.clockFromDh   d h
clockFrom d (Nothing) (Nothing) (Nothing)  = K.clockFromD    d
clockFrom _ _ _ _ = K.bug "clockFrom"

getMaybe :: (c -> K.Ab a) -> Maybe c -> K.Ab (Maybe a)
getMaybe f (Just c)  = return . Just =<< f c
getMaybe _ _         = return Nothing

getInt :: (K.CContent c) => K.Ab c -> K.Ab Int
getInt c = do d <- K.getDec c
              return $ fromInteger $ K.decimalNum d

getInteger :: (K.CContent c) => K.Ab c -> K.Ab Integer
getInteger c = do i <- getInt c
                  return $ toInteger i


-- ----------------------  of-clock

-- | [of-clock /E/ -day \/N]    Add term \/N as decimal days of clock.
--   [of-clock /E/ -hour \/N]   Add term \/N as decimal hours of clock.
--   [of-clock /E/ -min \/N]    Add term \/N as decimal minutes of clock.
--   [of-clock /E/ -sec \/N]    Add term \/N as decimal seconds of clock.
--   [of-clock /E/ -sign \/N]   Add term \/N as decimal sign of clock, i.e., -1, 0, or 1.
--
consOfClock :: (K.CContent c) => C.RopCons c
consOfClock med =
  do cops     <- Rop.getLetR      med
     content  <- Rop.getCox       med "-content"
     sign     <- Rop.getMaybeTerm med "-sign"
     day      <- Rop.getMaybeTerm med "-day"
     hour     <- Rop.getMaybeTerm med "-hour"
     minute   <- Rop.getMaybeTerm med "-min"
     sec      <- Rop.getMaybeTerm med "-sec"
     Right $ relmapOfClock med (cops, content, [sign, day, hour, minute, sec])

-- | Create @of-clockt@ relmap.
relmapOfClock :: (K.CContent c) =>
  C.Intmed c -> (K.CopSet c, K.Cox c, [Maybe K.TermName]) -> C.Relmap c
relmapOfClock med = C.relmapFlow med . relkitOfClock

-- | Create @of-clock@ relkit.
relkitOfClock :: (K.CContent c) =>
  (K.CopSet c, K.Cox c, [Maybe K.TermName]) -> C.RelkitFlow c
relkitOfClock _ Nothing = C.relkitUnfixed
relkitOfClock (cops, cox, ns) (Just he1) = kit where
    ns'    = K.catMaybes ns
    pk     = K.termPicker ns' he1
    he2    = ns' `K.headAppend` he1
    kit    = Rop.newCheck pk $ Right $ C.relkitLineAb False he2 f
    f cs1  = do clock <- K.getClock $ K.calcCox cops he1 cs1 cox
                let cs2 = K.zipMaybe2 ns $ clockContents clock
                Right $ cs2 ++ cs1

clockContents :: (K.CContent c) => K.Clock -> [c]
clockContents clock = [sign, day, hour, minute, sec] where
    sign          = K.pInt $ K.clockSign clock
    day           = K.pInteger d
    hour          = K.maybeEmpty K.pInt h
    minute        = K.maybeEmpty K.pInt m
    sec           = K.maybeEmpty K.pInt s
    (d, h, m, s)  = K.clockAtts clock


-- ----------------------  alt-clock

-- | [alt-clock \/P ... -day /D/]   Alter days of clock to /D/ in term \/P ....
--   [alt-clock \/P ... -hour /H/]  Alter hours of clock to /H/ in term \/P ....
--   [alt-clock \/P ... -min /M/]   Alter minutes of clock to /M/ in term \/P ....
--   [alt-clock \/P ... -sec /S/]   Alter seconds of clock to /S/ in term \/P ....
--
consAltClock :: (K.CContent c) => C.RopCons c
consAltClock med =
    do cops   <- Rop.getLetR     med
       ts     <- Rop.getTerms    med "-clock"
       day    <- Rop.getMaybeCox med "-day"
       hour   <- Rop.getMaybeCox med "-hour"
       minute <- Rop.getMaybeCox med "-min"
       sec    <- Rop.getMaybeCox med "-sec"
       Right $ relmapAltClock med (cops, ts, (day, hour, minute, sec))

-- | Create @alt-clock@ relmap.
relmapAltClock :: (K.CContent c) =>
  C.Intmed c -> (K.CopSet c, [K.TermName], (K.MaybeCox c, K.MaybeCox c, K.MaybeCox c, K.MaybeCox c)) -> C.Relmap c
relmapAltClock med = C.relmapFlow med . relkitAltClock

-- | Create @alt-clock@ relkit.
relkitAltClock :: (K.CContent c)
  => (K.CopSet c, [K.TermName], (K.MaybeCox c, K.MaybeCox c, K.MaybeCox c, K.MaybeCox c))
  -> C.RelkitFlow c
relkitAltClock _ Nothing = C.relkitUnfixed
relkitAltClock (cops, ns, (day, hour, minute, sec)) (Just he1) = kit where
    pk     = K.termPicker ns he1
    he2    = K.forwardTerms pk `K.headMap` he1
    kit    = Rop.preCheck pk $ Right $ C.relkitLineAb False he2 f
    f cs1  = do let run = K.calcCox cops he1 cs1
                    cs  = K.pickTerms pk cs1
                d <- getMaybe (getInteger . run) day
                h <- getMaybe (getInt . run) hour
                m <- getMaybe (getInt . run) minute
                s <- getMaybe (getInt . run) sec
                clocks <- (K.getClock . Right) K.<#> cs
                let clocks' = (K.pClock . K.clockAlter d h m s) <$> clocks
                Right (clocks' ++ K.cutTerms pk cs1)


-- ----------------------  to-clock

-- | [to-clock /\/P/ ...]
--      Convert content of term /\/P/ ... to clock.
--   [to-clock /\/P/ ... -replace /E/]
--      Convert content of term /\/P/ ... to clock,
--      or replace inconvertible content to /E/.
--
consToClock :: (K.CContent c) => C.RopCons c
consToClock med =
    do cops <- Rop.getLetR     med
       ns   <- Rop.getTerms    med "-term"
       rep' <- Rop.getMaybeCox med "-replace"
       Right $ case rep' of
         Nothing  -> relmapToClock med ns
         Just rep -> relmapToClockReplace med (cops, ns, rep)

-- | Create @to-clock@ relmap.
relmapToClock :: (K.CContent c) => C.Intmed c -> [K.TermName] -> C.Relmap c
relmapToClock med = C.relmapFlow med . relkitToClock

-- | Create @to-clock@ relkit.
relkitToClock :: (K.CContent c) => [K.TermName] -> C.RelkitFlow c
relkitToClock _ Nothing = C.relkitUnfixed
relkitToClock ns (Just he1)
    | K.duplicated ns     = Msg.dupTerm ns
    | K.newTermsExist pk  = Msg.newTerm pk he1
    | otherwise           = Right kit
    where
      pk        = K.termPicker ns he1
      he2       = K.headMap (K.forwardTerms pk) he1
      kit       = C.relkitLine False he2 flow
      flow cs1  = let cs = K.toClockContent <$> K.pickTerms pk cs1
                  in cs ++ K.cutTerms pk cs1

-- | Create @to-clock@ relmap with @-replace@ option.
relmapToClockReplace :: (K.CContent c) => C.Intmed c -> (K.CopSet c, [K.TermName], K.Cox c) -> C.Relmap c
relmapToClockReplace med = C.relmapFlow med . relkitToClockReplace

-- | Create @to-clock@ relkit with @-replace@ option.
relkitToClockReplace :: (K.CContent c) => (K.CopSet c, [K.TermName], K.Cox c) -> C.RelkitFlow c
relkitToClockReplace _ Nothing = C.relkitUnfixed
relkitToClockReplace (cops, ns, rep) (Just he1)
    | K.duplicated ns     = Msg.dupTerm ns
    | K.newTermsExist pk  = Msg.newTerm pk he1
    | otherwise           = Right kit
    where
      pk        = K.termPicker ns he1
      he2       = K.headMap (K.forwardTerms pk) he1
      kit       = C.relkitLineAb False he2 flow
      flow cs1  = do let run = K.calcCox cops he1 cs1
                     cRep <- run rep
                     let cs = K.toClockReplace cRep <$> K.pickTerms pk cs1
                     Right $ cs ++ K.cutTerms pk cs1

