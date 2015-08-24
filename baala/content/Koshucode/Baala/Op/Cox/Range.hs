{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Cox.Range
  ( ropsCoxRange,
    RangeAttr,
  
    -- * range
    consRange, relmapRange,
    -- $range
  
    -- * range-year
    relmapRangeYear, relkitRangeYear,
    -- * range-month
    relmapRangeMonth, relkitRangeMonth,
    -- * range-day
    relmapRangeDay, relkitRangeDay,

    -- * range-hour
    relmapRangeHour, relkitRangeHour,
    -- * range-minute
    relmapRangeMinute, relkitRangeMinute,
    -- * range-second
    relmapRangeSecond, relkitRangeSecond,
  ) where

import Prelude hiding (getContents)
import qualified Koshucode.Baala.Base        as B
import qualified Koshucode.Baala.Core        as C
import qualified Koshucode.Baala.Op.Builtin  as Op
import qualified Koshucode.Baala.Op.Cox.Get  as Op


-- | Implementation of relational operators.
ropsCoxRange :: (C.CContent c) => [C.Rop c]
ropsCoxRange = Op.ropList "cox-calc"
    --        CONSTRUCTOR       USAGE                             ATTRIBUTE
    [ Op.def  consRange         "range /N -from E -to E"          "1 -term | -from -to"
    , Op.def  consRangeYear     "range-year /N -from /P to /P"    "1 -term | -from -to"
    , Op.def  consRangeMonth    "range-month /N -from /P to /P"   "1 -term | -from -to"
    , Op.def  consRangeDay      "range-day /N -from /P to /P"     "1 -term | -from -to"
    , Op.def  consRangeHour     "range-hour /N -from /P to /P"    "1 -term | -from -to"
    , Op.def  consRangeMinute   "range-minute /N -from /P to /P"  "1 -term | -from -to"
    , Op.def  consRangeSecond   "range-second /N -from /P to /P"  "1 -term | -from -to"
    ]


-- ----------------------  range

-- $range
--
--  Add term @\/n@ @0@, @\/n@ @1@, ..., and @\/n@ @9@.
--  
--    > range /n -from 0 -to 9

type RangeAttr c = (B.TermName, C.CopSet c, C.Cox c, C.Cox c)

getRangeAttr :: (C.CContent c) => C.Intmed c -> B.Ab (RangeAttr c)
getRangeAttr med =
  do term     <- Op.getTerm med "-term"
     coxLow   <- Op.getCox  med "-from"
     coxHigh  <- Op.getCox  med "-to"
     let cops = C.globalCopset $ C.ropGlobal med
     Right (term, cops, coxLow, coxHigh)

consRange :: (C.CContent c) => C.RopCons c
consRange med = Right . relmapRange med =<< getRangeAttr med

relmapRange :: (C.CContent c) => C.Intmed c -> RangeAttr c -> C.Relmap c
relmapRange med = C.relmapFlow med . relkitRange

relkitRange :: (C.CContent c) => RangeAttr c -> C.RelkitFlow c
relkitRange _ Nothing = Right C.relkitNothing
relkitRange (n, cops, coxLow, coxHigh) (Just he1) = Right kit2 where
    he2      = B.headCons n he1
    kit2     = C.relkitJust he2 $ C.RelkitOneToAbMany False f2 []
    f2 _ cs  = do decLow    <- C.getDec $ C.coxRunCox cops he1 cs coxLow
                  decHigh   <- C.getDec $ C.coxRunCox cops he1 cs coxHigh

                  let low    = B.decimalNum decLow
                      high   = B.decimalNum decHigh
                      decs   = map C.pInteger [low .. high]

                  Right $ map (: cs) decs


-- ----------------------  range-year

consRangeYear :: (C.CContent c) => C.RopCons c
consRangeYear med = Right . relmapRangeYear med =<< getRangeAttr med

relmapRangeYear :: (C.CContent c) => C.Intmed c -> RangeAttr c -> C.Relmap c
relmapRangeYear med = C.relmapFlow med . relkitRangeYear

relkitRangeYear :: (C.CContent c) => RangeAttr c -> C.RelkitFlow c
relkitRangeYear = relkitRangeBy B.timeRangeYear


-- ----------------------  range-month

consRangeMonth :: (C.CContent c) => C.RopCons c
consRangeMonth med = Right . relmapRangeMonth med =<< getRangeAttr med

relmapRangeMonth :: (C.CContent c) => C.Intmed c -> RangeAttr c -> C.Relmap c
relmapRangeMonth med = C.relmapFlow med . relkitRangeMonth

relkitRangeMonth :: (C.CContent c) => RangeAttr c -> C.RelkitFlow c
relkitRangeMonth = relkitRangeBy B.timeRangeMonth


-- ----------------------  range-day

consRangeDay :: (C.CContent c) => C.RopCons c
consRangeDay med = Right . relmapRangeDay med =<< getRangeAttr med

relmapRangeDay :: (C.CContent c) => C.Intmed c -> RangeAttr c -> C.Relmap c
relmapRangeDay med = C.relmapFlow med . relkitRangeDay

relkitRangeDay :: (C.CContent c) => RangeAttr c -> C.RelkitFlow c
relkitRangeDay = relkitRangeBy B.timeRangeDay

relkitRangeBy :: (C.CContent c) => B.RangeBy B.Time -> RangeAttr c -> C.RelkitFlow c
relkitRangeBy _ _ Nothing = Right C.relkitNothing
relkitRangeBy range (n, cops, from, to) (Just he1) = Right kit2 where
    he2      = B.headCons n he1
    kit2     = C.relkitJust he2 $ C.RelkitOneToAbMany False f2 []
    f2 _ cs  = do timeFrom  <-  C.getTime $ C.coxRunCox cops he1 cs from
                  timeTo    <-  C.getTime $ C.coxRunCox cops he1 cs to
                  let ts    =   map C.pTime $ range timeFrom timeTo
                  Right $ map (: cs) ts


-- ----------------------  range-hour

consRangeHour :: (C.CContent c) => C.RopCons c
consRangeHour med = Right . relmapRangeHour med =<< getRangeAttr med

relmapRangeHour :: (C.CContent c) => C.Intmed c -> RangeAttr c -> C.Relmap c
relmapRangeHour med = C.relmapFlow med . relkitRangeHour

relkitRangeHour :: (C.CContent c) => RangeAttr c -> C.RelkitFlow c
relkitRangeHour = relkitRangeClock 3600


-- ----------------------  range-minute

consRangeMinute :: (C.CContent c) => C.RopCons c
consRangeMinute med = Right . relmapRangeMinute med =<< getRangeAttr med

relmapRangeMinute :: (C.CContent c) => C.Intmed c -> RangeAttr c -> C.Relmap c
relmapRangeMinute med = C.relmapFlow med . relkitRangeMinute

relkitRangeMinute :: (C.CContent c) => RangeAttr c -> C.RelkitFlow c
relkitRangeMinute = relkitRangeClock 60


-- ----------------------  range-second

consRangeSecond :: (C.CContent c) => C.RopCons c
consRangeSecond med = Right . relmapRangeSecond med =<< getRangeAttr med

relmapRangeSecond :: (C.CContent c) => C.Intmed c -> RangeAttr c -> C.Relmap c
relmapRangeSecond med = C.relmapFlow med . relkitRangeSecond

relkitRangeSecond :: (C.CContent c) => RangeAttr c -> C.RelkitFlow c
relkitRangeSecond = relkitRangeClock 1

relkitRangeClock :: (C.CContent c) => Int -> RangeAttr c -> C.RelkitFlow c
relkitRangeClock _ _ Nothing = Right C.relkitNothing
relkitRangeClock sec (n, cops, from, to) (Just he1) = Right kit2 where
    he2      = B.headCons n he1
    kit2     = C.relkitJust he2 $ C.RelkitOneToAbMany False f2 []
    f2 _ cs  = do clockFrom  <- C.getClock $ C.coxRunCox cops he1 cs from
                  clockTo    <- C.getClock $ C.coxRunCox cops he1 cs to
                  let range   = B.clockRangeBy $ B.clockStep sec
                      clocks  = map C.pClock $ range clockFrom clockTo
                  Right $ map (: cs) clocks
