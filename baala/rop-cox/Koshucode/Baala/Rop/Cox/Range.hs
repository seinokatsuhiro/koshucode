{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Rop.Cox.Range
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
import qualified Koshucode.Baala.Base          as B
import qualified Koshucode.Baala.Syntax        as S
import qualified Koshucode.Baala.Data          as D
import qualified Koshucode.Baala.Core          as C
import qualified Koshucode.Baala.Rop.Base      as Op
import qualified Koshucode.Baala.Rop.Cox.Get   as Op


-- | Implementation of relational operators.
ropsCoxRange :: (D.CContent c) => [C.Rop c]
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

type RangeAttr c = (S.TermName, D.CopSet c, D.Cox c, D.Cox c)

getRangeAttr :: (D.CContent c) => C.Intmed c -> B.Ab (RangeAttr c)
getRangeAttr med =
  do term     <- Op.getTerm med "-term"
     coxLow   <- Op.getCox  med "-from"
     coxHigh  <- Op.getCox  med "-to"
     let cops = C.globalCopset $ C.ropGlobal med
     Right (term, cops, coxLow, coxHigh)

consRange :: (D.CContent c) => C.RopCons c
consRange med = Right . relmapRange med =<< getRangeAttr med

relmapRange :: (D.CContent c) => C.Intmed c -> RangeAttr c -> C.Relmap c
relmapRange med = C.relmapFlow med . relkitRange

relkitRange :: (D.CContent c) => RangeAttr c -> C.RelkitFlow c
relkitRange _ Nothing = Right C.relkitNothing
relkitRange (n, cops, coxLow, coxHigh) (Just he1) = Right kit2 where
    he2      = D.headCons n he1
    kit2     = C.relkitJust he2 $ C.RelkitOneToAbMany False f2 []
    f2 _ cs  = do decLow    <- D.getDec $ D.coxRunCox cops he1 cs coxLow
                  decHigh   <- D.getDec $ D.coxRunCox cops he1 cs coxHigh

                  let low    = D.decimalNum decLow
                      high   = D.decimalNum decHigh
                      decs   = map D.pInteger [low .. high]

                  Right $ map (: cs) decs


-- ----------------------  range-year

consRangeYear :: (D.CContent c) => C.RopCons c
consRangeYear med = Right . relmapRangeYear med =<< getRangeAttr med

relmapRangeYear :: (D.CContent c) => C.Intmed c -> RangeAttr c -> C.Relmap c
relmapRangeYear med = C.relmapFlow med . relkitRangeYear

relkitRangeYear :: (D.CContent c) => RangeAttr c -> C.RelkitFlow c
relkitRangeYear = relkitRangeBy D.timeRangeYear


-- ----------------------  range-month

consRangeMonth :: (D.CContent c) => C.RopCons c
consRangeMonth med = Right . relmapRangeMonth med =<< getRangeAttr med

relmapRangeMonth :: (D.CContent c) => C.Intmed c -> RangeAttr c -> C.Relmap c
relmapRangeMonth med = C.relmapFlow med . relkitRangeMonth

relkitRangeMonth :: (D.CContent c) => RangeAttr c -> C.RelkitFlow c
relkitRangeMonth = relkitRangeBy D.timeRangeMonth


-- ----------------------  range-day

consRangeDay :: (D.CContent c) => C.RopCons c
consRangeDay med = Right . relmapRangeDay med =<< getRangeAttr med

relmapRangeDay :: (D.CContent c) => C.Intmed c -> RangeAttr c -> C.Relmap c
relmapRangeDay med = C.relmapFlow med . relkitRangeDay

relkitRangeDay :: (D.CContent c) => RangeAttr c -> C.RelkitFlow c
relkitRangeDay = relkitRangeBy D.timeRangeDay

relkitRangeBy :: (D.CContent c) => B.RangeBy D.Time -> RangeAttr c -> C.RelkitFlow c
relkitRangeBy _ _ Nothing = Right C.relkitNothing
relkitRangeBy range (n, cops, from, to) (Just he1) = Right kit2 where
    he2      = D.headCons n he1
    kit2     = C.relkitJust he2 $ C.RelkitOneToAbMany False f2 []
    f2 _ cs  = do timeFrom  <-  D.getTime $ D.coxRunCox cops he1 cs from
                  timeTo    <-  D.getTime $ D.coxRunCox cops he1 cs to
                  let ts    =   map D.pTime $ range timeFrom timeTo
                  Right $ map (: cs) ts


-- ----------------------  range-hour

consRangeHour :: (D.CContent c) => C.RopCons c
consRangeHour med = Right . relmapRangeHour med =<< getRangeAttr med

relmapRangeHour :: (D.CContent c) => C.Intmed c -> RangeAttr c -> C.Relmap c
relmapRangeHour med = C.relmapFlow med . relkitRangeHour

relkitRangeHour :: (D.CContent c) => RangeAttr c -> C.RelkitFlow c
relkitRangeHour = relkitRangeClock 3600


-- ----------------------  range-minute

consRangeMinute :: (D.CContent c) => C.RopCons c
consRangeMinute med = Right . relmapRangeMinute med =<< getRangeAttr med

relmapRangeMinute :: (D.CContent c) => C.Intmed c -> RangeAttr c -> C.Relmap c
relmapRangeMinute med = C.relmapFlow med . relkitRangeMinute

relkitRangeMinute :: (D.CContent c) => RangeAttr c -> C.RelkitFlow c
relkitRangeMinute = relkitRangeClock 60


-- ----------------------  range-second

consRangeSecond :: (D.CContent c) => C.RopCons c
consRangeSecond med = Right . relmapRangeSecond med =<< getRangeAttr med

relmapRangeSecond :: (D.CContent c) => C.Intmed c -> RangeAttr c -> C.Relmap c
relmapRangeSecond med = C.relmapFlow med . relkitRangeSecond

relkitRangeSecond :: (D.CContent c) => RangeAttr c -> C.RelkitFlow c
relkitRangeSecond = relkitRangeClock 1

relkitRangeClock :: (D.CContent c) => Int -> RangeAttr c -> C.RelkitFlow c
relkitRangeClock _ _ Nothing = Right C.relkitNothing
relkitRangeClock sec (n, cops, from, to) (Just he1) = Right kit2 where
    he2      = D.headCons n he1
    kit2     = C.relkitJust he2 $ C.RelkitOneToAbMany False f2 []
    f2 _ cs  = do clockFrom  <- D.getClock $ D.coxRunCox cops he1 cs from
                  clockTo    <- D.getClock $ D.coxRunCox cops he1 cs to
                  let range   = D.clockRangeBy $ D.clockStep sec
                      clocks  = map D.pClock $ range clockFrom clockTo
                  Right $ map (: cs) clocks
