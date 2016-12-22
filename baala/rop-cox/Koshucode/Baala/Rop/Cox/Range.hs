{-# OPTIONS_GHC -Wall #-}

-- | Generate range of contents.

module Koshucode.Baala.Rop.Cox.Range
  ( ropsCoxRange,
    RangeAttr,
    -- * range
    consRange, relmapRange,
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
import qualified Koshucode.Baala.DataPlus      as K
import qualified Koshucode.Baala.Core          as C
import qualified Koshucode.Baala.Rop.Base      as Rop


-- | Implementation of relational operators.
ropsCoxRange :: (K.CContent c) => [C.Rop c]
ropsCoxRange = Rop.rops "cox-calc"
    [ consRange         K.& [ "range /N -from E -to E"          K.& "-term . -from -to" ]
    , consRangeYear     K.& [ "range-year /N -from /P to /P"    K.& "-term . -from -to" ]
    , consRangeMonth    K.& [ "range-month /N -from /P to /P"   K.& "-term . -from -to" ]
    , consRangeDay      K.& [ "range-day /N -from /P to /P"     K.& "-term . -from -to" ]
    , consRangeHour     K.& [ "range-hour /N -from /P to /P"    K.& "-term . -from -to" ]
    , consRangeMinute   K.& [ "range-minute /N -from /P to /P"  K.& "-term . -from -to" ]
    , consRangeSecond   K.& [ "range-second /N -from /P to /P"  K.& "-term . -from -to" ]
    ]


-- ----------------------  range

-- | Attribute for range operators.
type RangeAttr c = (K.TermName, K.CopSet c, K.Cox c, K.Cox c)

getRangeAttr :: (K.CContent c) => C.Intmed c -> K.Ab (RangeAttr c)
getRangeAttr med =
  do term     <- Rop.getTerm med "-term"
     coxLow   <- Rop.getCox  med "-from"
     coxHigh  <- Rop.getCox  med "-to"
     let cops = C.globalCopset $ C.ropGlobal med
     Right (term, cops, coxLow, coxHigh)

-- | __range \/N -from E -to E__
consRange :: (K.CContent c) => C.RopCons c
consRange med = Right . relmapRange med =<< getRangeAttr med

-- | Create @range@ relmap.
relmapRange :: (K.CContent c) => C.Intmed c -> RangeAttr c -> C.Relmap c
relmapRange med = C.relmapFlow med . relkitRange

-- | Create @range@ relkit.
relkitRange :: (K.CContent c) => RangeAttr c -> C.RelkitFlow c
relkitRange _ Nothing = Right C.relkitNothing
relkitRange (n, cops, coxLow, coxHigh) (Just he1) = Right kit2 where
    he2      = K.headCons n he1
    kit2     = C.relkitAbMany he2 False flow
    flow cs  = do decLow    <- K.getDec $ K.coxRunCox cops he1 cs coxLow
                  decHigh   <- K.getDec $ K.coxRunCox cops he1 cs coxHigh

                  let low    = K.decimalNum decLow
                      high   = K.decimalNum decHigh
                      decs   = map K.pInteger [low .. high]

                  Right $ map (: cs) decs


-- ----------------------  range-year

consRangeYear :: (K.CContent c) => C.RopCons c
consRangeYear med = Right . relmapRangeYear med =<< getRangeAttr med

-- | Create @range-year@ relmap.
relmapRangeYear :: (K.CContent c) => C.Intmed c -> RangeAttr c -> C.Relmap c
relmapRangeYear med = C.relmapFlow med . relkitRangeYear

-- | Create @range-year@ relkit.
relkitRangeYear :: (K.CContent c) => RangeAttr c -> C.RelkitFlow c
relkitRangeYear = relkitRangeBy K.timeRangeYear


-- ----------------------  range-month

consRangeMonth :: (K.CContent c) => C.RopCons c
consRangeMonth med = Right . relmapRangeMonth med =<< getRangeAttr med

-- | Create @range-month@ relmap.
relmapRangeMonth :: (K.CContent c) => C.Intmed c -> RangeAttr c -> C.Relmap c
relmapRangeMonth med = C.relmapFlow med . relkitRangeMonth

-- | Create @range-month@ relkit.
relkitRangeMonth :: (K.CContent c) => RangeAttr c -> C.RelkitFlow c
relkitRangeMonth = relkitRangeBy K.timeRangeMonth


-- ----------------------  range-day

consRangeDay :: (K.CContent c) => C.RopCons c
consRangeDay med = Right . relmapRangeDay med =<< getRangeAttr med

-- | Create @range-day@ relmap.
relmapRangeDay :: (K.CContent c) => C.Intmed c -> RangeAttr c -> C.Relmap c
relmapRangeDay med = C.relmapFlow med . relkitRangeDay

-- | Create @range-day@ relkit.
relkitRangeDay :: (K.CContent c) => RangeAttr c -> C.RelkitFlow c
relkitRangeDay = relkitRangeBy K.timeRangeDay

-- | Create /range-by/ relkit.
relkitRangeBy :: (K.CContent c) => K.RangeBy K.Time -> RangeAttr c -> C.RelkitFlow c
relkitRangeBy _ _ Nothing = Right C.relkitNothing
relkitRangeBy range (n, cops, from, to) (Just he1) = Right kit2 where
    he2      = K.headCons n he1
    kit2     = C.relkitAbMany he2 False flow
    flow cs  = do timeFrom  <- K.getTime $ K.coxRunCox cops he1 cs from
                  timeTo    <- K.getTime $ K.coxRunCox cops he1 cs to
                  let ts     = map K.pTime $ range timeFrom timeTo
                  Right $ map (: cs) ts


-- ----------------------  range-hour

consRangeHour :: (K.CContent c) => C.RopCons c
consRangeHour med = Right . relmapRangeHour med =<< getRangeAttr med

-- | Create @range-hour@ relmap.
relmapRangeHour :: (K.CContent c) => C.Intmed c -> RangeAttr c -> C.Relmap c
relmapRangeHour med = C.relmapFlow med . relkitRangeHour

-- | Create @range-hour@ relkit.
relkitRangeHour :: (K.CContent c) => RangeAttr c -> C.RelkitFlow c
relkitRangeHour = relkitRangeClock 3600


-- ----------------------  range-minute

consRangeMinute :: (K.CContent c) => C.RopCons c
consRangeMinute med = Right . relmapRangeMinute med =<< getRangeAttr med

-- | Create @range-minute@ relmap.
relmapRangeMinute :: (K.CContent c) => C.Intmed c -> RangeAttr c -> C.Relmap c
relmapRangeMinute med = C.relmapFlow med . relkitRangeMinute

-- | Create @range-minute@ relkit.
relkitRangeMinute :: (K.CContent c) => RangeAttr c -> C.RelkitFlow c
relkitRangeMinute = relkitRangeClock 60


-- ----------------------  range-second

consRangeSecond :: (K.CContent c) => C.RopCons c
consRangeSecond med = Right . relmapRangeSecond med =<< getRangeAttr med

-- | Create @range-second@ relmap.
relmapRangeSecond :: (K.CContent c) => C.Intmed c -> RangeAttr c -> C.Relmap c
relmapRangeSecond med = C.relmapFlow med . relkitRangeSecond

-- | Create @range-second@ relkit.
relkitRangeSecond :: (K.CContent c) => RangeAttr c -> C.RelkitFlow c
relkitRangeSecond = relkitRangeClock 1

-- | Create /range-clock/relkit.
relkitRangeClock :: (K.CContent c) => Int -> RangeAttr c -> C.RelkitFlow c
relkitRangeClock _ _ Nothing = Right C.relkitNothing
relkitRangeClock sec (n, cops, from, to) (Just he1) = Right kit2 where
    he2      = K.headCons n he1
    kit2     = C.relkitAbMany he2 False flow
    flow cs  = do clockFrom  <- K.getClock $ K.coxRunCox cops he1 cs from
                  clockTo    <- K.getClock $ K.coxRunCox cops he1 cs to
                  let range   = K.clockRangeBy $ K.clockStep sec
                      clocks  = map K.pClock $ range clockFrom clockTo
                  Right $ map (: cs) clocks
