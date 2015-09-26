{-# OPTIONS_GHC -Wall #-}

-- | Content operators.

module Koshucode.Baala.Op.Cop.Time
  ( copsTime
    -- $Operators
  ) where

import qualified Koshucode.Baala.Base       as B
import qualified Koshucode.Baala.Data       as C
import qualified Koshucode.Baala.Core       as C
import qualified Koshucode.Baala.Op.Message as Msg



-- ----------------------
-- $Operators
--
--  [@add-day@]    Add days to time.
--
--  [@add-week@]   Add weeks to time.
--
--  [@add-month@]  Add months to time.
--
--  [@add-year@]   Add years to time.
--
--  [@mjd@]        Modified Jurian Day of time.
--

copsTime :: (C.CTime c, C.CDec c) => [C.Cop c]
copsTime =
    [ C.CopCalc  (C.copNormal "add-day")    $ copTimeAdd B.timeAddDay
    , C.CopCalc  (C.copNormal "add-week")   $ copTimeAdd B.timeAddWeek
    , C.CopCalc  (C.copNormal "add-month")  $ copTimeAdd B.timeAddMonth
    , C.CopCalc  (C.copNormal "add-year")   $ copTimeAdd B.timeAddYear
    , C.CopCalc  (C.copNormal "mjd")        copMjd
    , C.CopCalc  (C.copNormal "weekly")     $ copDateForm B.weekly
    , C.CopCalc  (C.copNormal "monthly")    $ copDateForm B.monthly
    , C.CopCalc  (C.copNormal "yearly")     $ copDateForm B.yearly
    ]

copTimeAdd :: (C.CTime c, C.CDec c) => (Integer -> B.Map B.Time) -> C.CopCalc c
copTimeAdd add [Right c1, Right c2]
    | C.isDec c1 && C.isTime c2 = addc c1 c2
    | C.isDec c2 && C.isTime c1 = addc c2 c1
    where addc nc tc = let n = toInteger $ B.decimalNum $ C.gDec nc
                           t = C.gTime tc
                       in Right $ C.pTime $ add n t
copTimeAdd _ _ = Msg.unexpAttr "add-time"

copMjd :: (C.CTime c, C.CDec c) => C.CopCalc c
copMjd [Right c] | C.isTime c = Right $ C.pInteger $ B.timeMjd $ C.gTime c
copMjd _ = Msg.unexpAttr "mjd"

copDateForm :: (C.CTime c) => B.Map B.Date -> C.CopCalc c
copDateForm f [Right c] | C.isTime c = C.putTime $ B.timeMapDate f $ C.gTime c
copDateForm _ _ = Msg.unexpAttr "date"

