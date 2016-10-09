{-# OPTIONS_GHC -Wall #-}

-- | Content operators on clocks and times.

module Koshucode.Baala.Cop.Time
  ( copsTime
    -- $Operators
  ) where

import qualified Koshucode.Baala.Base        as B
import qualified Koshucode.Baala.Data        as D
import qualified Koshucode.Baala.Cop.Message as Msg



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

-- | Content operators on clocks and times.
copsTime :: (D.CTime c, D.CDec c) => [D.Cop c]
copsTime =
    [ D.CopCalc  (D.copNormal "add-day")    $ copTimeAdd D.timeAddDay
    , D.CopCalc  (D.copNormal "add-week")   $ copTimeAdd D.timeAddWeek
    , D.CopCalc  (D.copNormal "add-month")  $ copTimeAdd D.timeAddMonth
    , D.CopCalc  (D.copNormal "add-year")   $ copTimeAdd D.timeAddYear
    , D.CopCalc  (D.copNormal "mjd")        copMjd
    , D.CopCalc  (D.copNormal "weekly")     $ copDateForm D.weekly
    , D.CopCalc  (D.copNormal "monthly")    $ copDateForm D.monthly
    , D.CopCalc  (D.copNormal "yearly")     $ copDateForm D.yearly
    ]

copTimeAdd :: (D.CTime c, D.CDec c) => (Integer -> B.Map D.Time) -> D.CopCalc c
copTimeAdd add [Right c1, Right c2]
    | D.isDec c1 && D.isTime c2 = addc c1 c2
    | D.isDec c2 && D.isTime c1 = addc c2 c1
    where addc nc tc = let n = toInteger $ D.decimalNum $ D.gDec nc
                           t = D.gTime tc
                       in Right $ D.pTime $ add n t
copTimeAdd _ _ = Msg.unexpAttr "add-time"

copMjd :: (D.CTime c, D.CDec c) => D.CopCalc c
copMjd [Right c] | D.isTime c = Right $ D.pInteger $ D.timeMjd $ D.gTime c
copMjd _ = Msg.unexpAttr "mjd"

copDateForm :: (D.CTime c) => B.Map D.Date -> D.CopCalc c
copDateForm f [Right c] | D.isTime c = D.putTime $ D.timeMapDate f $ D.gTime c
copDateForm _ _ = Msg.unexpAttr "date"

