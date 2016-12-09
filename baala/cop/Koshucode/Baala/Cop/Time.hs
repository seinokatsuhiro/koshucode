{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

-- | Content operators on clocks and times.

module Koshucode.Baala.Cop.Time
  ( -- * Operators
    copsTime,
    -- * Implementations
    copTimeAdd,
    copMjd,
    copTime,
    copClock,
  ) where

import qualified Koshucode.Baala.Overture     as O
import qualified Koshucode.Baala.Base         as B
import qualified Koshucode.Baala.Data         as D
import qualified Koshucode.Baala.Cop.Message  as Msg


-- | Content operators on clocks and times.
copsTime :: (D.CContent c) => [D.Cop c]
copsTime =
    [ D.CopCalc  (D.copNormal "add-day")    $ copTimeAdd D.timeAddDay
    , D.CopCalc  (D.copNormal "add-week")   $ copTimeAdd D.timeAddWeek
    , D.CopCalc  (D.copNormal "add-month")  $ copTimeAdd D.timeAddMonth
    , D.CopCalc  (D.copNormal "add-year")   $ copTimeAdd D.timeAddYear
    , D.CopCalc  (D.copNormal "clock")      copClock
    , D.CopCalc  (D.copNormal "mjd")        copMjd
    , D.CopCalc  (D.copNormal "time")       copTime
    , D.CopCalc  (D.copNormal "weekly")     $ copDateForm D.weekly
    , D.CopCalc  (D.copNormal "monthly")    $ copDateForm D.monthly
    , D.CopCalc  (D.copNormal "yearly")     $ copDateForm D.yearly
    ]

-- | Add day\/week\/month\/year to time.
--
--   >>> add-week ( 5 )( 2013-04-18 )
--   2013-05-23
--
copTimeAdd :: (D.CTime c, D.CDec c) => (Integer -> O.Map D.Time) -> D.CopCalc c
copTimeAdd add [Right c1, Right c2]
    | D.isDec c1 && D.isTime c2 = addc c1 c2
    | D.isDec c2 && D.isTime c1 = addc c2 c1
    where addc nc tc = let n = toInteger $ D.decimalNum $ D.gDec nc
                           t = D.gTime tc
                       in Right $ D.pTime $ add n t
copTimeAdd _ _ = Msg.unexpAttr "add-time"

-- | Convert time to the modified Jurian day.
--
--   >>> mjd 2013-04-18
--   56400
--
copMjd :: (D.CTime c, D.CDec c) => D.CopCalc c
copMjd [Right c] | D.isTime c = Right $ D.pInteger $ D.timeMjd $ D.gTime c
copMjd _ = Msg.unexpAttr "mjd"

copDateForm :: (D.CTime c) => O.Map D.Date -> D.CopCalc c
copDateForm f [Right c] | D.isTime c = D.putTime $ D.timeAltDate f $ D.gTime c
copDateForm _ _ = Msg.unexpAttr "date"

-- | Create time from year, month, and day.
--
--   >>> time ( 2013 )( 4 )( 18 )
--   2013-04-18
--
copTime :: (D.CContent c) => D.CopCalc c
copTime [i -> Just y, i -> Just m] = D.putTime =<< D.timeFromYmAb y m
copTime [i -> Just y, i -> Just m, i -> Just d] = D.putTime $ D.timeFromYmd y m d
copTime cs = Msg.badArg cs

-- | Create clock from day, hour, minute, and second.
--
--   >>> clock ( 0 )( 12 )( 45 )( 30 )
--   |12:45:30|
--
copClock :: (D.CContent c) => D.CopCalc c
copClock [i -> Just d] =
    D.putClock $ D.clockFromD d
copClock [i -> Just d, i -> Just h] =
    D.putClock $ D.clockFromDh d h
copClock [i -> Just d, i -> Just h, i -> Just m] =
    D.putClock $ D.clockFromDhm d h m
copClock [i -> Just d, i -> Just h, i -> Just m, i -> Just s] =
    D.putClock $ D.clockFromDhms d h m s
copClock cs = Msg.badArg cs

i :: (D.CDec c, Integral n) => B.Ab c -> Maybe n
i (Right c) | D.isDec c  = Just $ truncate $ toRational $ D.gDec c
i _ = Nothing

