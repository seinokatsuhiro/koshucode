{-# LANGUAGE PatternSynonyms #-}
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
    copDayw,
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
    , D.CopCalc  (D.copNormal "dayw")       $ copDayw
    ]

-- | Add day\/week\/month\/year to time.
--
--   >>> add-week ( 5 )( 2013-04-18 )
--   2013-05-23
--
copTimeAdd :: (D.CContent c) => (Integer -> O.Map D.Time) -> D.CopCalc c
copTimeAdd add [Right c1, Right c2]
    | D.isDec c1 && D.isTime c2 = addc c1 c2
    | D.isDec c2 && D.isTime c1 = addc c2 c1
    where addc nc tc = let n = toInteger $ D.decimalNum $ D.gDec nc
                           t = D.gTime tc
                       in Right $ D.pTime $ add n t
copTimeAdd _ xs = Msg.badArg xs

-- | Convert time to the modified Jurian day.
--
--   >>> mjd 2013-04-18
--   56400
--
copMjd :: (D.CContent c) => D.CopCalc c
copMjd [Right c] | D.isTime c = Right $ D.pInteger $ D.mjdInteger $ D.toMjd $ D.gTime c
copMjd xs = Msg.badArg xs

copDateForm :: (D.CContent c) => O.Map D.Date -> D.CopCalc c
copDateForm f [Right c] | D.isTime c = D.putTime $ D.timeAltDate f $ D.gTime c
copDateForm _ xs = Msg.badArg xs

-- | Create time from year, month, and day.
--
--   >>> time ( 2013 )( 4 )( 18 )
--   2013-04-18
--
copTime :: (D.CContent c) => D.CopCalc c
copTime = arg2 where
    arg2         ((i -> Just y) : (i -> Just m) : cs) = arg3 y m cs
    arg2         cs                    = Msg.badArg cs

    arg3 y m     ((i -> Just d) : cs)  = arg4 (D.dateYmd y m d) cs
    arg3 y m     []                    = D.pTime <$> D.monthlyTimeDate (D.dateYmd y m 1)
    arg3 _ _     cs                    = Msg.badArg cs

    arg4 day     ((i -> Just h) : cs)  = arg5 day h cs
    arg4 day     []                    = D.pTime <$> D.monthlyTimeDate day
    arg4 _       cs                    = Msg.badArg cs

    arg5 day h   ((i -> Just m) : cs)  = arg6 day h m cs
    arg5 day h   []                    = D.pTime <$> D.monthlyTimeClock day (D.clockDh 0 h)
    arg5 _ _     cs                    = Msg.badArg cs

    arg6 day h m [i -> Just s]         = D.pTime <$> D.monthlyTimeClock day (D.clockDhms 0 h m s)
    arg6 day h m []                    = D.pTime <$> D.monthlyTimeClock day (D.clockDhm  0 h m)
    arg6 _ _ _   cs                    = Msg.badArg cs

-- | Create clock from day, hour, minute, and second.
--
--   >>> clock ( 0 )( 12 )( 45 )( 30 )
--   |12:45:30|
--
copClock :: (D.CContent c) => D.CopCalc c
copClock = arg1 where
    arg1       ((i -> Just d) : cs)  = arg2 d cs
    arg1       cs                    = Msg.badArg cs

    arg2 d     ((i -> Just h) : cs)  = arg3 d h cs
    arg2 d     []                    = D.putClock $ D.clockFromD d
    arg2 _     cs                    = Msg.badArg cs

    arg3 d h   ((i -> Just m) : cs)  = arg4 d h m cs
    arg3 d h   []                    = D.putClock $ D.clockFromDh d h
    arg3 _ _   cs                    = Msg.badArg cs

    arg4 d h m [i -> Just s]         = D.putClock $ D.clockFromDhms d h m s
    arg4 d h m []                    = D.putClock $ D.clockFromDhm  d h m
    arg4 _ _ _ cs                    = Msg.badArg cs

i :: (D.CDec c, Integral n) => B.Ab c -> Maybe n
i (Right c) | D.isDec c  = Just $ truncate $ toRational $ D.gDec c
i _ = Nothing

-- | Day of week.
--
--   >>> dayw 2013-04-18
--   4
--
copDayw :: (D.CContent c) => D.CopCalc c
copDayw [D.getTime -> Right t] = Right $ daywContent $ D.mjdDayw t
copDayw cs = Msg.badArg cs

daywContent :: (D.CDec c) => D.Dayw -> c
daywContent = D.pInt . fromEnum
