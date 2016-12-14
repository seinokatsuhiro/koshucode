{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

-- | Content operators on clocks and times.

module Koshucode.Baala.Cop.Time
  ( -- * Operators
    copsTime,

    -- * Implementations
    copTimeAdd,
    copDateForm,
    copMjd,
    copTime,
    copClock,
    copDayw,
  ) where

import qualified Koshucode.Baala.Overture     as O
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
--   >>> add-month ( 1 )( 2013-04-18 )
--   2013-05-18
--
copTimeAdd :: (D.CContent c) => (Integer -> O.Map D.Time) -> D.CopCalc c
copTimeAdd add [D.getIntegral -> Right n, D.getTime -> Right t]
    = D.putTime $ add n t
copTimeAdd _ cs = Msg.unexpArg cs ["integer time"]

-- | Convert time to the modified Jurian day.
--
--   >>> mjd 2013-04-18
--   56400
--
copMjd :: (D.CContent c) => D.CopCalc c
copMjd [(D.getTime -> Right t)] = Right $ D.pInteger $ D.mjdInteger $ D.toMjd t
copMjd cs = Msg.unexpArg cs ["time"]

-- | Convert format of date.
--
--   >>> weekly 2013-04-18
--   2013-#16-4
--
copDateForm :: (D.CContent c) => O.Map D.Date -> D.CopCalc c
copDateForm f [D.getTime -> Right t] = D.putTime $ D.timeAltDate f t
copDateForm _ cs = Msg.unexpArg cs ["time"]

-- | Create time from year, month, and day.
--
--   >>> time ( 2013 )( 4 )( 18 )
--   2013-04-18
--
copTime :: (D.CContent c) => D.CopCalc c
copTime cs0 = arg1 cs0 where
    arg1         ((D.getIntegral -> Right y) : cs)
                      = arg2 y cs
    arg1         _    = bad

    arg2 y       ((D.getIntegral -> Right m) : cs)
                      = arg3 y m cs
    arg2 _       _    = bad

    arg3 y m     ((D.getIntegral -> Right d) : cs)
                      = arg4 (D.dateYmd y m d) cs
    arg3 y m     []   = D.pTime <$> D.monthlyTimeDate (D.dateYmd y m 1)
    arg3 _ _     _    = bad

    arg4 day     ((D.getIntegral -> Right h) : cs)
                      = arg5 day h cs
    arg4 day     []   = D.pTime <$> D.monthlyTimeDate day
    arg4 _       _    = bad

    arg5 day h   ((D.getIntegral -> Right m) : cs)
                      = arg6 day h m cs
    arg5 day h   []   = D.pTime <$> D.monthlyTimeClock day (D.clockDh 0 h)
    arg5 _ _     _    = bad

    arg6 day h m [(D.getIntegral -> Right s)]
                      = D.pTime <$> D.monthlyTimeClock day (D.clockDhms 0 h m s)
    arg6 day h m []   = D.pTime <$> D.monthlyTimeClock day (D.clockDhm  0 h m)
    arg6 _ _ _   _    = bad

    bad = Msg.unexpArg cs0 ["year month? day? hour? min? sec?"]

-- | Create clock from day, hour, minute, and second.
--
--   >>> clock ( 0 )( 12 )( 45 )( 30 )
--   |12:45:30|
--
copClock :: (D.CContent c) => D.CopCalc c
copClock cs0 = arg1 cs0 where
    arg1       ((D.getIntegral -> Right d) : cs)
                     = arg2 d cs
    arg1       []    = D.putClock $ D.clockFromD 0
    arg1       _     = bad

    arg2 d     ((D.getIntegral -> Right h) : cs)
                     = arg3 d h cs
    arg2 d     []    = D.putClock $ D.clockFromD d
    arg2 _     _     = bad

    arg3 d h   ((D.getIntegral -> Right m) : cs)
                     = arg4 d h m cs
    arg3 d h   []    = D.putClock $ D.clockFromDh d h
    arg3 _ _   _     = bad

    arg4 d h m [(D.getIntegral -> Right s)]
                     = D.putClock $ D.clockFromDhms d h m s
    arg4 d h m []    = D.putClock $ D.clockFromDhm  d h m
    arg4 _ _ _ _     = bad

    bad = Msg.unexpArg cs0 ["day? hour? min? sec?"]

-- | Day of week.
--
--   >>> dayw 2013-04-18
--   4
--
copDayw :: (D.CContent c) => D.CopCalc c
copDayw [D.getTime -> Right t] = Right $ daywContent $ D.mjdDayw t
copDayw cs = Msg.unexpArg cs ["time"]

daywContent :: (D.CDec c) => D.Dayw -> c
daywContent = D.pInt . fromEnum
