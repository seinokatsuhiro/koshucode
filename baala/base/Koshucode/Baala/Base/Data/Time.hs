{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Data.Time
  ( Time (..),
    timeFromYMD,
    timeMJD,
    timeMapMJD,
    timeTruncateDay, timeTruncateMonth,
    timeNextMonth, timeNextYear,
    -- * Add
    timeAddDay, timeAddWeek, timeAddMonth, timeAddYear,
  ) where

import qualified Data.Time.Calendar            as T
import qualified Koshucode.Baala.Base.Abort    as B
import qualified Koshucode.Baala.Base.Prelude  as B
import qualified Koshucode.Baala.Base.Text     as B
import qualified Koshucode.Baala.Base.Message  as Msg

data Time
    = TimeYMD T.Day
    | TimeYM T.Day
      deriving (Show, Eq, Ord)

timeFromYMD :: Integer -> Int -> Int -> B.Ab Time
timeFromYMD y m d =
    case T.fromGregorianValid y m d of
      Just day -> Right $ TimeYMD day
      Nothing  -> Msg.notDate y m d

timeFromYMD' :: Integer -> Int -> Int -> Time
timeFromYMD' y m d = TimeYMD $ T.fromGregorian y m d

instance B.Write Time where
    write _ = writeTime

writeTime :: Time -> B.Doc
writeTime (TimeYMD day) = writeDay day
writeTime (TimeYM day)  = writeDay day

writeDay :: T.Day -> B.Doc
writeDay = B.doc . show

timeDay :: Time -> T.Day
timeDay (TimeYMD day) = day
timeDay (TimeYM  day) = day

timeMJD :: Time -> Integer
timeMJD = T.toModifiedJulianDay . timeDay

timeMapMJD :: B.Map Integer -> B.Map Time
timeMapMJD f time = timeMapDay g time where
    g (T.ModifiedJulianDay d) = T.ModifiedJulianDay $ f d

timeMapDay :: B.Map T.Day -> B.Map Time
timeMapDay f (TimeYMD day) = TimeYMD $ f day
timeMapDay f (TimeYM  day) = TimeYM  $ f day

timeTruncateDay :: B.Map Time
timeTruncateDay time =
    let (y, m, _) = T.toGregorian $ timeDay time
    in timeFromYMD' y m 1

timeTruncateMonth :: B.Map Time
timeTruncateMonth time =
    let (y, _, _) = T.toGregorian $ timeDay time
    in timeFromYMD' y 1 1

timeNextMonth :: B.Map Time
timeNextMonth time =
    let (y, m, _) = T.toGregorian $ timeDay time
    in if m == 12
       then timeFromYMD' (y + 1) 1 1
       else timeFromYMD' y (m + 1) 1

timeNextYear :: B.Map Time
timeNextYear time =
    let (y, _, _) = T.toGregorian $ timeDay time
    in timeFromYMD' (y + 1) 1 1


-- ----------------------  Add

timeAddDay :: Integer -> B.Map Time
timeAddDay n = timeMapMJD (+ n)

timeAddWeek :: Integer -> B.Map Time
timeAddWeek n = timeAddDay (7 * n)

timeAddMonth :: Integer -> B.Map Time
timeAddMonth n time =
    let (y, m, d) = T.toGregorian $ timeDay time
        (yd, m')  = (toInteger m + n) `divMod` 12
        y'        = y + yd
    in timeFromYMD' y' (fromInteger m') d

timeAddYear :: Integer -> B.Map Time
timeAddYear n time =
    let (y, m, d) = T.toGregorian $ timeDay time
        y'        = y + n
    in timeFromYMD' y' m d
