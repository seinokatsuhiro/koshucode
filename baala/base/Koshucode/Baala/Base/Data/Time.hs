{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Data.Time
  ( Time (..),
    timeFromYMD,
    timeMapMJD,
    timeAddDay,
    timeTruncateDay, timeTruncateMonth,
    timeNextMonth, timeNextYear,
  ) where

import qualified Data.Time.Calendar as T
import qualified Koshucode.Baala.Base.Prelude  as B
import qualified Koshucode.Baala.Base.Text     as B

data Time
    = TimeYMD T.Day
    | TimeYM T.Day
      deriving (Show, Eq, Ord)

timeFromYMD :: Integer -> Int -> Int -> Maybe Time
timeFromYMD y m d = fmap TimeYMD $ T.fromGregorianValid y m d

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

timeMapMJD :: B.Map Integer -> B.Map Time
timeMapMJD f time = timeMapDay g time where
    g (T.ModifiedJulianDay d) = T.ModifiedJulianDay $ f d

timeMapDay :: B.Map T.Day -> B.Map Time
timeMapDay f (TimeYMD day) = TimeYMD $ f day
timeMapDay f (TimeYM  day) = TimeYM  $ f day

timeAddDay :: Integer -> B.Map Time
timeAddDay d = timeMapMJD (+ d)

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
