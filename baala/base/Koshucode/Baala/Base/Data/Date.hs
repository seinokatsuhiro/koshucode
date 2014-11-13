{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Data.Date
  ( Date (..),
    Year, Month, Week, Day,
    dateFromYmdAb, dateFromYwdAb, dateFromYdAb,
    dateDay, dateMapDay,
    monthly, weekly, yearly,
  ) where

import qualified Data.Time.Calendar               as T
import qualified Data.Time.Calendar.WeekDate      as T
import qualified Data.Time.Calendar.OrdinalDate   as T
import qualified Koshucode.Baala.Base.Abort       as B
import qualified Koshucode.Baala.Base.Prelude     as B
import qualified Koshucode.Baala.Base.Text        as B
import qualified Koshucode.Baala.Base.Message     as Msg


-- ----------------------  Date

data Date
    = Monthly T.Day    -- ^ Date in /YYYY-MM-DD/
    | Weekly  T.Day    -- ^ Date in /YYYY-#W-D/
    | Yearly  T.Day    -- ^ Date in /YYYY-##D/
      deriving (Show)

instance Eq  Date where  a == b         = dateDay a == dateDay b
instance Ord Date where  a `compare` b  = dateDay a `compare` dateDay b

type Year   = Integer
type Week   = Int
type Month  = Int
type Day    = Int

-- | Create date from year, month, and day.
dateFromYmdAb :: Year -> Month -> Day -> B.Ab Date
dateFromYmdAb y m d =
    case T.fromGregorianValid y m d of
      Just day -> Right $ Monthly day
      Nothing  -> Msg.notDate y m d

-- | Create date from year, week, and day.
dateFromYwdAb :: Year -> Week -> Day -> B.Ab Date
dateFromYwdAb y w d =
    case T.fromWeekDateValid y w d of
      Just day -> Right $ Weekly day
      Nothing  -> Msg.notDate y w d

-- | Create date from year and day.
dateFromYdAb :: Year -> Day -> B.Ab Date
dateFromYdAb y d =
    case T.fromOrdinalDateValid y d of
      Just day -> Right $ Yearly day
      Nothing  -> Msg.notDate y 0 d


-- ----------------------  Write

instance B.Write Date where
    write _ = writeDate

writeDate :: Date -> B.Doc
writeDate date =
    case date of
      Monthly d   -> dateMonth $ T.toGregorian    d
      Weekly  d   -> dateWeek  $ T.toWeekDate     d
      Yearly  d   -> dateYear  $ T.toOrdinalDate  d
    where
      dateMonth (y, m, d)  = B.doc y `hy`   B.doc02 m `hy` B.doc02 d
      dateWeek  (y, w, d)  = B.doc y `hyw`  B.doc w   `hy` B.doc d
      dateYear  (y, d)     = B.doc y `hyww` B.doc d

hy, hyw, hyww :: B.Bin B.Doc
hy    = B.docConcat "-"
hyw   = B.docConcat "-#"
hyww  = B.docConcat "-##"


-- ----------------------  Utility

dateDay :: Date -> T.Day
dateDay (Monthly d)  = d
dateDay (Weekly  d)  = d
dateDay (Yearly  d)  = d

dateMapDay :: B.Map T.Day -> B.Map Date
dateMapDay f (Monthly d)  = Monthly $ f d
dateMapDay f (Weekly  d)  = Weekly  $ f d
dateMapDay f (Yearly  d)  = Yearly  $ f d

monthly :: B.Map Date
monthly = Monthly . dateDay

weekly :: B.Map Date
weekly  = Weekly . dateDay

yearly :: B.Map Date
yearly  = Yearly . dateDay
