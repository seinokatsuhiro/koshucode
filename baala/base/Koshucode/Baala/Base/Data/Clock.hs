{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Data.Clock
  ( Clock (..), DayCount, Hour, Min, Sec,
    hmsFromSec, secFromHms,
  ) where

import qualified Koshucode.Baala.Base.Prelude  as B
import qualified Koshucode.Baala.Base.Text     as B

data Clock
    = Clock DayCount Sec
      deriving (Show, Eq, Ord)

type DayCount = Integer
type Hour = Int
type Min  = Int
type Sec  = Int

instance B.Write Clock where
    write _ (Clock day sec) =
        let (d, doc) = secDoc sec
        in wrapBar $ case day + d of
                       0  -> doc
                       d2 -> B.doc d2 B.<> B.doc "'" B.<> doc

wrapBar :: B.Map B.Doc
wrapBar = B.docWrap "|" "|"

secDoc :: Sec -> (DayCount, B.Doc)
secDoc sec = (d, hms) where
    hms             = dd h B.<> colon B.<> dd m B.<> colon B.<> dd s
    (d, h, m, s)    = hmsFromSec sec
    colon           = B.doc ":"

    dd :: Int -> B.Doc
    dd n | n < 10    = B.doc $ '0' : show n
         | otherwise = B.doc n

secFromHms :: (Hour, Min, Sec) -> Sec
secFromHms (h, m, s) = (h * 60 + m) * 60 + s

hmsFromSec :: Sec -> (DayCount, Hour, Min, Sec)
hmsFromSec sec =
    let (m', s)   =  sec `divMod` 60
        (h', m)   =  m'  `divMod` 60
        (d,  h)   =  h'  `divMod` 24
    in (toInteger d, h, m, s)

