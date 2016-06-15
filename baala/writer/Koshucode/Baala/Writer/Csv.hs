{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Writer.Csv
  ( resultCsv,
    resultCsvHeading,
    resultTsvHeading,
  ) where

import qualified Data.Char                 as Char
import qualified System.IO                 as IO
import qualified Koshucode.Baala.Base      as B
import qualified Koshucode.Baala.Syntax    as S
import qualified Koshucode.Baala.Data      as D
import qualified Koshucode.Baala.Core      as C


-- --------------------------------------------  Writer

-- | Comman-separated-values output without heading.
resultCsv :: (D.CContent c) => C.ResultWriter c
resultCsv = C.ResultWriterChunk "csv" $ hPutXsv csvSetting

-- hPutCsv :: (D.CContent c) => C.ResultWriterJudge c
-- hPutCsv h _ status js =
--     do let csvLines = map (csvLine . D.judgeContString) js
--        IO.hPutStrLn h $ CSV.printCSV csvLines
--        return status
--     where
--       csvLine (D.JudgeAffirm _ xs) = map snd xs
--       csvLine _ = []

-- | Comman-separated-values output with heading.
resultCsvHeading :: (D.CContent c) => C.ResultWriter c
resultCsvHeading = C.ResultWriterChunk "csv-heading" $ hPutXsv csvHeadSetting

-- | Tab-separated-values output with heading.
resultTsvHeading :: (D.CContent c) => C.ResultWriter c
resultTsvHeading = C.ResultWriterChunk "tab-heading" $ hPutXsv tabHeadSetting

hPutXsv :: forall c. (D.CContent c) => XsvSetting -> C.ResultWriterChunk c
hPutXsv XsvSetting { xsvHead = isHead, xsvSep = sep, xsvQuote = quote } h _ status sh =
    do let csv = concatMap toCsv chunks
       IO.hPutStr h $ unlines csv
       return status
    where
      chunks = concatMap S.shortBody sh

      toCsv :: C.ResultChunk c -> [String]
      toCsv (C.ResultRel _ (D.Rel he bo)) = appendHead he $ map line bo
      toCsv _ = []

      join :: [String] -> String
      join = B.intercalate sep

      line :: [c] -> String
      line = join . map (csvContent quote)

      appendHead he xs
          | isHead    = join (map quote $ D.headNames he) : xs
          | otherwise = xs

csvContent :: (D.CContent c) => B.Map String -> c -> String
csvContent quote c
    | D.isText   c  = quote $ D.gText c
    | D.isDec    c  = D.encodeDecimalCompact $ D.gDec c
    | D.isEmpty  c  = ""
    | D.isBool   c  = if D.gBool c then "true" else "false"
    | D.isClock  c  = quote $ B.mixToFlatString $ B.mixEncode $ D.gClock c
    | D.isTime   c  = quote $ B.mixToFlatString $ B.mixEncode $ D.gTime c
    | D.isTerm   c  = quote $ '/' : D.gTerm c

    | D.isList   c  = "<list>"
    | D.isSet    c  = "<set>"
    | D.isTie    c  = "<tie>"
    | D.isRel    c  = "<rel>"
    | D.isInterp c  = "<interp>"
    | D.isType   c  = "<type>"
    | otherwise     = "<?>"


-- --------------------------------------------  Setting

data XsvSetting = XsvSetting
    { xsvHead   :: Bool
    , xsvSep    :: String
    , xsvQuote  :: String -> String
    }

csvSetting :: XsvSetting
csvSetting = XsvSetting { xsvHead  = False
                        , xsvSep   = ","
                        , xsvQuote = enquote }

tabSetting :: XsvSetting
tabSetting = XsvSetting { xsvHead  = False
                        , xsvSep   = "\t"
                        , xsvQuote = toSpace }

csvHeadSetting :: XsvSetting
csvHeadSetting = csvSetting { xsvHead = True }

tabHeadSetting :: XsvSetting
tabHeadSetting = tabSetting { xsvHead = True }

enquote :: B.Map String
enquote str = '"' : q str where
    q ""          = ['"']
    q ('"' : xs)  = '"' : '"' : q xs
    q (x : xs)    =         x : q xs

toSpace :: B.Map String
toSpace ""           = ""
toSpace (x : xs) | Char.isControl x  = ' ' : xs'
                 | otherwise         =   x : xs'
                 where xs' = toSpace xs

