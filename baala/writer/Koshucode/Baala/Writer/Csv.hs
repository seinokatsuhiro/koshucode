{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Writer.Csv
  ( resultCsv,
    resultCsvHeading,
  ) where

import qualified System.IO                 as IO
import qualified Text.CSV                  as CSV
import qualified Koshucode.Baala.Base      as B
import qualified Koshucode.Baala.Data      as D
import qualified Koshucode.Baala.Core      as C

resultCsv :: (B.Write c) => C.ResultWriter c
resultCsv = C.ResultWriterJudge "csv" hPutCsv

hPutCsv :: (B.Write c) => C.ResultWriterJudge c
hPutCsv h _ status js =
    do let csvLines = map (toCSV . D.textualjudge id) js
       IO.hPutStrLn h $ CSV.printCSV csvLines
       return status
    where
      toCSV (D.JudgeAffirm p xs) = p : map snd xs
      toCSV _ = []

resultCsvHeading :: (D.CContent c, B.Write c) => C.ResultWriter c
resultCsvHeading = C.ResultWriterChunk "csv-heading" hPutCsvHeading

hPutCsvHeading :: forall c. (D.CContent c, B.Write c) => C.ResultWriterChunk c
hPutCsvHeading h _ status sh =
    do let csv = concatMap toCsv chunks
       IO.hPutStr h $ unlines csv
       return status
    where
      chunks = concatMap D.shortBody sh

      toCsv :: C.ResultChunk c -> [String]
      toCsv (C.ResultRel _ (D.Rel he bo)) =
          comma (map enquote $ D.headNames he) : map line bo
      toCsv _ = []

      line :: [c] -> String
      line cs = comma $ map csvContent cs

      comma = B.intercalate ","

csvContent :: (D.CContent c, B.Write c) => c -> String
csvContent c
    | D.isText   c = enquote $ D.gText c
    | D.isDec    c = D.decimalStringCompact $ D.gDec c
    | D.isEmpty  c = ""
    | D.isBool   c = if D.gBool c then "true" else "false"
    | D.isClock  c = enquote $ show $ D.writeClockBody $ D.gClock c
    | D.isTime   c = enquote $ B.writeString c
    | D.isTerm   c = enquote $ '/' : D.gTerm c

    | D.isList   c = enquote "<list>"
    | D.isSet    c = enquote "<set>"
    | D.isAssn   c = enquote "<assn>"
    | D.isRel    c = enquote "<rel>"
    | D.isInterp c = enquote "<interp>"
    | D.isType   c = enquote "<type>"
    | otherwise    = enquote "<unknown>"

enquote :: String -> String
enquote str = '"' : q str where
    q ""          = ['"']
    q ('"' : xs)  = '"' : '"' : q xs
    q (x : xs)    =         x : q xs

