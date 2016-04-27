{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Writer.Csv
  ( resultCsv,
    resultCsvHeading,
    resultTsvHeading,
  ) where

import qualified Data.Char                 as Char
import qualified System.IO                 as IO
import qualified Text.CSV                  as CSV
import qualified Koshucode.Baala.Base      as B
import qualified Koshucode.Baala.Syntax    as D
import qualified Koshucode.Baala.Data      as D
import qualified Koshucode.Baala.Core      as C

resultCsv :: (B.Write c) => C.ResultWriter c
resultCsv = C.ResultWriterJudge "csv" hPutCsv

hPutCsv :: (B.Write c) => C.ResultWriterJudge c
hPutCsv h _ status js =
    do let csvLines = map (toCSV . D.textualjudge B.nullShortener) js
       IO.hPutStrLn h $ CSV.printCSV csvLines
       return status
    where
      toCSV (D.JudgeAffirm p xs) = p : map snd xs
      toCSV _ = []

resultCsvHeading :: (D.CContent c, B.Write c) => C.ResultWriter c
resultCsvHeading = C.ResultWriterChunk "csv-heading" $ hPutXsvHeading "," enquote

resultTsvHeading :: (D.CContent c, B.Write c) => C.ResultWriter c
resultTsvHeading = C.ResultWriterChunk "tab-heading" $ hPutXsvHeading "\t" toSpace

hPutXsvHeading :: forall c. (D.CContent c, B.Write c)
    => String -> B.Map String -> C.ResultWriterChunk c
hPutXsvHeading delim text h _ status sh =
    do let csv = concatMap toCsv chunks
       IO.hPutStr h $ unlines csv
       return status
    where
      chunks = concatMap D.shortBody sh

      toCsv :: C.ResultChunk c -> [String]
      toCsv (C.ResultRel _ (D.Rel he bo)) =
          comma (map text $ D.headNames he) : map line bo
      toCsv _ = []

      line :: [c] -> String
      line cs = comma $ map (csvContent text) cs

      comma = B.intercalate delim

csvContent :: (D.CContent c, B.Write c) => B.Map String -> c -> String
csvContent text c
    | D.isText   c  = text $ D.gText c
    | D.isDec    c  = D.encodeDecimalCompact $ D.gDec c
    | D.isEmpty  c  = ""
    | D.isBool   c  = if D.gBool c then "true" else "false"
    | D.isClock  c  = text $ show $ D.writeClockBody $ D.gClock c
    | D.isTime   c  = text $ B.writeString c
    | D.isTerm   c  = text $ '/' : D.gTerm c

    | D.isList   c  = "<list>"
    | D.isSet    c  = "<set>"
    | D.isTie    c  = "<tie>"
    | D.isRel    c  = "<rel>"
    | D.isInterp c  = "<interp>"
    | D.isType   c  = "<type>"
    | otherwise     = "<?>"

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

