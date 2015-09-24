{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Writer.Csv
  ( resultCsv,
  ) where

import qualified System.IO                 as IO
import qualified Text.CSV                  as CSV
import qualified Koshucode.Baala.Base      as B
import qualified Koshucode.Baala.Core      as C

resultCsv :: (B.Write c) => C.ResultWriter c
resultCsv = C.ResultWriterJudge "csv" hPutCsv

hPutCsv :: (B.Write c) => C.ResultWriterJudge c
hPutCsv h _ status js =
    do let csvLines = map (toCSV . B.textualjudge id) js
       IO.hPutStrLn h $ CSV.printCSV csvLines
       return status
    where
      toCSV (B.JudgeAffirm p xs) = p : map snd xs
      toCSV _ = []
