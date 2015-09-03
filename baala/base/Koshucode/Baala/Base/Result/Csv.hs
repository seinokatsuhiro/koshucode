{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Result.Csv
  ( resultCsv,
  ) where

import qualified System.IO                           as IO
import qualified Text.CSV                            as CSV
import qualified Koshucode.Baala.Base.Data           as B
import qualified Koshucode.Baala.Base.Text           as B
import qualified Koshucode.Baala.Base.Result.Result  as B

resultCsv :: (B.Write c) => B.ResultWriter c
resultCsv = B.ResultWriterJudge "csv" hPutCsv

hPutCsv :: (B.Write c) => B.ResultWriterJudge c
hPutCsv h _ status js =
    do let csvLines = map (toCSV . B.textualjudge id) js
       IO.hPutStrLn h $ CSV.printCSV csvLines
       return status
    where
      toCSV (B.JudgeAffirm p xs) = p : map snd xs
      toCSV _ = []

