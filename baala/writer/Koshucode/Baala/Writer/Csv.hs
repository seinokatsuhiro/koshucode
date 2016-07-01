{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Writer.Csv
  ( resultCsv,
    resultCsvHeading,
    resultTsvHeading,
  ) where

import qualified Data.Char                 as Char
import qualified Koshucode.Baala.Base      as B
import qualified Koshucode.Baala.Syntax    as S
import qualified Koshucode.Baala.Data      as D
import qualified Koshucode.Baala.Core      as C


-- --------------------------------------------  Writer

-- | Comman-separated-values output without heading.
resultCsv :: (D.CContent c) => C.ResultWriter c
resultCsv = C.ResultWriterChunk "csv" $ hPutXsv csvSetting

-- | Comman-separated-values output with heading.
resultCsvHeading :: (D.CContent c) => C.ResultWriter c
resultCsvHeading = C.ResultWriterChunk "csv-heading" $ hPutXsv csvHeadSetting

-- | Tab-separated-values output with heading.
resultTsvHeading :: (D.CContent c) => C.ResultWriter c
resultTsvHeading = C.ResultWriterChunk "tab-heading" $ hPutXsv tabHeadSetting

hPutXsv :: forall c. (D.CContent c) => XsvSetting -> C.ResultWriterChunk c
hPutXsv setting@XsvSetting { xsvHead = isHead, xsvSep = sep, xsvQuote = quote } h _ status sh =
    do let csv = concatMap toCsv chunks
       B.hPutMixLines B.crlfBreak h $ map B.mix csv
       return status
    where
      chunks = concatMap S.shortBody sh

      toCsv :: C.ResultChunk c -> [String]
      toCsv (C.ResultRel _ (D.Rel he bo)) = appendHead he $ map line bo
      toCsv _ = []

      join :: [String] -> String
      join = B.intercalate sep

      line :: [c] -> String
      line = join . map (csvContent setting)

      appendHead he xs
          | isHead    = join (map quote $ D.headNames he) : xs
          | otherwise = xs

csvContent :: (D.CContent c) => XsvSetting -> c -> String
csvContent setting@XsvSetting { xsvQuote = quote } c
    | D.isCode   c  = quote $ D.gCode c
    | D.isText   c  = quote $ D.gText c
    | D.isTerm   c  = quote $ '/' : D.gTerm c
    | D.isDec    c  = D.encodeDecimalCompact $ D.gDec c
    | D.isClock  c  = quote $ B.mixToFlatString $ B.mixEncode $ D.gClock c
    | D.isTime   c  = quote $ B.mixToFlatString $ B.mixEncode $ D.gTime c
    | D.isBool   c  = boolToString $ D.gBool c

    | D.isList   c  = csvSubContents setting $ D.gList c
    | D.isSet    c  = csvSubContents setting $ D.gSet c 
    | otherwise     = D.contString c

csvSubContents :: (D.CContent c) => XsvSetting -> [c] -> String
csvSubContents setting@XsvSetting { xsvSubSep = sub } cs =
    B.intercalate sub $ csvContent setting <$> cs

boolToString :: Bool -> String
boolToString True  = "true"
boolToString False = "false"

-- --------------------------------------------  Setting

data XsvSetting = XsvSetting
    { xsvHead     :: Bool
    , xsvSep      :: String
    , xsvSubSep   :: String
    , xsvQuote  :: String -> String
    }

csvSetting :: XsvSetting
csvSetting = XsvSetting { xsvHead    = False
                        , xsvSep     = ","
                        , xsvSubSep  = "|"
                        , xsvQuote   = enquote }

tabSetting :: XsvSetting
tabSetting = XsvSetting { xsvHead    = False
                        , xsvSep     = "\t"
                        , xsvSubSep  = "|"
                        , xsvQuote  = toSpace }

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

