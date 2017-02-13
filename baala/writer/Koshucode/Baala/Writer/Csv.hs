{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

-- | CSV output.

module Koshucode.Baala.Writer.Csv
  ( resultCsv,
    resultCsvHeading,
    resultTsvHeading,
  ) where

import qualified Data.Char                 as Ch
import qualified Koshucode.Baala.Overture  as O
import qualified Koshucode.Baala.Base      as B
import qualified Koshucode.Baala.Syntax    as S
import qualified Koshucode.Baala.Type      as T
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
       B.hPutMixLines B.crlfBreak h csv
       return status
    where
      chunks = concatMap S.shortBody sh

      toCsv :: C.ResultChunk c -> [B.MixText]
      toCsv (C.ResultRel _ (T.Rel he bo)) = appendHead he (line <$> bo)
      toCsv _ = []

      join :: [B.MixText] -> B.MixText
      join = B.mixJoin $ B.mix sep

      line :: [c] -> B.MixText
      line = join . map (csvContent setting)

      appendHead he xs
          | isHead    = join (mixName <$> T.getTermNames he) : xs
          | otherwise = xs
      mixName = (B.mix . quote . S.stringChars . S.termNameContent)

csvContent :: (D.CContent c) => XsvSetting -> c -> B.MixText
csvContent setting@XsvSetting { xsvQuote = quote } c
    | D.isCode   c  = B.mix $ quote $ D.gCode c
    | D.isText   c  = B.mix $ quote $ D.gText c
    | D.isTerm   c  = B.mix $ quote $ S.termNameChars $ D.gTerm c
    | D.isDec    c  = B.mix (T.encodeDecimalCompact $ D.gDec c :: S.Chars)
    | D.isClock  c  = B.mixEncode $ D.gClock c
    | D.isTime   c  = B.mixEncode $ D.gTime c
    | D.isBool   c  = boolToMix $ D.gBool c

    | D.isList   c  = csvSubs setting $ D.gList c
    | D.isSet    c  = csvSubs setting $ D.gSet c 
    | otherwise     = B.mix $ D.contentString c

csvSubs :: (D.CContent c) => XsvSetting -> [c] -> B.MixText
csvSubs setting@XsvSetting { xsvSubSep = sub } cs =
    B.mixJoin (B.mix sub) (csvContent setting <$> cs)

boolToMix :: Bool -> B.MixText
boolToMix True  = B.mixString "true"
boolToMix False = B.mixString "false"

-- --------------------------------------------  Setting

data XsvSetting = XsvSetting
    { xsvHead     :: Bool
    , xsvSep      :: String
    , xsvSubSep   :: String
    , xsvQuote    :: S.CharsMap
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

{-| Enclose text in double quotations.

    >>> putStrLn $ enquote "foo"
    "foo"

    >>> putStrLn $ enquote "foo\"bar"
    "foo""bar"
    -}
enquote :: (O.Textual t) => t -> t
enquote t = let t' = replace (== '"') "\"\"" t
            in "\"" O.++ t' O.++ "\""

{-| Convert control characters to space character.

    >>> putStrLn $ toSpace "foo"
    foo

    >>> putStrLn $ toSpace "foo\r\nbar"
    foo  bar
    -}
toSpace :: (O.Textual t) => t -> t
toSpace = replace Ch.isControl " "

{-| Replace character in text.

    >>> replace '.' "--" "foo.bar..baz."
    "foo--bar----baz--"
    -}
replace :: (O.Textual t) => O.Test Char -> t -> t -> t
replace from to = loop where
    loop t = case O.span (not . from) t of
               (a, b) | O.tIsEmpty b -> a
                      | O.tIsEmpty a -> rep b
                      | otherwise    -> a O.++ rep b

    rep (O.cut -> O.Jp c t)
        | from c      = to O.++ loop t
        | otherwise   = c O.<:> loop t
    rep t = t

