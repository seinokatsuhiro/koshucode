{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

-- | Text lines delimited by carriage returns or line feeds.

module Koshucode.Baala.Base.Code.Line
  ( -- * Line
    LineNumber,
    NumberedLine,
    linesCrlf, linesCrlfNumbered,
    linesCrlfBz, linesCrlfBzNumbered,
    bzLines,
    dropBom,

    -- * CodeLine
    CodeLine (..),
    IndentSize,
    lineIndentPair,
  ) where

import qualified Data.ByteString.Lazy                 as Bz
import qualified Koshucode.Baala.Overture             as O
import qualified Koshucode.Baala.Base.Abort           as B
import qualified Koshucode.Baala.Base.IO              as B
import qualified Koshucode.Baala.Base.Prelude         as B


-- ----------------------  Line

-- | Line number.
type LineNumber = Int

-- | Line with number.
type NumberedLine t = (LineNumber, t)

-- | Split string into lines.
--   The result strings do not contain
--   carriage returns (@\\r@)
--   and line feeds (@\\n@).
--
--   >>> linesCrlf "aaa\nbbb\r\nccc\n\nddd\n"
--   ["aaa","bbb","ccc","","ddd"]
--
linesCrlf :: String -> [String]
linesCrlf s
    | O.tIsEmpty s = []
    | otherwise = ln : next s2
    where
      (ln, s2) = break (`elem` ("\r\n" :: String)) s
      next (O.tCut -> O.Jp '\r' s3) = next s3
      next (O.tCut -> O.Jp '\n' s3) = linesCrlf s3
      next s3                       = linesCrlf s3

-- | Line number and its content.
linesCrlfNumbered :: String -> [NumberedLine String]
linesCrlfNumbered = zip [1..] . linesCrlf

-- | Create numbered lines from lazy bytestring.
linesCrlfBzNumbered :: (B.ToCode code) => code -> [NumberedLine String]
linesCrlfBzNumbered = zip [1..] . linesCrlfBzString

-- | Create string lines from lazy bytestring.
linesCrlfBzString :: (B.ToCode code) => code -> [String]
linesCrlfBzString = map B.bzString . linesCrlfBz

-- | Split lazy bytestring by newline character sequence.
--   This function drops the BOM sequence.
linesCrlfBz :: (B.ToCode code) => code -> [O.Bz]
linesCrlfBz = linesCrlfBzRaw . dropBom . B.toCode

-- | Split lazy bytestring by newline character sequence.
linesCrlfBzRaw :: (B.ToCode code) => code -> [O.Bz]
linesCrlfBzRaw = bzLines . B.toCode

-- | Split lazy bytestring by newline character sequence.
bzLines :: O.Bz -> [O.Bz]
bzLines = loop where
    loop bz | Bz.null bz  = []
            | otherwise   = case Bz.break crlf bz of
                              (ln, bz') -> ln : loop (strip bz')
                          
    crlf n = (n == lf) || (n == cr)
    lf     = 10 -- '\n'
    cr     = 13 -- '\r'

    strip bz2@(Bz.uncons -> Just (c , bz2'))
               | c == cr   = strip bz2'
               | c == lf   = bz2'
               | otherwise = bz2
    strip bz2 = bz2
                  
-- | Remove UTF-8 BOM (EF BB BF in hexadecimal) from lazy bytestring.
--
--   >>> dropBom "\xEF\xBB\xBF|foo bar baz"
--   "|foo bar baz"
--
--   >>> dropBom "\xEF|foo bar baz"
--   "\239|foo bar baz"
--
dropBom :: O.Bz -> O.Bz
dropBom bz = case Bz.splitAt 3 bz of
               (bom, body) | "\xEF\xBB\xBF" == bom -> body
                           | otherwise             -> bz


-- ----------------------  CodeLine

-- | Tokens in line.
data CodeLine k t = CodeLine
    { lineNumber  :: LineNumber    -- ^ Line number, from 1.
    , lineContent :: t             -- ^ Line content without newline.
    , lineTokens  :: [k t]         -- ^ Tokens in the line.
    } deriving (Show, Eq, Ord)

instance (B.GetCodePos (k t)) => B.GetCodePos (CodeLine k t) where
    getCPs (CodeLine _ _ toks) = B.getCPs toks

-- | Type for indent size.
type IndentSize = Int

-- | Calculate indent of line and pairing it.
lineIndentPair :: (k t -> IndentSize) -> CodeLine k t -> (IndentSize, CodeLine k t)
lineIndentPair ind ln@(CodeLine _ _ (tk : _)) = (ind tk, ln)
lineIndentPair _   ln@(CodeLine _ _ [])       = (0, ln)

