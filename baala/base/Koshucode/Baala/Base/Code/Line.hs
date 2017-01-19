{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Text lines delimited by carriage returns or line feeds.

module Koshucode.Baala.Base.Code.Line
  ( -- * Line
    LineNumber,
    NumberedLine,
    linesCrlf, linesCrlfNumbered,
    linesCrlfBz, linesCrlfBzNumbered,
    dropBom,

    -- * CodeLine
    CodeLine (..),
    IndentSize,
    lineIndentPair,
  ) where

import qualified Data.ByteString.Lazy                 as Bz
import qualified Koshucode.Baala.Base.Abort           as B
import qualified Koshucode.Baala.Base.IO              as B
import qualified Koshucode.Baala.Base.Prelude         as B


-- ----------------------  Line

-- | Line number.
type LineNumber = Int

-- | Line with number.
type NumberedLine = (LineNumber, String)

-- | Split string into lines.
--   The result strings do not contain
--   carriage returns (@\\r@)
--   and line feeds (@\\n@).
--
--   >>> linesCrlf "aaa\nbbb\r\nccc\n\nddd\n"
--   ["aaa","bbb","ccc","","ddd"]
linesCrlf :: String -> [String]
linesCrlf "" = []
linesCrlf s = ln : next s2 where
    (ln, s2) = break (`elem` ("\r\n" :: String)) s
    next ('\r' : s3) = next s3
    next ('\n' : s3) = linesCrlf s3
    next s3          = linesCrlf s3

-- | Line number and its content.
linesCrlfNumbered :: String -> [NumberedLine]
linesCrlfNumbered = zip [1..] . linesCrlf

-- | Split lazy bytestring by newline character sequence.
--   This function drops the BOM sequence.
linesCrlfBz :: (B.ToCode code) => code -> [B.Bz]
linesCrlfBz = linesCrlfBzRaw . dropBom

-- | Split lazy bytestring by newline character sequence.
linesCrlfBzRaw :: (B.ToCode code) => code -> [B.Bz]
linesCrlfBzRaw = loop . B.toCode where
    loop bz
        | Bz.null bz  = []
        | otherwise   = case Bz.break crlf bz of
                          (ln, bz') -> ln : loop (strip bz')
                          
    crlf n = (n == lf) || (n == cr)
    lf = 10 -- '\n'
    cr = 13 -- '\r'

    strip bz2 = case Bz.uncons bz2 of
                  Nothing -> bz2
                  Just (c , bz2')
                      | c == cr   -> strip bz2'
                      | c == lf   -> bz2'
                      | otherwise -> bz2

-- | Create numbered lines from lazy bytestring.
linesCrlfBzNumbered :: (B.ToCode code) => code -> [NumberedLine]
linesCrlfBzNumbered = zip [1..] . linesCrlfBzString

-- | Create string lines from lazy bytestring.
linesCrlfBzString :: (B.ToCode code) => code -> [String]
linesCrlfBzString = map B.bzString . linesCrlfBz

-- | Remove UTF-8 BOM (EF BB BF) from lazy bytestring.
dropBom :: (B.ToCode code) => code -> B.Bz
dropBom code =
    let bz = B.toCode code
    in case Bz.splitAt 3 bz of
         (bom, body) | "\xEF\xBB\xBF" `Bz.isPrefixOf` bom -> body
                     | otherwise                          -> bz


-- ----------------------  CodeLine

-- | Tokens in line.
data CodeLine k t = CodeLine
    { lineNumber  :: LineNumber    -- ^ Line number, from 1.
    , lineContent :: t             -- ^ Line content without newline.
    , lineTokens  :: [k t]         -- ^ Tokens in the line.
    } deriving (Show, Eq, Ord)

instance (B.GetCodePos (k t)) => B.GetCodePos (CodeLine k t) where
    getCPs (CodeLine _ _ ts) = B.getCPs $ head ts

-- | Type for indent size.
type IndentSize = Int

-- | Calculate indent of line and pairing it.
lineIndentPair :: (k t -> IndentSize) -> CodeLine k t -> (IndentSize, CodeLine k t)
lineIndentPair ind ln@(CodeLine _ _ (tk : _)) = (ind tk, ln)
lineIndentPair _   ln@(CodeLine _ _ [])       = (0, ln)

