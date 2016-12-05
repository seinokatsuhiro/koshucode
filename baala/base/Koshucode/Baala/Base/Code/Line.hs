{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Text lines delimited by carriage returns or line feeds.

module Koshucode.Baala.Base.Code.Line
  ( -- * Line
    LineNumber,
    NumberedLine,
    linesCrlfNumbered,
    linesCrlf,
    linesCrlfBzNumbered,
    linesCrlfBz, linesCrlfBzString,
    dropBom,

    -- * CodeLine
    CodeLine (..),
    IndentSize,
    lineIndentPair,
  ) where

import qualified Data.ByteString.Lazy                 as Bz
import qualified Data.ByteString.Lazy.UTF8            as Bu
import qualified Koshucode.Baala.Base.Abort           as B
import qualified Koshucode.Baala.Base.IO              as B
import qualified Koshucode.Baala.Base.Prelude         as B


-- ----------------------  Line

-- | Line number.
type LineNumber = Int

-- | Line with number.
type NumberedLine = (LineNumber, String)

-- | Line number and its content.
linesCrlfNumbered :: String -> [NumberedLine]
linesCrlfNumbered = zip [1..] . linesCrlf

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

-- | Create numbered lines from lazy bytestring.
linesCrlfBzNumbered :: (B.ToCode code) => code -> [NumberedLine]
linesCrlfBzNumbered = zip [1..] . linesCrlfBzString

-- | Create string lines from lazy bytestring.
linesCrlfBzString :: (B.ToCode code) => code -> [String]
linesCrlfBzString = map Bu.toString . linesCrlfBz

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

-- | Remove UTF-8 BOM (EF BB BF) from lazy bytestring.
dropBom :: (B.ToCode code) => code -> B.Bz
dropBom code =
    let bz = B.toCode code
    in case Bz.splitAt 3 bz of
         (bom, body) | "\xEF\xBB\xBF" `Bz.isPrefixOf` bom -> body
                     | otherwise                          -> bz


-- ----------------------  CodeLine

-- | Tokens in line.
data CodeLine a = CodeLine
    { lineNumber  :: LineNumber    -- ^ Line number, from 1.
    , lineContent :: String        -- ^ Line content without newline.
    , lineTokens  :: [a]           -- ^ Tokens in the line.
    } deriving (Show, Eq, Ord)

instance (B.GetCodePos a) => B.GetCodePos (CodeLine a) where
    getCPs (CodeLine _ _ ts) = B.getCPs $ head ts

-- | Type for indent size.
type IndentSize = Int

-- | Calculate indent of line and pairing it.
lineIndentPair :: (a -> IndentSize) -> CodeLine a -> (IndentSize, CodeLine a)
lineIndentPair ind ln@(CodeLine _ _ (tk : _)) = (ind tk, ln)
lineIndentPair _   ln@(CodeLine _ _ [])       = (0, ln)

