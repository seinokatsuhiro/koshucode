{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | Text lines delimited by carriage returns or line feeds.

module Koshucode.Baala.Base.Syntax.Line
  ( -- * Line
    LineNumber,
    NumberedLine,
    linesCrlfNumbered,
    linesCrlf, linesFrom,
    linesCrlfBzNumbered,
    linesCrlfBz, linesCrlfBzString,
    dropBom,

    -- * CodeLine
    CodeLine (..),
    IndentSize,
    lineIndentPair,

    -- * CodeScan
    CodeScan (..), CodeScanMap, WordTable,
    codeScanUp, codeScanUpBz,
    codeUpdate, codeUpdateWords,
    codeChange,
  ) where

import qualified Data.ByteString.Lazy                 as Bz
import qualified Data.ByteString.Lazy.UTF8            as Bu
import qualified Data.Map                             as Map
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

linesFrom :: (Show a) => a -> [String]
linesFrom = lines . show

linesCrlfBzNumbered :: B.Bz -> [NumberedLine]
linesCrlfBzNumbered = zip [1..] . linesCrlfBzString

linesCrlfBzString :: B.Bz -> [String]
linesCrlfBzString = map Bu.toString . linesCrlfBz

linesCrlfBz :: B.Bz -> [B.Bz]
linesCrlfBz = linesCrlfBzRaw . dropBom

linesCrlfBzRaw :: B.Bz -> [B.Bz]
linesCrlfBzRaw = loop where
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

-- | Remove UTF-8 BOM (EF BB BF) from lazy ByteString.
dropBom :: B.Bz -> B.Bz
dropBom bz =
  case Bz.splitAt 3 bz of
    (bom, body) | "\xEF\xBB\xBF" `Bz.isPrefixOf` bom -> body
                | otherwise                          -> bz


-- ----------------------  CodeLine

-- | Tokens in line.
data CodeLine a = CodeLine
    { lineNumber  :: LineNumber    -- ^ Line number, from 1.
    , lineContent :: String        -- ^ Line content without newline.
    , lineTokens  :: [a]           -- ^ Tokens in the line.
    } deriving (Show, Eq, Ord)

instance (B.CodePtr a) => B.CodePtr (CodeLine a) where
    codePtList (CodeLine _ _ ts) = B.codePtList $ head ts

type IndentSize = Int

lineIndentPair :: (a -> IndentSize) -> CodeLine a -> (IndentSize, CodeLine a)
lineIndentPair ind ln@(CodeLine _ _ (tk : _)) = (ind tk, ln)
lineIndentPair _   ln@(CodeLine _ _ [])       = (0, ln)


-- ----------------------  CodeScan

-- | Code scanner divides input text into output tokens.
data CodeScan a = CodeScan
     { codeMap     :: CodeScanMap a   -- ^ Updater
     , codeInputPt :: B.CodePt        -- ^ Code point
     , codeInput   :: String          -- ^ Input text
     , codeOutput  :: [a]             -- ^ Output tokens
     , codeWords   :: WordTable       -- ^ Collected words
     }

-- | Update code scanner.
type CodeScanMap a = B.Map (CodeScan a)

-- | Collected words.
type WordTable = Map.Map String String

-- | Split source text into 'CodeLine' list.
--
--   1. Split source text into lines by line delimiters
--      (carriage return @\\r@ or line feed @\\n@).
--
--   2. Numbering lines from 1.
--      Internally, this is represented as
--      a list of pairs @(@'B.LineNumber'@,@ 'String'@)@.
--
--   3. Tokenize each lines,
--      and put tokens together in 'CodeLine'.

codeScanUp :: CodeScanMap a -> B.NIOPoint -> String -> [CodeLine a]
codeScanUp f res = codeScanUpLines f res . linesCrlfNumbered

-- | Lazy bytestring version of 'codeScanUp'.
codeScanUpBz :: CodeScanMap a -> B.NIOPoint -> B.Bz -> [CodeLine a]
codeScanUpBz f res = codeScanUpLines f res . linesCrlfBzNumbered

codeScanUpLines :: CodeScanMap a -> B.NIOPoint -> [NumberedLine] -> [CodeLine a]
codeScanUpLines f res = loop (CodeScan f cp "" [] Map.empty) where
    cp    = B.def { B.codePtSource = res }

    loop _ [] = []
    loop r ((num, line) : ls) =
       let cp'  = setLine num line cp
           r'   = codeScan $ setScan cp' line r
           toks = reverse $ codeOutput r'
           ls'  = loop r' ls
       in CodeLine num line toks : ls'

setLine :: LineNumber -> String -> B.Map B.CodePt
setLine num line cp =
    cp { B.codePtLineNo    = num
       , B.codePtLineText  = line
       , B.codePtText      = line }

setScan :: B.CodePt -> String -> CodeScan a -> CodeScan a
setScan cp line sc =
    sc { codeInputPt = cp
       , codeInput   = line
       , codeOutput  = [] }

codeScan :: CodeScanMap a
codeScan sc@CodeScan { codeMap = f, codeInputPt = cp, codeInput = input }
    | null input  = call
    | otherwise   = codeScan call
    where call    = f sc { codeInputPt = setText input cp }

setText :: String -> B.Map B.CodePt
setText text cp = cp { B.codePtText = text }

-- | Update 'codeInput' and push result element to 'codeOutput'.
codeUpdate :: String -> a -> CodeScanMap a
codeUpdate cs tok sc =
    sc { codeInput  = cs
       , codeOutput = tok : codeOutput sc }

-- | Update code scanner with word table.
codeUpdateWords :: WordTable -> String -> a -> CodeScanMap a
codeUpdateWords ws cs tok sc =
    sc { codeInput  = cs
       , codeOutput = tok : codeOutput sc
       , codeWords  = ws }

-- | Change mapper of code sc.
codeChange :: CodeScanMap a -> CodeScanMap a
codeChange f sc = sc { codeMap = f }

