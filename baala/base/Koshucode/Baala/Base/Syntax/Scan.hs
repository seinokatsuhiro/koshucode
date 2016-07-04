{-# OPTIONS_GHC -Wall #-}

-- | Code scanner.

module Koshucode.Baala.Base.Syntax.Scan
  ( -- * Type
    CodeScan (..), CodeScanMap, WordTable,

    -- * Function
    codeScanUp, codeScanUpBz,
    codeUpdate, codeUpdateWords,
    codeChange,
  ) where

import qualified Data.Map                             as Map
import qualified Koshucode.Baala.Base.IO              as B
import qualified Koshucode.Baala.Base.Prelude         as B
import qualified Koshucode.Baala.Base.Syntax.Line     as B


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

-- | Split source text into 'B.CodeLine' list.
--
--   1. Split source text into lines by line delimiters
--      (carriage return @\\r@ or line feed @\\n@).
--
--   2. Numbering lines from 1.
--      Internally, this is represented as
--      a list of pairs @(@'B.LineNumber'@, @'String'@)@.
--
--   3. Tokenize each lines,
--      and put tokens together in 'B.CodeLine'.

codeScanUp :: CodeScanMap a -> B.NIOPoint -> String -> [B.CodeLine a]
codeScanUp f nio = codeScanUpLines f nio . B.linesCrlfNumbered

-- | Lazy bytestring version of 'codeScanUp'.
codeScanUpBz :: CodeScanMap a -> B.NIOPoint -> B.Bz -> [B.CodeLine a]
codeScanUpBz f nio = codeScanUpLines f nio . B.linesCrlfBzNumbered

codeScanUpLines :: CodeScanMap a -> B.NIOPoint -> [B.NumberedLine] -> [B.CodeLine a]
codeScanUpLines f nio = loop (CodeScan f cp "" [] Map.empty) where
    cp    = B.def { B.codePtSource = nio }

    loop _ [] = []
    loop r ((num, line) : ls) =
       let cp'  = setCodePt num line cp
           r'   = codeScan $ setScan cp' line r
           toks = reverse $ codeOutput r'
           ls'  = loop r' ls
       in B.CodeLine num line toks : ls'

setCodePt :: B.LineNumber -> String -> B.Map B.CodePt
setCodePt num line cp =
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

