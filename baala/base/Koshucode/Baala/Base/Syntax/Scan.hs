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
data CodeScan i o = CodeScan
     { codeMap     :: CodeScanMap i o  -- ^ Updater
     , codeInputPt :: B.CodePt         -- ^ Code point
     , codeInput   :: i                -- ^ Input text
     , codeOutput  :: [o]              -- ^ Output tokens
     , codeWords   :: WordTable        -- ^ Collected words
     }

-- | Update code scanner.
type CodeScanMap i o = B.Map (CodeScan i o)

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

codeScanUp :: CodeScanMap String o -> B.NIOPoint -> String -> [B.CodeLine o]
codeScanUp f nio = codeScanUpLines f nio . B.linesCrlfNumbered

-- | Lazy bytestring version of 'codeScanUp'.
codeScanUpBz :: CodeScanMap String o -> B.NIOPoint -> B.Bz -> [B.CodeLine o]
codeScanUpBz f nio = codeScanUpLines f nio . B.linesCrlfBzNumbered

codeScanUpLines :: CodeScanMap String o -> B.NIOPoint -> [B.NumberedLine] -> [B.CodeLine o]
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

setScan :: B.CodePt -> i -> CodeScan i o -> CodeScan i o
setScan cp line sc =
    sc { codeInputPt = cp
       , codeInput   = line
       , codeOutput  = [] }

codeScan :: CodeScanMap String o
codeScan sc@CodeScan { codeMap = f, codeInputPt = cp, codeInput = input }
    | null input  = call
    | otherwise   = codeScan call
    where call    = f sc { codeInputPt = setText input cp }

setText :: String -> B.Map B.CodePt
setText text cp = cp { B.codePtText = text }

-- | Update 'codeInput' and push result element to 'codeOutput'.
codeUpdate :: i -> o -> CodeScanMap i o
codeUpdate cs tok sc =
    sc { codeInput  = cs
       , codeOutput = tok : codeOutput sc }

-- | Update code scanner with word table.
codeUpdateWords :: WordTable -> i -> o -> CodeScanMap i o
codeUpdateWords ws cs tok sc =
    sc { codeInput  = cs
       , codeOutput = tok : codeOutput sc
       , codeWords  = ws }

-- | Change mapper of code sc.
codeChange :: CodeScanMap i o -> CodeScanMap i o
codeChange f sc = sc { codeMap = f }

