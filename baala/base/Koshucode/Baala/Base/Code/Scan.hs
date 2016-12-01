{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

-- | Code scanner.

module Koshucode.Baala.Base.Code.Scan
  ( -- * Type
    CodeScan (..), CodeScanMap, WordTable,

    -- * Function
    isBol,
    codeScanSave, codeScanRestore,
    codeScanUp, codeScanUpBz,
    codeUpdate, codeUpdateWords,
    codeUpdateList, codeUpdateListWords,
    codeChange,
  ) where

import qualified Data.Map                             as Map
import qualified Koshucode.Baala.Overture             as O
import qualified Koshucode.Baala.Base.Abort           as B
import qualified Koshucode.Baala.Base.IO              as B
import qualified Koshucode.Baala.Base.Prelude         as B
import qualified Koshucode.Baala.Base.Code.Line       as B


-- | Code scanner divides input text into output tokens.
data CodeScan i o = CodeScan
     { codeMapSaved :: [CodeScanMap i o]  -- ^ Saved updater
     , codeMap      :: CodeScanMap i o    -- ^ Updater
     , codeInputPt  :: B.CodePos          -- ^ Code position
     , codeInput    :: i                  -- ^ Input text
     , codeOutput   :: [o]                -- ^ Output tokens
     , codeWords    :: WordTable          -- ^ Collected words
     }

-- | Return 'codeInputPt'.
instance B.GetCodePos (CodeScan i o) where
    getCPs cp = [codeInputPt cp]
    getCP  cp = codeInputPt cp

-- | Update code scanner.
type CodeScanMap i o = O.Map (CodeScan i o)

-- | Collected words.
type WordTable = Map.Map String String

-- | Test scanner at the beginning of line, i.e., no output collected.
isBol :: O.Test (CodeScan i o)
isBol CodeScan {..} = null codeOutput

-- | Save current updater.
codeScanSave :: CodeScanMap i o
codeScanSave sc@CodeScan { codeMapSaved = fs, codeMap = f } =
    sc { codeMapSaved = f : fs }

-- | Restore saved updater.
codeScanRestore :: CodeScanMap i o
codeScanRestore sc =
    case codeMapSaved sc of
      []      -> sc
      f : fs  -> sc { codeMapSaved = fs, codeMap = f }

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
--
codeScanUp :: CodeScanMap String o -> B.NIOPoint -> String -> [B.CodeLine o]
codeScanUp f nio = codeScanUpLines f nio . B.linesCrlfNumbered

-- | Lazy bytestring version of 'codeScanUp'.
codeScanUpBz :: CodeScanMap String o -> B.NIOPoint -> B.Bz -> [B.CodeLine o]
codeScanUpBz f nio = codeScanUpLines f nio . B.linesCrlfBzNumbered

codeScanUpLines :: CodeScanMap String o -> B.NIOPoint -> [B.NumberedLine] -> [B.CodeLine o]
codeScanUpLines f nio = loop (CodeScan [] f cp "" [] Map.empty) where
    cp    = B.def { B.cpSource = nio }

    loop _ [] = []
    loop r ((num, line) : ls) =
       let cp'  = setCodePos num line cp
           r'   = codeScan $ setScan cp' line r
           toks = reverse $ codeOutput r'
           ls'  = loop r' ls
       in B.CodeLine num line toks : ls'

setCodePos :: B.LineNumber -> String -> O.Map B.CodePos
setCodePos num line cp =
    cp { B.cpLineNo    = num
       , B.cpLineText  = line
       , B.cpText      = line }

setScan :: B.CodePos -> i -> CodeScan i o -> CodeScan i o
setScan cp line sc =
    sc { codeInputPt = cp
       , codeInput   = line
       , codeOutput  = [] }

codeScan :: CodeScanMap String o
codeScan sc@CodeScan { codeMap = f, codeInputPt = cp, codeInput = input }
    | null input  = call
    | otherwise   = codeScan call
    where call    = f sc { codeInputPt = setText input cp }

setText :: String -> O.Map B.CodePos
setText text cp = cp { B.cpText = text }

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

-- | Multi-element version of 'codeUpdate'.
codeUpdateList :: i -> [o] -> CodeScanMap i o
codeUpdateList cs toks sc =
    sc { codeInput  = cs
       , codeOutput = toks ++ codeOutput sc }

-- | Multi-element and cached version of 'codeUpdate'.
codeUpdateListWords :: WordTable -> i -> [o] -> CodeScanMap i o
codeUpdateListWords ws cs toks sc =
    sc { codeInput  = cs
       , codeOutput = toks ++ codeOutput sc
       , codeWords  = ws }

-- | Change mapper of code sc.
codeChange :: O.Map (CodeScanMap i o)
codeChange f sc = sc { codeMap = f }

