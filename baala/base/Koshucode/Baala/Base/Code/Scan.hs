{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

-- | Code scanner.

module Koshucode.Baala.Base.Code.Scan
  ( -- * Type
    CodeScan (..), CodeScanMap,
    WordCache, emptyWordCache,

    -- * Function
    isBol,
    codeScanSave, codeScanRestore,
    codeScanUp, codeScanUpBz,
    codeUpdate, codeUpdateWords,
    codeUpdateList, codeUpdateListWords,
    codeChange,
  ) where

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
     , codeWords    :: WordCache i        -- ^ Cached words
     }

-- | Return 'codeInputPt'.
instance B.GetCodePos (CodeScan i o) where
    getCPs cp = [codeInputPt cp]
    getCP  cp = codeInputPt cp

-- | Update code scanner.
type CodeScanMap i o = O.Map (CodeScan i o)

-- | Cached words.
type WordCache t = O.Cache t t

-- | Empty word cache.
emptyWordCache :: WordCache t
emptyWordCache = O.cache [] id

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
codeScanUp :: CodeScanMap String o -> B.IxIOPoint -> String -> [B.CodeLine String o]
codeScanUp f ixio = codeScanUpLines f ixio . B.linesCrlfNumbered

-- | Lazy bytestring version of 'codeScanUp'.
codeScanUpBz :: (B.ToCode code) => CodeScanMap String o -> B.IxIOPoint -> code -> [B.CodeLine String o]
codeScanUpBz f ixio = codeScanUpLines f ixio . B.linesCrlfBzNumbered

codeScanUpLines :: CodeScanMap String o -> B.IxIOPoint -> [B.NumberedLine] -> [B.CodeLine String o]
codeScanUpLines f ixio = loop (CodeScan [] f cp "" [] emptyWordCache) where
    cp = B.def { B.cpIndex = O.getIx ixio
               , B.cpPath  = O.getIOPath ixio }

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
codeUpdateWords :: WordCache i -> i -> o -> CodeScanMap i o
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
codeUpdateListWords :: WordCache i -> i -> [o] -> CodeScanMap i o
codeUpdateListWords ws cs toks sc =
    sc { codeInput  = cs
       , codeOutput = toks ++ codeOutput sc
       , codeWords  = ws }

-- | Change mapper of code sc.
codeChange :: O.Map (CodeScanMap i o)
codeChange f sc = sc { codeMap = f }

