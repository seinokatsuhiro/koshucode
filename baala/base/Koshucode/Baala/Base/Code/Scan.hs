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
    codeScanUp,
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
data CodeScan k t = CodeScan
     { scanMapSaved :: [CodeScanMap k t]  -- ^ Saved updater
     , scanMap      :: CodeScanMap k t    -- ^ Updater
     , scanCp       :: B.TCodePos t       -- ^ Code position
     , scanInput    :: t                  -- ^ Input text
     , scanOutput   :: [k t]              -- ^ Output tokens
     , scanCache    :: WordCache t        -- ^ Cached words
     }

-- | Return 'scanCp'.
instance (O.Textual t) => B.GetCodePos (CodeScan k t) where
    getCPs scan = [B.getCP scan]
    getCP  scan = B.cpStringify $ scanCp scan

-- | Update code scanner.
type CodeScanMap k t = O.Map (CodeScan k t)

-- | Cached words.
type WordCache t = O.Cache t t

-- | Empty word cache.
emptyWordCache :: WordCache t
emptyWordCache = O.cache [] id

-- | Test scanner at the beginning of line, i.e., no output collected.
isBol :: O.Test (CodeScan k t)
isBol CodeScan {..} = null scanOutput

-- | Save current updater.
codeScanSave :: CodeScanMap k t
codeScanSave sc@CodeScan { scanMapSaved = fs, scanMap = f } =
    sc { scanMapSaved = f : fs }

-- | Restore saved updater.
codeScanRestore :: CodeScanMap k t
codeScanRestore sc =
    case scanMapSaved sc of
      []      -> sc
      f : fs  -> sc { scanMapSaved = fs, scanMap = f }

-- | Split source text into 'B.CodeLine' list.
--
--   1. Split source text into lines by line delimiters
--      (carriage return @\\r@ or line feed @\\n@).
--
--   2. Numbering lines from 1.
--      Internally, this is represented as
--      a list of pairs @(@'B.LineNumber'@, @'O.Textual'@)@.
--
--   3. Tokenize each lines,
--      and put tokens together in 'B.CodeLine'.
--
codeScanUp :: (B.ToCode code, O.Textual t) => CodeScanMap k t -> B.IxIOPoint -> code -> [B.CodeLine k t]
codeScanUp f ixio = codeScanUpLines f ixio . B.numberedLines

codeScanUpLines :: (O.Textual t) => CodeScanMap k t -> B.IxIOPoint -> [B.NumberedLine t] -> [B.CodeLine k t]
codeScanUpLines f ixio = loop (CodeScan [] f cp O.tEmpty [] emptyWordCache) where
    cp = B.def { B.cpIndex = O.getIx ixio
               , B.cpPath  = O.getIOPath ixio }

    loop _ [] = []
    loop r ((num, line) : ls) =
       let cp'  = setCodePos num line cp
           r'   = codeScan $ setScan cp' line r
           toks = reverse $ scanOutput r'
           ls'  = loop r' ls
       in B.CodeLine num line toks : ls'

setCodePos :: B.LineNumber -> t -> O.Map (B.TCodePos t)
setCodePos num line cp =
    cp { B.cpLineNo    = num
       , B.cpLineText  = line
       , B.cpText      = line }

setScan :: B.TCodePos t -> t -> CodeScan k t -> CodeScan k t
setScan cp line sc =
    sc { scanCp = cp
       , scanInput   = line
       , scanOutput  = [] }

codeScan :: (O.Textual t) => CodeScanMap k t
codeScan sc@CodeScan { scanMap = f, scanCp = cp, scanInput = i }
    | O.tIsEmpty i  = call
    | otherwise     = codeScan call
    where call      = f sc { scanCp = setText i cp }

setText :: t -> O.Map (B.TCodePos t)
setText text cp = cp { B.cpText = text }

-- | Update 'scanInput' and push result element to 'scanOutput'.
codeUpdate :: t -> k t -> CodeScanMap k t
codeUpdate cs tok sc =
    sc { scanInput  = cs
       , scanOutput = tok : scanOutput sc }

-- | Update code scanner with word table.
codeUpdateWords :: WordCache t -> t -> k t -> CodeScanMap k t
codeUpdateWords ws cs tok sc =
    sc { scanInput  = cs
       , scanOutput = tok : scanOutput sc
       , scanCache  = ws }

-- | Multi-element version of 'codeUpdate'.
codeUpdateList :: t -> [k t] -> CodeScanMap k t
codeUpdateList cs toks sc =
    sc { scanInput  = cs
       , scanOutput = toks ++ scanOutput sc }

-- | Multi-element and cached version of 'codeUpdate'.
codeUpdateListWords :: WordCache t -> t -> [k t] -> CodeScanMap k t
codeUpdateListWords ws cs toks sc =
    sc { scanInput  = cs
       , scanOutput = toks ++ scanOutput sc
       , scanCache  = ws }

-- | Change mapper of code sc.
codeChange :: O.Map (CodeScanMap k t)
codeChange f sc = sc { scanMap = f }

