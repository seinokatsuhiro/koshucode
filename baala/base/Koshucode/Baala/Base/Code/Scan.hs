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
data CodeScan k t = CodeScan
     { codeMapSaved :: [CodeScanMap k t]  -- ^ Saved updater
     , codeMap      :: CodeScanMap k t    -- ^ Updater
     , codeInputPt  :: B.CodePos          -- ^ Code position
     , codeInput    :: t                  -- ^ Input text
     , codeOutput   :: [k t]              -- ^ Output tokens
     , codeWords    :: WordCache t        -- ^ Cached words
     }

-- | Return 'codeInputPt'.
instance B.GetCodePos (CodeScan k t) where
    getCPs cp = [codeInputPt cp]
    getCP  cp = codeInputPt cp

-- | Update code scanner.
type CodeScanMap k t = O.Map (CodeScan k t)

-- | Cached words.
type WordCache t = O.Cache t t

-- | Empty word cache.
emptyWordCache :: WordCache t
emptyWordCache = O.cache [] id

-- | Test scanner at the beginning of line, i.e., no output collected.
isBol :: O.Test (CodeScan k t)
isBol CodeScan {..} = null codeOutput

-- | Save current updater.
codeScanSave :: CodeScanMap k t
codeScanSave sc@CodeScan { codeMapSaved = fs, codeMap = f } =
    sc { codeMapSaved = f : fs }

-- | Restore saved updater.
codeScanRestore :: CodeScanMap k t
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
codeScanUp :: CodeScanMap k String -> B.IxIOPoint -> String -> [B.CodeLine k String]
codeScanUp f ixio = codeScanUpLines f ixio . B.linesCrlfNumbered

-- | Lazy bytestring version of 'codeScanUp'.
codeScanUpBz :: (B.ToCode code) => CodeScanMap k String -> B.IxIOPoint -> code -> [B.CodeLine k String]
codeScanUpBz f ixio = codeScanUpLines f ixio . B.linesCrlfBzNumbered

codeScanUpLines :: CodeScanMap k String -> B.IxIOPoint -> [B.NumberedLine] -> [B.CodeLine k String]
codeScanUpLines f ixio = loop (CodeScan [] f cp O.tEmpty [] emptyWordCache) where
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

setScan :: B.CodePos -> t -> CodeScan k t -> CodeScan k t
setScan cp line sc =
    sc { codeInputPt = cp
       , codeInput   = line
       , codeOutput  = [] }

codeScan :: CodeScanMap k String
codeScan sc@CodeScan { codeMap = f, codeInputPt = cp, codeInput = input }
    | null input  = call
    | otherwise   = codeScan call
    where call    = f sc { codeInputPt = setText input cp }

setText :: String -> O.Map B.CodePos
setText text cp = cp { B.cpText = text }

-- | Update 'codeInput' and push result element to 'codeOutput'.
codeUpdate :: t -> k t -> CodeScanMap k t
codeUpdate cs tok sc =
    sc { codeInput  = cs
       , codeOutput = tok : codeOutput sc }

-- | Update code scanner with word table.
codeUpdateWords :: WordCache t -> t -> k t -> CodeScanMap k t
codeUpdateWords ws cs tok sc =
    sc { codeInput  = cs
       , codeOutput = tok : codeOutput sc
       , codeWords  = ws }

-- | Multi-element version of 'codeUpdate'.
codeUpdateList :: t -> [k t] -> CodeScanMap k t
codeUpdateList cs toks sc =
    sc { codeInput  = cs
       , codeOutput = toks ++ codeOutput sc }

-- | Multi-element and cached version of 'codeUpdate'.
codeUpdateListWords :: WordCache t -> t -> [k t] -> CodeScanMap k t
codeUpdateListWords ws cs toks sc =
    sc { codeInput  = cs
       , codeOutput = toks ++ codeOutput sc
       , codeWords  = ws }

-- | Change mapper of code sc.
codeChange :: O.Map (CodeScanMap k t)
codeChange f sc = sc { codeMap = f }

