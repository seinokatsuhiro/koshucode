{-# OPTIONS_GHC -Wall #-}

-- | Construction of relmaps.

module Koshucode.Baala.Core.Relmap.Construct
  ( -- * Source
    relmapConst, relmapSource,

    -- * Calculation
    relmapFlow, relmapHook,
    relmapBinary, relmapConfl,

    -- * Variable
    relmapNest, relmapCopy,
    relmapLink, relmapLocalVar,

    -- * Append relmaps
    -- $AppendRelmaps

    -- * Bundle of constructors
    ConsRelmap', consRelmap,
  ) where

import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Core.Lexmap           as C
import qualified Koshucode.Baala.Core.Relmap.Global    as C
import qualified Koshucode.Baala.Core.Relmap.Relkit    as C
import qualified Koshucode.Baala.Core.Relmap.Relmap    as C
import qualified Koshucode.Baala.Core.Relmap.Rop       as C
import qualified Koshucode.Baala.Core.Message          as Msg


-- ----------------------  Source

-- | Make a constant relmap.
relmapConst :: C.RopUse' h c -> B.Rel c -> C.Relmap' h c
relmapConst = C.RelmapConst . C.ropLexmap

-- | Relmap for retrieving relation from dataset.
relmapSource :: C.RopUse' h c -> B.JudgePat -> [B.TermName] -> C.Relmap' h c
relmapSource = C.RelmapSource . C.ropLexmap


-- ----------------------  Calculation

-- | Make a flow relmap.
--   Flow relmaps take no submaps.
relmapFlow :: C.RopUse' h c -> C.RelkitFlow c -> C.Relmap' h c
relmapFlow use relkit = relmapConfl use (const relkit) []

relmapHook :: C.RopUse' h c -> C.RelkitHook' h c -> C.Relmap' h c
relmapHook = C.RelmapHook . C.ropLexmap

-- | Make a binary relmap.
--   Binary relmaps take one submap.
relmapBinary :: C.RopUse' h c -> C.RelkitBinary c -> C.Relmap' h c -> C.Relmap' h c
relmapBinary use kit rmap = relmapConfl use (kit . head) [rmap]

-- | Make a confluent relmap.
--   Confluent relmaps take multiple submaps.
relmapConfl :: C.RopUse' h c -> C.RelkitConfl c -> [C.Relmap' h c] -> C.Relmap' h c
relmapConfl = C.RelmapCalc . C.ropLexmap


-- ----------------------  Variable

-- | Parent for nested relation references.
relmapNest :: C.RopUse' h c -> B.Map (C.Relmap' h c)
relmapNest = C.RelmapNest . C.ropLexmap

relmapCopy :: C.RopUse' h c -> C.RopName -> B.Map (C.Relmap' h c)
relmapCopy = C.RelmapCopy . C.ropLexmap

relmapLink :: C.RopUse' h c -> C.Relmap' h c
relmapLink = C.RelmapLink . C.ropLexmap

relmapLocalVar :: C.RopUse' h c -> String -> C.Relmap' h c
relmapLocalVar u@C.RopUse { C.ropLexmap = lx } n = relmapLink u2 where
    u2   = u  { C.ropLexmap = lx2 }
    lx2  = lx { C.lexType   = C.LexmapLocal
              , C.lexToken  = B.textToken n }


-- ----------------------
-- $AppendRelmaps
--
--  This picture represents calculation
--  of mapping input relation to output relation.
--
--  > input -[ relmap ]- output
--
--  Relmap /A/ maps relation /R1/ to relation /R2/.
--  Another relmap /B/ maps /R2/ to /R3/.
--
--  > R1 -[ A ]- R2
--  > R2 -[ B ]- R3
--
--  Two relmaps /A/ and /B/ are jointed
--  with intermidiate relation /R2/.
--
--  > R1 -[ A ]- R2 -[ B ]- R3
--
--  Or, we can draw a directly jointed picture.
--  This whole structure is also 'RelmapAppend' /A B/.
--
--  > R1 -[ A ]--[ B ]- R3


-- ----------------------  Bundle of constructors

-- | Second step of constructing relmap, make relmap from lexmap.
type ConsRelmap' h c = C.Lexmap -> B.Ab (C.Relmap' h c)

-- | Construct relmap from lexmap.
consRelmap :: C.FindRop' h c -> h c -> ConsRelmap' h c
consRelmap findRop hook = relmap where
    relmap lx
        | link      = Right $ C.RelmapLink lx
        | otherwise = Msg.abRelmap [lx] $ case findRop name of
                        Nothing  -> Msg.bug "missing operator"
                        Just rop -> do sub <- relmap `mapM` C.lexSubmap lx
                                       C.ropCons rop $ C.RopUse hook lx sub
        where link = C.lexType lx /= C.LexmapBase
              name = C.lexName lx


