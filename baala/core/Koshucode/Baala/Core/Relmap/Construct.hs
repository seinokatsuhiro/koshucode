{-# OPTIONS_GHC -Wall #-}

-- | Construction of relmaps.

module Koshucode.Baala.Core.Relmap.Construct
  ( -- * Source
    relmapConst, relmapSource,

    -- * Calculation
    relmapFlow, relmapHook,
    relmapBinary, relmapConfl,

    -- * Variable
    relmapLink,
    relmapNest, relmapLocalNest,
    relmapCopy, relmapLocalSymbol, 

    -- * Append relmaps
    -- $AppendRelmaps

    -- * Bundle of constructors
    ConsRelmap', consRelmap,
  ) where

import qualified Koshucode.Baala.Overture              as O
import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Syntax                as S
import qualified Koshucode.Baala.Type                  as T
import qualified Koshucode.Baala.Core.Lexmap           as C
import qualified Koshucode.Baala.Core.Relkit           as C
import qualified Koshucode.Baala.Core.Relmap.Relmap    as C
import qualified Koshucode.Baala.Core.Relmap.Rop       as C
import qualified Koshucode.Baala.Data.Message          as Msg
import qualified Koshucode.Baala.Core.Relmap.Message   as Msg


-- ----------------------  Source

-- | Make a constant relmap.
relmapConst :: C.Intmed' h c -> T.Rel c -> C.Relmap' h c
relmapConst = C.RelmapConst . C.medLexmap

-- | Relmap for retrieving relation from dataset.
relmapSource :: C.Intmed' h c -> S.JudgeClass -> [S.TermName] -> C.Relmap' h c
relmapSource = C.RelmapSource . C.medLexmap


-- ----------------------  Calculation

-- | Make a flow relmap.
--   Flow relmaps take no submaps.
relmapFlow :: C.Intmed' h c -> C.RelkitFlow c -> C.Relmap' h c
relmapFlow use relkit = relmapConfl use (const relkit) []

-- | Relmap with hook data.
relmapHook :: C.Intmed' h c -> C.RelkitHook' h c -> C.Relmap' h c
relmapHook = C.RelmapHook . C.medLexmap

-- | Make a binary relmap.
--   Binary relmaps take one submap.
relmapBinary :: C.Intmed' h c -> C.RelkitBinary c -> C.Relmap' h c -> C.Relmap' h c
relmapBinary use kit rmap = relmapConfl use (kit . head) [rmap]

-- | Make a confluent relmap.
--   Confluent relmaps take multiple submaps.
relmapConfl :: C.Intmed' h c -> C.RelkitConfl c -> [C.Relmap' h c] -> C.Relmap' h c
relmapConfl = C.RelmapCalc . C.medLexmap


-- ----------------------  Variable

-- | Link-to-other relmap.
relmapLink :: C.Intmed' h c -> C.Relmap' h c
relmapLink = C.RelmapLink . C.medLexmap

-- | Reference base of nested relation references.
--   This base is refered by 'relmapLocalNest'.
relmapNest :: C.Intmed' h c -> O.Map (C.Relmap' h c)
relmapNest = C.RelmapNest . C.medLexmap

-- | Relation reference for nested relation.
relmapLocalNest :: C.Intmed' h c -> S.TermName -> C.Relmap' h c
relmapLocalNest med n = relmapLocal med $ S.LocalNest n

-- | Reference base of copy-of-input relation reference.
--   This base is refered by 'relmapLocalSymbol'.
relmapCopy :: C.Intmed' h c -> C.RopName -> O.Map (C.Relmap' h c)
relmapCopy = C.RelmapCopy . C.medLexmap

-- | Relation reference for locally introduced relation.
relmapLocalSymbol :: C.Intmed' h c -> C.RopName -> C.Relmap' h c
relmapLocalSymbol med n = relmapLocal med $ S.LocalSymbol n

relmapLocal :: C.Intmed' h c -> S.LocalRef -> C.Relmap' h c
relmapLocal use ref = relmapLink use' where
    lx    = C.medLexmap use
    cp    = B.csGetCP lx
    tok   = C.lexToken lx
    use'  = use { C.medLexmap = lx' }
    lx'   = lx  { C.lexType   = C.LexmapLocal
                , C.lexToken  = S.TLocal cp ref (-1) [tok] }

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
                        Just rop -> do sub <- namedRelmap `mapM` C.lexSubmap lx
                                       C.ropCons rop $ C.Intmed hook lx sub
        where link = C.lexType lx /= C.LexmapBase
              name = C.lexName lx

    namedRelmap (n, lx) = do rmap <- relmap lx
                             Right (O.tString n, rmap)

