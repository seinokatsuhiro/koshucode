{-# OPTIONS_GHC -Wall #-}

-- | Operations on 'C.Relmap'.

module Koshucode.Baala.Core.Relmap.Construct
  (  -- * Generic relmap
    ConsRelmap', relmapCons,
  
    -- * Constructor
    relmapSource, relmapConst,
    relmapFlow, relmapHook,
    relmapBinary, relmapConfl,
    relmapCopy, relmapNest, relmapNestVar,
    relmapLink,
  
    -- * Select from relmap
    relmapSourceList, relmapNameList,
  
    -- * Append relmaps
    -- $AppendRelmaps
  ) where

import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Core.Lexmap           as C
import qualified Koshucode.Baala.Core.Relmap.Global    as C
import qualified Koshucode.Baala.Core.Relmap.Operator  as C
import qualified Koshucode.Baala.Core.Message          as Msg


-- ----------------------  Generic relmap

-- | Make a constructor pair of lexmap and relmap.
relmapCons :: C.Global' h c -> h c -> (C.ConsLexmap, ConsRelmap' h c)
relmapCons g hook = (consL, consR) where
    consL         = C.consLexmap findSorter
    consR         = consRelmap findRop g hook
    findSorter n  = C.ropSorter `fmap` findRop n
    findRop       = C.opsetFindRop $ C.globalOpset g

-- | Second step of constructing relmap, make relmap from lexmap.
type ConsRelmap' h c = C.Lexmap -> B.Ab (C.Relmap' h c)

consRelmap :: (C.RopName -> Maybe (C.Rop' h c)) -> C.Global' h c -> h c -> ConsRelmap' h c
consRelmap findRop g hook = relmap where
    relmap lx =
        case C.lexType lx of
          C.LexmapDerived  -> Right $ C.RelmapLink lx
          C.LexmapNest     -> Right $ C.RelmapLink lx
          C.LexmapBase     -> case findRop $ C.lexRopName lx of
                                Just rop -> Msg.abRelmap [lx] $ cons rop
                                Nothing  -> Msg.bug "missing operator @consRelmap"
        where cons rop      = do sub <- relmap `mapM` C.lexSubmap lx
                                 C.ropCons rop $ C.RopUse hook g lx sub


-- ----------------------  Construct

-- | Retrieve relation from dataset.
relmapSource :: C.RopUse' h c -> B.JudgePat -> [B.TermName] -> (C.Relmap' h c)
relmapSource = C.RelmapSource . C.ropLexmap

-- | Make a constant relmap.
relmapConst :: C.RopUse' h c -> B.Rel c -> C.Relmap' h c
relmapConst = C.RelmapConst . C.ropLexmap

-- | Make a flow relmap.
--   Flow relmaps take no subrelmaps.
relmapFlow :: C.RopUse' h c -> C.RelkitFlow c -> C.Relmap' h c
relmapFlow use relkit = relmapConfl use (const relkit) []

relmapHook :: C.RopUse' h c -> C.RelkitHook' h c -> C.Relmap' h c
relmapHook = C.RelmapHook . C.ropLexmap

-- | Make a binary relmap.
--   Binary relmaps take one subrelmap.
relmapBinary :: C.RopUse' h c -> C.RelkitBinary c -> C.Relmap' h c -> C.Relmap' h c
relmapBinary use kit rmap = relmapConfl use (kit . head) [rmap]

-- | Make a confluent relmap.
--   Confluent relmaps take multiple subrelmaps.
relmapConfl :: C.RopUse' h c -> C.RelkitConfl c -> [C.Relmap' h c] -> C.Relmap' h c
relmapConfl = C.RelmapCalc . C.ropLexmap

relmapCopy :: C.RopUse' h c -> String -> B.Map (C.Relmap' h c)
relmapCopy = C.RelmapCopy . C.ropLexmap

relmapNest :: C.RopUse' h c -> [B.Terminal String] -> B.Map (C.Relmap' h c)
relmapNest = C.RelmapNest . C.ropLexmap

relmapNestVar :: C.RopUse' h c -> String -> C.Relmap' h c
relmapNestVar u@C.RopUse { C.ropLexmap = lx } n = relmapLink u2 where
    u2   = u  { C.ropLexmap = lx2 }
    lx2  = lx { C.lexType     = C.LexmapNest
              , C.lexRopToken = B.textToken n }

relmapLink :: C.RopUse' h c -> C.Relmap' h c
relmapLink = C.RelmapLink . C.ropLexmap


-- ----------------------  Select

-- | List of 'C.RelmapSource'
relmapSourceList :: C.Relmap' h c -> [C.Relmap' h c]
relmapSourceList = relmapList f where
    f rmap@(C.RelmapSource _ _ _) = [rmap]
    f _ = []

-- | List of name in 'C.RelmapLink'
relmapNameList :: C.Relmap' h c -> [String]
relmapNameList = relmapList f where
    f (C.RelmapLink lx) = [C.lexRopName lx]
    f _ = []

relmapList :: B.Map (C.Relmap' h c -> [a])
relmapList f = loop where
    loop (C.RelmapAppend rmap1 rmap2) = loop rmap1 ++ loop rmap2
    loop (C.RelmapCalc _ _ rmaps)     = concatMap loop rmaps
    loop m = f m


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

