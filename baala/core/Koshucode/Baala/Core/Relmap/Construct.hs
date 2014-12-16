{-# OPTIONS_GHC -Wall #-}

-- | Operations on 'C.Relmap'.

module Koshucode.Baala.Core.Relmap.Construct
  (  -- * Generic relmap
    ConsRelmap, relmapCons,
  
    -- * Constructor
    relmapSource, relmapConst,
    relmapFlow, relmapGlobal,
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
relmapCons :: C.Global c -> (C.ConsLexmap, ConsRelmap c)
relmapCons g = (consL, consR) where
    consL         = C.consLexmap findSorter
    consR         = consRelmap findRop g
    findSorter n  = C.ropSorter `fmap` findRop n
    findRop       = C.opsetFindRop $ C.globalOpset g

-- | Second step of constructing relmap, make relmap from lexmap.
type ConsRelmap c = C.Lexmap -> B.Ab (C.Relmap c)

consRelmap :: (C.RopName -> Maybe (C.Rop c)) -> C.Global c -> ConsRelmap c
consRelmap findRop g = relmap where
    relmap lx =
        case C.lexType lx of
          C.LexmapDerived  -> Right $ C.RelmapLink lx
          C.LexmapNest     -> Right $ C.RelmapLink lx
          C.LexmapBase     -> case findRop n of
                                Just rop -> Msg.abRelmap [lx] $ cons rop
                                Nothing  -> Msg.bug "missing operator @consRelmap"
        where n             = C.lexRopName lx
              cons rop      = do sub <- relmap `mapM` C.lexSubmap lx
                                 C.ropCons rop $ C.RopUse g lx sub


-- ----------------------  Construct

-- | Retrieve relation from dataset.
relmapSource :: C.RopUse c -> B.JudgePat -> [B.TermName] -> (C.Relmap c)
relmapSource = C.RelmapSource . C.ropLexmap

-- | Make a constant relmap.
relmapConst :: C.RopUse c -> B.Rel c -> C.Relmap c
relmapConst = C.RelmapConst . C.ropLexmap

-- | Make a flow relmap.
--   Flow relmaps take no subrelmaps.
relmapFlow :: C.RopUse c -> C.RelkitFlow c -> C.Relmap c
relmapFlow use relkit = relmapConfl use (const relkit) []

-- | Make a global relmap.
--   Global relmaps are flow relmaps with globals.
relmapGlobal :: C.RopUse c -> C.RelkitGlobal c -> C.Relmap c
relmapGlobal = C.RelmapGlobal . C.ropLexmap

-- | Make a binary relmap.
--   Binary relmaps take one subrelmap.
relmapBinary :: C.RopUse c -> C.RelkitBinary c -> C.Relmap c -> C.Relmap c
relmapBinary use kit rmap = relmapConfl use (kit . head) [rmap]

-- | Make a confluent relmap.
--   Confluent relmaps take multiple subrelmaps.
relmapConfl :: C.RopUse c -> C.RelkitConfl c -> [C.Relmap c] -> C.Relmap c
relmapConfl = C.RelmapCalc . C.ropLexmap

relmapCopy :: C.RopUse c -> String -> B.Map (C.Relmap c)
relmapCopy = C.RelmapCopy . C.ropLexmap

relmapNest :: C.RopUse c -> [B.Terminal String] -> B.Map (C.Relmap c)
relmapNest = C.RelmapNest . C.ropLexmap

relmapNestVar :: C.RopUse c -> String -> C.Relmap c
relmapNestVar u@C.RopUse { C.ropLexmap = lx } n = relmapLink u2 where
    u2   = u  { C.ropLexmap = lx2 }
    lx2  = lx { C.lexType     = C.LexmapNest
              , C.lexRopToken = B.textToken n }

relmapLink :: C.RopUse c -> C.Relmap c
relmapLink = C.RelmapLink . C.ropLexmap


-- ----------------------  Select

-- | List of 'C.RelmapSource'
relmapSourceList :: C.Relmap c -> [C.Relmap c]
relmapSourceList = relmapList f where
    f rmap@(C.RelmapSource _ _ _) = [rmap]
    f _ = []

-- | List of name in 'C.RelmapLink'
relmapNameList :: C.Relmap c -> [String]
relmapNameList = relmapList f where
    f (C.RelmapLink lx) = [C.lexRopName lx]
    f _ = []

relmapList :: B.Map (C.Relmap c -> [a])
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

