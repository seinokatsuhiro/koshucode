{-# OPTIONS_GHC -Wall #-}

-- | Operations on 'C.Relmap'.

module Koshucode.Baala.Core.Relmap.Construct
(  -- * Generic relmap
  RelmapCons (..),
  ConsRelmap,
  relmapCons,

  -- * Constructor
  relmapSource,
  relmapConst,
  relmapFlow,
  relmapGlobal,
  relmapBinary,
  relmapConfl,
  relmapCopy,
  relmapWith,
  relmapWithVar,
  relmapLink,

  -- * Select from relmap
  relmapSourceList,
  relmapNameList,

  -- * Append relmaps
  -- $AppendRelmaps
) where

import qualified Koshucode.Baala.Base                  as B
import qualified Koshucode.Baala.Core.Lexmap           as C
import qualified Koshucode.Baala.Core.Relmap.Global    as C
import qualified Koshucode.Baala.Core.Relmap.Operator  as C
import qualified Koshucode.Baala.Core.Message          as Message


-- ----------------------  Generic relmap

-- | Constructor of lexmap and relmap.
data RelmapCons c = RelmapCons C.ConsLexmap (ConsRelmap c)

instance Show (RelmapCons c) where
    show _ = "RelmapCons <lex> <full>"

-- | Make a constructor pair of lexmap and relmap.
relmapCons :: C.Global c -> RelmapCons c
relmapCons g = make $ unzip $ map pair rops where
    rops = C.opsetRops $ C.globalOpset g
    make (l, r) = RelmapCons (C.consLexmap l) (consRelmap g r)
    pair (C.Rop n _ sorter cons _) = ((n, sorter), (n, cons))

-- | Second step of constructing relmap, make relmap from lexmap.
type ConsRelmap c = C.Lexmap -> B.Ab (C.Relmap c)

consRelmap :: C.Global c -> [B.Named (C.RopCons c)] -> ConsRelmap c
consRelmap global conses = relmap where
    relmap lx =
        let rop  = C.lexOpName  lx
            roa  = C.lexAttr    lx
            lxs  = C.lexSubmap  lx
        in case C.lexType lx of
             C.LexmapWith    -> Right $ C.RelmapLink lx rop roa
             C.LexmapDerived -> Right $ C.RelmapLink lx rop roa
             C.LexmapBase    -> case lookup rop conses of
                                 Nothing   -> Message.unkRelmap rop
                                 Just cons -> Message.abRelmap [lx] $
                                              do rmaps <- mapM relmap lxs
                                                 cons $ C.RopUse global lx rmaps


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

relmapWith :: C.RopUse c -> [B.Terminal String] -> B.Map (C.Relmap c)
relmapWith = C.RelmapWith . C.ropLexmap

relmapWithVar :: C.RopUse c -> String -> C.Relmap c
relmapWithVar use n = relmapLink (withVar use) n []

withVar :: B.Map (C.RopUse c)
withVar u@C.RopUse { C.ropLexmap = lx } =
    u { C.ropLexmap = lx { C.lexType = C.LexmapWith }}

relmapLink :: C.RopUse c -> String -> C.AttrTrees -> C.Relmap c
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
    f (C.RelmapLink _ n _) = [n]
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

