{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Operations on 'C.Relmap'.

module Koshucode.Baala.Core.Relmap.Relmap
( 
  -- * Append relmaps
  -- $AppendRelmaps

  -- * Constructors
  relmapSource,
  relmapConst,
  relmapAlias,
  relmapFlow,
  relmapGlobal,
  relmapBinary,
  relmapConfl,

  -- * Selectors
  relmapSourceList,
  relmapNameList,

  -- * Linker
  relmapLink,
) where

import qualified Koshucode.Baala.Base               as B
import qualified Koshucode.Baala.Core.Relmap.Rop    as C



-- ----------------------  Constructors

-- | Retrieve relation from dataset.
relmapSource :: C.RopUse c -> B.JudgePattern -> [B.Termname] -> (C.Relmap c)
relmapSource = C.RelmapSource . C.ropHalf

-- | Make a constant relmap.
relmapConst :: C.RopUse c -> B.Rel c -> C.Relmap c
relmapConst = C.RelmapConst . C.ropHalf

-- | Alias for relmap.
relmapAlias :: C.RopUse c -> C.Relmap c -> C.Relmap c
relmapAlias = C.RelmapAlias . C.ropHalf

-- | Make a flow relmap.
--   Flow relmaps take no subrelmaps.
relmapFlow :: C.RopUse c -> C.RelkitCalc c -> C.Relmap c
relmapFlow use relkit = relmapConfl use (const relkit) []

-- | Make a global relmap.
--   Global relmaps are flow relmaps with globals.
relmapGlobal :: C.RopUse c -> C.RelkitGlobal c -> C.Relmap c
relmapGlobal = C.RelmapGlobal . C.ropHalf

-- | Make a binary relmap.
--   Binary relmaps take one subrelmap.
relmapBinary :: C.RopUse c -> C.RelkitBinary c -> C.Relmap c -> C.Relmap c
relmapBinary use kit m = relmapConfl use (kit . head) [m]

-- | Make a confluent relmap.
--   Confluent relmaps take multiple subrelmaps.
relmapConfl :: C.RopUse c -> C.RelkitConfl c -> [C.Relmap c] -> C.Relmap c
relmapConfl = C.RelmapCalc . C.ropHalf



-- ----------------------  Selector

-- | List of 'C.RelmapSource'
relmapSourceList :: C.Relmap c -> [C.Relmap c]
relmapSourceList = relmapList f where
    f m@(C.RelmapSource _ _ _) = [m]
    f _ = []

-- | List of name in 'C.RelmapLink'
relmapNameList :: C.Relmap c -> [String]
relmapNameList = relmapList f where
    f (C.RelmapLink _ n _) = [n]
    f _ = []

relmapList :: B.Map (C.Relmap c -> [a])
relmapList f = loop where
    loop (C.RelmapAlias _ m1)   = loop m1
    loop (C.RelmapAppend m1 m2) = loop m1 ++ loop m2
    loop (C.RelmapCalc _ _ ms)  = concatMap loop ms
    loop m = f m



-- ----------------------  Link

-- | Link relmaps by its name.
relmapLink :: forall c. [B.Named (C.Relmap c)] -> B.Map (C.Relmap c)
relmapLink rslist = maplink where
    rsrec   = maplink `B.mapSndTo` rslist
    maplink = C.mapToRelmap link

    link :: B.Map (C.Relmap c)
    link (C.RelmapLink half name Nothing) =
        C.RelmapLink half name $ lookup name rsrec
    link r = r



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

