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
  relmapSpecialize,
) where

import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Core.Relmap.Rop      as C
import qualified Koshucode.Baala.Core.Relmap.Relkit   as C



-- ----------------------  Constructors

-- | Retrieve relation from dataset.
relmapSource :: C.RopUse c -> B.JudgePattern -> [B.Termname] -> (C.Relmap c)
relmapSource = C.RelmapSource . C.ropLex

-- | Make a constant relmap.
relmapConst :: C.RopUse c -> B.Rel c -> C.Relmap c
relmapConst = C.RelmapConst . C.ropLex

-- | Alias for relmap.
relmapAlias :: C.RopUse c -> C.Relmap c -> C.Relmap c
relmapAlias = C.RelmapAlias . C.ropLex

-- | Make a flow relmap.
--   Flow relmaps take no subrelmaps.
relmapFlow :: C.RopUse c -> C.RelkitCalc c -> C.Relmap c
relmapFlow use relkit = relmapConfl use (const relkit) []

-- | Make a global relmap.
--   Global relmaps are flow relmaps with globals.
relmapGlobal :: C.RopUse c -> C.RelkitGlobal c -> C.Relmap c
relmapGlobal = C.RelmapGlobal . C.ropLex

-- | Make a binary relmap.
--   Binary relmaps take one subrelmap.
relmapBinary :: C.RopUse c -> C.RelkitBinary c -> C.Relmap c -> C.Relmap c
relmapBinary use kit m = relmapConfl use (kit . head) [m]

-- | Make a confluent relmap.
--   Confluent relmaps take multiple subrelmaps.
relmapConfl :: C.RopUse c -> C.RelkitConfl c -> [C.Relmap c] -> C.Relmap c
relmapConfl = C.RelmapCalc . C.ropLex



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
relmapLink :: forall c. [C.RelmapDef c] -> B.Map (C.Relmap c)
relmapLink rslist = maplink where
    rsrec :: [C.RelmapDef c]
    rsrec = maplink `B.mapSndTo` rslist

    maplink = C.mapToRelmap link

    link :: B.Map (C.Relmap c)
    link (C.RelmapLink lx name Nothing) =
        C.RelmapLink lx name $ lookup name rsrec
    link r = r

relmapSpecialize :: C.Global c -> [C.RelmapDef c]
  -> [C.RelkitDef c] -> B.Relhead -> C.Relmap c -> B.Ab (C.Relkit c, [C.RelkitDef c])
relmapSpecialize global rdef = sp [] where
    sel = C.globalSelect global

    sp _  kits _  (C.RelmapSource lx p ns)           = right kits lx (C.relkitConst $ sel p ns)
    sp _  kits _  (C.RelmapConst  lx rel)            = right kits lx (C.relkitConst rel)
    sp ks kits h1 (C.RelmapAlias  _ relmap)          = sp ks kits h1 relmap
    sp ks kits h1 (C.RelmapLink   _ _ (Just relmap)) = sp ks kits h1 relmap
    sp ks kits h1 (C.RelmapLink   lx name Nothing) =
        case lookup name rdef of
          Nothing -> abort lx $ Left $ B.AbortAnalysis [] $ B.AAUnkRelmap name
          Just r  -> let key = (h1, C.relmapLexList r)
                     in if key `elem` ks
                        then Right (C.Relkit h1 (B.Sourced [] $ C.RelkitLink name key Nothing),
                                     kits)
                        else case lookup key kits of
                               Just relkit -> Right (relkit, kits)
                               Nothing -> do (relkit@(C.Relkit h2 _), kits2) <- sp (key : ks) kits h1 r
                                             Right (C.Relkit h2 (B.Sourced [] $ C.RelkitLink name key Nothing),
                                                    (key, relkit) : kits2)

    sp ks kits h1 (C.RelmapAppend relmap1 relmap2) =
        do (relkit2, kits2) <- sp ks kits h1 relmap1
           (relkit3, kits3) <- sp ks kits2 (C.relkitHead relkit2) relmap2
           Right (B.mappend relkit2 relkit3, kits3)

    sp ks kits h1 (C.RelmapCalc lx mk relmaps) =
        abort lx $ do
          (subkits, kits2) <- spCollect ks kits h1 relmaps
          relkit <- mk subkits h1
          right kits2 lx relkit

    sp _ kits h1 (C.RelmapGlobal lx mk) =
        abort lx $ do
          relkit <- mk global h1
          right kits lx relkit

    spCollect _  kits _ [] = Right ([], kits)
    spCollect ks kits h1 (r : rs) =
        do (relkit,  kits2) <- sp        ks kits  h1 r
           (relkits, kits3) <- spCollect ks kits2 h1 rs
           Right (relkit : relkits, kits3)

    right kits lx r = Right (C.relkitSetSource lx r, kits)

    abort = B.abortableFrom "specialize"




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

