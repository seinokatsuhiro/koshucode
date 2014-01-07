{-# OPTIONS_GHC -Wall #-}

{-| Data structures for relation-to-relation mappings -}

module Koshucode.Baala.Core.Relmap.Relmap
( 
  -- * Append relmaps
  -- $AppendRelmaps

  -- * Constructors
  relmapSource,
  relmapConst,
  relmapAlias,
  relmapCalc,
  relmapConfl,
  relmapGlobal,

  -- * Selectors
  relmapSourceList,
  relmapNameList,

  -- * Linker
  relmapLinker,
) where

import qualified Koshucode.Baala.Base              as B
import qualified Koshucode.Baala.Core.Relmap.Rop   as C
import qualified Koshucode.Baala.Core.Relmap.Relfy as C



-- ----------------------  Constructors

{-| Retrieve relation from dataset. -}
relmapSource :: C.RopUse c -> B.JudgePattern -> [B.Termname] -> (C.Relmap c)
relmapSource = C.RelmapSource . C.ropHalf

{-| Constant relmap. -}
relmapConst :: C.RopUse c -> B.Rel c -> C.Relmap c
relmapConst = C.RelmapConst . C.ropHalf

{-| Alias for relmap. -}
relmapAlias :: C.RopUse c -> C.Relmap c -> C.Relmap c
relmapAlias = C.RelmapAlias . C.ropHalf

{-| Make a non-confluent relmap. -}
relmapCalc :: C.RopUse c -> C.RelmapCalcRelfy c -> C.Relmap c
relmapCalc use relfy = relmapConfl use (const relfy) []

{-| Make a confluent relmap. -}
relmapConfl :: C.RopUse c -> C.RelmapConflRelfy c -> [C.Relmap c] -> C.Relmap c
relmapConfl = C.RelmapCalc . C.ropHalf

relmapGlobal :: C.RopUse c -> (C.Global c -> C.RelmapCalcRelfy c) -> C.Relmap c
relmapGlobal = C.RelmapGlobal . C.ropHalf


-- ----------------------  Selector

{-| List of 'RelmapSource' -}
relmapSourceList :: C.Relmap c -> [C.Relmap c]
relmapSourceList = relmapList f where
    f m@(C.RelmapSource _ _ _) = [m]
    f _ = []

{-| List of name in 'RelmapName' -}
relmapNameList :: C.Relmap c -> [String]
relmapNameList = relmapList f where
    f (C.RelmapName _ n) = [n]
    f _ = []

relmapList :: B.Map (C.Relmap c -> [a])
relmapList f = loop where
    loop (C.RelmapAlias _ m1)   = loop m1
    loop (C.RelmapAppend m1 m2) = loop m1 ++ loop m2
    loop (C.RelmapCalc _ _ ms)  = concatMap loop ms
    loop m = f m



-- ----------------------  Link

{-| Link relmaps by its name. -}
relmapLinker
    :: [B.Named (C.Relmap c)]  -- ^ Relmap and its linking name
    -> C.Relmap c              -- ^ Relmap before linking
    -> C.Relmap c              -- ^ Linked relmap
relmapLinker = relmapLinker' . relmapLink

relmapLinker' :: [B.Named (C.Relmap c)] -> B.Map (C.Relmap c)
relmapLinker' ms' = link where
    link (C.RelmapAlias h m)     = C.RelmapAlias h (link m)
    link (C.RelmapAppend m1 m2)  = C.RelmapAppend (link m1) (link m2)
    link (C.RelmapCalc h g ms)   = C.RelmapCalc h g $ map link ms
    link m@(C.RelmapName _ n)    = case lookup n ms' of
                                   Just m' -> m'
                                   Nothing -> m
    link m                     = m

relmapLink :: B.Map [B.Named (C.Relmap c)]
relmapLink ms = ms' where
    ms'         = map link ms  -- make linked relmaps
    link (n, m) = (n, linker m)
    linker      = relmapLinker' ms'



-- ----------------------
{- $AppendRelmaps

   This picture represents calculation
   of mapping input relation to output relation.

   > input -[ relmap ]- output

   Relmap /A/ maps relation /R1/ to relation /R2/.
   Another relmap /B/ maps /R2/ to /R3/.

   > R1 -[ A ]- R2
   > R2 -[ B ]- R3

   Two relmaps /A/ and /B/ are jointed
   with intermidiate relation /R2/.

   > R1 -[ A ]- R2 -[ B ]- R3

   Or, we can draw a directly jointed picture.
   This whole structure is also 'RelmapAppend' /A B/.

   > R1 -[ A ]--[ B ]- R3

-}

