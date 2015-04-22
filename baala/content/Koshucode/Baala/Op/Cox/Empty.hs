{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Cox.Empty
  ( ropsCoxEmpty,
    -- * both
    consBoth, relmapBoth,
    -- * maybe
    consMaybe, relmapMaybe, relkitMaybe,
  ) where

import qualified Koshucode.Baala.Base         as B
import qualified Koshucode.Baala.Core         as C
import qualified Koshucode.Baala.Op.Builtin   as Op
import qualified Koshucode.Baala.Op.Lattice   as Op
import qualified Koshucode.Baala.Op.Cox.Get   as Op


-- | Relmap operators that handles empties.
--
--   [@both R@]
--
--   [@maybe R@]
--     Meet input and given relation.
--     It keeps input tuples of which counterparts are totally negated.
-- 
ropsCoxEmpty :: (C.CContent c) => [C.Rop c]
ropsCoxEmpty = Op.ropList "cox-empty"  -- GROUP
    --        CONSTRUCTOR USAGE                ATTRIBUTE
    [ Op.def  consBoth    "both R [-fill E]"   "1 -relmap/ | -fill"
    , Op.def  consMaybe   "maybe R [-fill E]"  "1 -relmap/ | -fill"
    ]


-- ----------------------  both

consBoth :: (C.CContent c) => C.RopCons c
consBoth med =
    do rmap <- Op.getRelmap med "-relmap"
       fill <- Op.getFiller med "-fill"
       Right $ relmapBoth med fill rmap

relmapBoth :: (Ord c, C.CRel c) => C.Intmed c -> c -> B.Map (C.Relmap c)
relmapBoth med fill rmap = C.relmapCopy med "i" rmapBoth where
    rmapBoth = rmapL `B.mappend` Op.relmapJoin med rmapR
    rmapR    = rmap  `B.mappend` relmapMaybe med fill rmapIn
    rmapL    = relmapMaybe med fill rmap
    rmapIn   = C.relmapLocalSymbol med "i"


-- ----------------------  maybe

consMaybe :: (C.CContent c) => C.RopCons c
consMaybe med =
    do rmap <- Op.getRelmap med "-relmap"
       fill <- Op.getFiller med "-fill"
       Right $ relmapMaybe med fill rmap

relmapMaybe :: (Ord c, C.CRel c) => C.Intmed c -> c -> B.Map (C.Relmap c)
relmapMaybe med = C.relmapBinary med . relkitMaybe

relkitMaybe :: forall c. (Ord c, C.CRel c) => c -> C.RelkitBinary c
relkitMaybe fill (C.Relkit _ (Just he2) kitb2) (Just he1) = Right kit3 where
    lr   = B.headNames he1 `B.headLR` B.headNames he2
    he3  = he2 `B.mappend` he1
    kit3 = C.relkitJust he3 $ C.RelkitAbFull False kitf3 [kitb2]
    kitf3 :: [C.BodyMap c] -> C.BodyMap c
    kitf3 bmaps bo1 =
        do let [bmap2] = bmaps
           bo2 <- bmap2 bo1
           let b2map = B.gatherToMap $ map (B.headRSplit lr) bo2
           Right $ step b2map `concatMap` bo1

    heFill = B.headRShare lr $ B.headTypes he2
    fills  = selectFiller fill `map` heFill
    step b2map cs1 = case B.lookupMap (B.headLShare lr cs1) b2map of
                       Just b2side -> map (++ cs1) b2side
                       Nothing     -> [fills ++ cs1]

relkitMaybe _ _ _ = Right C.relkitNothing

selectFiller :: (C.CRel c) => c -> B.Type -> c
selectFiller _ t@(B.TypeRel _) = C.pRel $ B.Rel (B.Head t) []
selectFiller fill _ = fill
