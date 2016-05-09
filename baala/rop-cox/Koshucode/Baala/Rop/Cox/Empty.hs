{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Rop.Cox.Empty
  ( ropsCoxEmpty,
    -- * both
    consBoth, relmapBoth,
    -- * maybe
    consMaybe, relmapMaybe, relkitMaybe,
  ) where

import qualified Koshucode.Baala.Base         as B
import qualified Koshucode.Baala.Data         as D
import qualified Koshucode.Baala.Core         as C
import qualified Koshucode.Baala.Rop.Base     as Op
import qualified Koshucode.Baala.Rop.Flat     as Op
import qualified Koshucode.Baala.Rop.Cox.Get  as Op


-- | Relmap operators that handles empties.
--
--   [@both R@]
--
--   [@maybe R@]
--     Meet input and given relation.
--     It keeps input tuples of which counterparts are totally negated.
-- 
ropsCoxEmpty :: (D.CContent c) => [C.Rop c]
ropsCoxEmpty = Op.ropList "cox-empty"  -- GROUP
    --        CONSTRUCTOR USAGE                ATTRIBUTE
    [ Op.def  consBoth    "both R [-fill E]"   "-relmap/ . -fill?"
    , Op.def  consMaybe   "maybe R [-fill E]"  "-relmap/ . -fill?"
    ]


-- ----------------------  both

consBoth :: (D.CContent c) => C.RopCons c
consBoth med =
    do rmap <- Op.getRelmap med "-relmap"
       fill <- Op.getFiller med "-fill"
       Right $ relmapBoth med fill rmap

relmapBoth :: (Ord c, D.CRel c) => C.Intmed c -> c -> B.Map (C.Relmap c)
relmapBoth med fill rmap = C.relmapCopy med "i" rmapBoth where
    rmapBoth = rmapL `B.mappend` Op.relmapJoin med rmapR
    rmapR    = rmap  `B.mappend` relmapMaybe med fill rmapIn
    rmapL    = relmapMaybe med fill rmap
    rmapIn   = C.relmapLocalSymbol med "i"


-- ----------------------  maybe

consMaybe :: (D.CContent c) => C.RopCons c
consMaybe med =
    do rmap <- Op.getRelmap med "-relmap"
       fill <- Op.getFiller med "-fill"
       Right $ relmapMaybe med fill rmap

relmapMaybe :: (Ord c, D.CRel c) => C.Intmed c -> c -> B.Map (C.Relmap c)
relmapMaybe med = C.relmapBinary med . relkitMaybe

relkitMaybe :: forall c. (Ord c, D.CRel c) => c -> C.RelkitBinary c
relkitMaybe fill (C.Relkit _ (Just he2) kitb2) (Just he1) = Right kit3 where
    lr   = D.headNames he1 `D.headLR` D.headNames he2
    he3  = he2 `B.mappend` he1
    kit3 = C.relkitJust he3 $ C.RelkitAbFull False kitf3 [kitb2]
    kitf3 :: [C.BodyMap c] -> C.BodyMap c
    kitf3 bmaps bo1 =
        do let [bmap2] = bmaps
           bo2 <- bmap2 bo1
           let b2map = B.gatherToMap $ map (D.headRSplit lr) bo2
           Right $ step b2map `concatMap` bo1

    heFill = D.headRSide lr $ D.headTypes he2
    fills  = selectFiller fill `map` heFill
    step b2map cs1 = case B.lookupMap (D.headLShare lr cs1) b2map of
                       Just b2side -> map (++ cs1) b2side
                       Nothing     -> [fills ++ cs1]

relkitMaybe _ _ _ = Right C.relkitNothing

selectFiller :: (D.CRel c) => c -> D.Type -> c
selectFiller _ t@(D.TypeRel _) = D.pRel $ D.Rel (D.Head t) []
selectFiller fill _ = fill
