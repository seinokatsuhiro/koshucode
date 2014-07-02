{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Nil
( ropsNil,
  -- * both
  consBoth, relmapBoth,
  -- * maybe
  consMaybe, relmapMaybe, relkitMaybe,
) where

import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Core                 as C
import qualified Koshucode.Baala.Op.Builtin           as Op
import qualified Koshucode.Baala.Op.Lattice.Tropashko as Op


-- | Relmap operators that handles nils.
--
--   [@both R@]
--
--   [@maybe R@]
--     Meet input and given relation.
--     It keeps input tuples of which counterparts are totally negated.
-- 
ropsNil :: (Ord c, C.CRel c, C.CNil c) => [C.Rop c]
ropsNil = Op.ropList "nil"  -- GROUP
    --         CONSTRUCTOR USAGE      ATTRIBUTE
    [ Op.ropI  consBoth    "both R"   "-relmap/"
    , Op.ropI  consMaybe   "maybe R"  "-relmap/"
    ]


-- ----------------------  both

consBoth :: (Ord c, C.CRel c, C.CNil c) => C.RopCons c
consBoth use =
    do rmap <- Op.getRelmap use
       Right $ relmapBoth use rmap

relmapBoth :: (Ord c, C.CRel c, C.CNil c) => C.RopUse c -> B.Map (C.Relmap c)
relmapBoth use rmap = C.relmapCopy use "i" rmapBoth where
    rmapBoth = rmapL `B.mappend` Op.relmapJoin use rmapR
    rmapR    = rmap  `B.mappend` relmapMaybe use rmapIn
    rmapL    = relmapMaybe use rmap
    rmapIn   = C.relmapWithVar use "i"



-- ----------------------  maybe

consMaybe :: (Ord c, C.CNil c) => C.RopCons c
consMaybe use =
    do rmap <- Op.getRelmap use
       Right $ relmapMaybe use rmap

relmapMaybe :: (Ord c, C.CNil c) => C.RopUse c -> B.Map (C.Relmap c)
relmapMaybe use = C.relmapBinary use relkitMaybe

relkitMaybe :: forall c. (Ord c, C.CNil c) => C.RelkitBinary c
relkitMaybe (C.Relkit (Just he2) kitb2) (Just he1) = Right kit3 where

    ind1, ind2 :: [Int]
    (ind1, ind2) = B.headNames he1 `B.snipPair` B.headNames he2

    share1, share2, right2 :: B.Map [c]
    share1 = B.snipFrom ind1
    share2 = B.snipFrom ind2
    right2 = B.snipOff  ind2

    kv cs2  = (share2 cs2, right2 cs2)

    he3  = he2 `B.mappend` he1
    kit3 = C.relkitJust he3 $ C.RelkitAbFull False kitf3 [kitb2]
    kitf3 :: [C.Relbmap c] -> C.Relbmap c
    kitf3 bmaps bo1 =
        do let [bmap2] = bmaps
           bo2 <- bmap2 bo1
           let b2map = B.gatherToMap $ map kv bo2
           Right $ step b2map `concatMap` bo1

    nils = replicate (B.headDegree he3 - B.headDegree he1) C.nil
    step b2map cs1 = case B.lookupMap (share1 cs1) b2map of
                       Just b2side -> map (++ cs1) b2side
                       Nothing     -> [nils ++ cs1]

relkitMaybe _ _ = Right C.relkitNothing

