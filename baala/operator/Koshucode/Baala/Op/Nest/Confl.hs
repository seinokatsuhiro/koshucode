{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Nest.Confl
( 
  -- * for
  consFor, relmapFor, relkitFor,
  -- * group
  consGroup, relmapGroup, relkitGroup,
  -- * rel-add
  consRelAdd, relmapRelAdd, relkitRelAdd,
) where

import qualified Koshucode.Baala.Base          as B
import qualified Koshucode.Baala.Core          as C
import qualified Koshucode.Baala.Op.Builtin    as Op
import qualified Koshucode.Baala.Op.Nest.Flow  as Op



-- ----------------------  for

consFor :: (C.CRel c) => C.RopCons c
consFor use =
  do n    <- Op.getTerm   use "-term"
     rmap <- Op.getRelmap use
     with <- Op.getOption [] Op.getTerms use "-with"
     Right $ relmapFor use with n rmap

relmapFor :: (C.CRel c) => C.RopUse c -> [B.TermName] -> B.TermName -> B.Map (C.Relmap c)
relmapFor use with n = C.relmapWith use (zip with with) . bin where
    bin = C.relmapBinary use $ relkitFor n

relkitFor :: forall c. (C.CRel c) => B.TermName -> C.RelkitBinary c
relkitFor n (C.Relkit (Just he2) kitb2) (Just he1) = Right kit3 where
    ns1   = B.headNames he1
    ind1  = [n] `B.snipIndex` ns1
    cut1  = B.snipOff  ind1
    he3   = B.Relnest n (B.headTerms he2) `B.headConsTerm` B.Relhead (cut1 $ B.headTerms he1)
    kit3  = C.relkitJust he3 $ C.RelkitOneToAbOne False kitf3 [kitb2]

    kitf3 :: [C.Relbmap c] -> [c] -> B.Ab [c]
    kitf3 bmaps cs1 =
        do let [bmap2] = bmaps
           bo2 <- bmap2 [cs1]
           Right $ C.pRel (B.Rel he2 bo2) : cut1 cs1
relkitFor _ _ _ = Right C.relkitNothing



-- ----------------------  rel-add

consRelAdd :: (C.CRel c) => C.RopCons c
consRelAdd use =
  do n    <- Op.getTerm   use "-term"
     rmap <- Op.getRelmap use
     with <- Op.getOption [] Op.getTerms use "-with"
     Right $ relmapRelAdd use with n rmap

relmapRelAdd :: (C.CRel c) => C.RopUse c -> [B.TermName] -> B.TermName -> B.Map (C.Relmap c)
relmapRelAdd use with n = C.relmapWith use (zip with with) . bin where
    bin = C.relmapBinary use $ relkitRelAdd n

relkitRelAdd :: (C.CRel c) => B.TermName -> C.RelkitBinary c
relkitRelAdd n (C.Relkit (Just he2) kitb2) (Just he1) = Right kit3 where
    he3   = B.Relnest n (B.headTerms he2) `B.headConsTerm` he1
    kit3  = C.relkitJust he3 $ C.RelkitOneToAbOne False kitf3 [kitb2]
    kitf3 bmaps cs1 =
        do let [bmap2] = bmaps
           bo2 <- bmap2 [cs1]
           Right $ C.pRel (B.Rel he2 bo2) : cs1
relkitRelAdd _ _ _ = Right C.relkitNothing



-- ----------------------  group

consGroup :: (Ord c, C.CRel c) => C.RopCons c
consGroup use =
  do n    <- Op.getTerm   use "-term"
     rmap <- Op.getRelmap use
     Right $ relmapGroup use n rmap

relmapGroup :: (Ord c, C.CRel c) => C.RopUse c -> B.TermName -> B.Map (C.Relmap c)
relmapGroup use = C.relmapBinary use . relkitGroup

-- | Grouping relation.
relkitGroup :: forall c. (Ord c, C.CRel c) => B.TermName -> C.RelkitBinary c
relkitGroup n (C.Relkit (Just he2) kitb2) (Just he1) = Right kit3 where

    ind1, ind2 :: [Int]
    (ind1, ind2) = B.headNames he1 `B.snipPair` B.headNames he2

    share1, share2 :: B.Map [c]
    share1 = B.snipFrom ind1
    share2 = B.snipFrom ind2

    toMap2 bo2 = Right $ B.gatherToMap $ map kv bo2
    kv cs2     = (share2 cs2, cs2)

    he3        = B.Relnest n (B.headTerms he2) `B.headConsTerm` he1
    kit3       = C.relkitJust he3 $ C.RelkitAbFull False kitf3 [kitb2]
    kitf3 bmaps bo1 =
        do let [bmap2] = bmaps
           bo2  <- bmap2 bo1
           map2 <- toMap2 bo2
           Right $ map (add map2) bo1

    add map2 cs1 =
        let b2maybe = B.lookupMap (share1 cs1) map2
            b2sub   = B.fromMaybe [] b2maybe
        in C.pRel (B.Rel he2 b2sub) : cs1

relkitGroup _ _ _ = Right C.relkitNothing

