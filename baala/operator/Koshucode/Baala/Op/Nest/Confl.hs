{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Nest.Confl
( 
  -- * rel-add
  consRelAdd, relmapRelAdd, relkitRelAdd,
  -- * rel-down
  consRelDown, relmapRelDown, relkitRelDown,
  -- * group
  consGroup, relmapGroup, relkitGroup,
) where

import qualified Koshucode.Baala.Base       as B
import qualified Koshucode.Baala.Core       as C
import qualified Koshucode.Baala.Op.Builtin as Op



-- ----------------------  rel-add

consRelAdd :: (C.CRel c) => C.RopCons c
consRelAdd use =
  do n    <- Op.getTerm   use "-term"
     rmap <- Op.getRelmap use
     with <- Op.getMaybe Op.getTerms use "-with"
     Right $ relmapRelAdd use (n, maybe [] id with) rmap

relmapRelAdd :: (C.CRel c) => C.RopUse c -> (B.TermName, [B.TermName]) -> B.Map (C.Relmap c)
relmapRelAdd use = C.relmapBinary use . relkitRelAdd

relkitRelAdd :: (C.CRel c) => (B.TermName, [B.TermName]) -> C.RelkitBinary c
relkitRelAdd (n, with) (C.Relkit (Just he2) kitb2) (Just he1) = Right kit3 where
    he3   = B.Relnest n (B.headTerms he2) `B.headConsTerm` he1
    kit3  = C.relkitJust he3 $ C.RelkitOneToAbOne False kitf3 [kitb2]
    kitf3 bmaps cs1 =
        do let [bmap2] = bmaps
           bo2 <- bmap2 [cs1]
           Right $ C.pRel (B.Rel he2 bo2) : cs1
relkitRelAdd _ _ _ = Right C.relkitNothing



-- ----------------------  rel-down

consRelDown :: (C.CRel c) => C.RopCons c
consRelDown use =
  do n <- Op.getTerm use "-term"
     Right $ relmapRelDown use n

relmapRelDown :: (C.CRel c) => C.RopUse c -> B.TermName -> C.Relmap c
relmapRelDown use = C.relmapFlow use . relkitRelDown

-- | RelDown the current relation in a term.
relkitRelDown :: (C.CRel c) => B.TermName -> C.RelkitCalc c
relkitRelDown _ Nothing = Right C.relkitNothing
relkitRelDown n (Just he1) = Right kit2 where
    he2       = B.Relhead [B.Relnest n $ B.headTerms he1]
    kit2      = C.relkitJust he2 $ C.RelkitFull False kitf2
    kitf2 bo1 = [[ C.pRel $ B.Rel he1 bo1 ]]



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
            b2sub   = B.maybeWith [] b2maybe
        in C.pRel (B.Rel he2 b2sub) : cs1

relkitGroup _ _ _ = Right C.relkitNothing

