{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Rop.Nest.Confl
  ( 
    -- * copy
    consCopy,
    -- $CopyExample
  
    -- * for
    consFor, relmapFor, relkitFor,
    -- $ForExample
  
    -- * group
    consGroup, relmapGroup, relkitGroup,
    -- $GroupExample
  
    -- * slice
    consSlice, relmapSlice, relkitSlice,
    -- $SliceExample
  
    -- * slice-up
    consSliceUp, relmapSliceUp, relkitSliceUp,
  ) where

import qualified Koshucode.Baala.Base           as B
import qualified Koshucode.Baala.Syntax         as S
import qualified Koshucode.Baala.Data           as D
import qualified Koshucode.Baala.Core           as C
import qualified Koshucode.Baala.Rop.Base       as Op
import qualified Koshucode.Baala.Rop.Nest.Flow  as Op



-- ----------------------  copy

-- $CopyExample
--
--  Equivalent operation for @down@ @\/r@.
--
--    > copy r ( dee | slice /r r )

consCopy :: C.RopCons c
consCopy med =
  do n    <- Op.getWord   med "-var"
     rmap <- Op.getRelmap med "-relmap"
     Right $ C.relmapCopy med n rmap



-- ----------------------  for

-- $ForExample
--
--  Remove term @\/c@ from a nested relation in term @\/r@.
--
--    > for /r ( cut /c )

consFor :: (D.CRel c) => C.RopCons c
consFor med =
    do n    <- Op.getTerm   med "-term"
       rmap <- Op.getRelmap med "-relmap"
       Right $ relmapFor med n rmap

relmapFor :: (D.CRel c) => C.Intmed c -> S.TermName -> B.Map (C.Relmap c)
relmapFor med n rmap = relmapForInner med n (Op.relmapUp med n `B.mappend` rmap)

relmapForInner :: (D.CRel c) => C.Intmed c -> S.TermName -> B.Map (C.Relmap c)
relmapForInner med n = C.relmapNest med . bin where
    bin = C.relmapBinary med $ relkitFor n

relkitFor :: forall c. (D.CRel c) => S.TermName -> C.RelkitBinary c
relkitFor n (C.Relkit _ (Just he2) kitb2) (Just he1) = Right kit3 where
    lr    = [n] `D.headLR` D.headNames he1
    side  = D.headRSide lr
    he3   = D.headConsNest n he2 $ D.headMap side he1
    kit3  = C.relkitJust he3 $ C.RelkitOneToAbOne False kitf3 [kitb2]

    kitf3 :: [C.BodyMap c] -> B.AbMap [c]
    kitf3 bmaps cs1 =
        do let [bmap2] = bmaps
           bo2 <- bmap2 [cs1]
           Right $ D.pRel (D.Rel he2 bo2) : side cs1

relkitFor _ _ _ = Right C.relkitNothing



-- ----------------------  group

-- $GroupExample
--
--  Split relation from @b@ by relation from @a@.
--  In other words, group @b@ by @a@.
--  Result relations are nested in term @\/r@.
--
--    > a | group b -to /r

consGroup :: (Ord c, D.CRel c) => C.RopCons c
consGroup med =
  do rmap <- Op.getRelmap med "-relmap"
     n    <- Op.getTerm   med "-to"
     Right $ relmapGroup med n rmap

relmapGroup :: (Ord c, D.CRel c) => C.Intmed c -> S.TermName -> B.Map (C.Relmap c)
relmapGroup med = C.relmapBinary med . relkitGroup

relkitGroup :: forall c. (Ord c, D.CRel c) => S.TermName -> C.RelkitBinary c
relkitGroup n (C.Relkit _ (Just he2) kitb2) (Just he1) = Right kit3 where
    lr      = D.headNames he1 `D.headLR` D.headNames he2
    toMap2  = B.gatherToMap . map (D.headRAssoc lr)
    he3     = D.headConsNest n he2 he1
    kit3    = C.relkitJust he3 $ C.RelkitAbFull False kitf3 [kitb2]
    kitf3 bmaps bo1 =
        do let [bmap2] = bmaps
           bo2  <- bmap2 bo1
           let map2 = toMap2 bo2
           Right $ map (add map2) bo1

    add map2 cs1 =
        let b2maybe = B.lookupMap (D.headLShare lr cs1) map2
            b2sub   = B.fromMaybe [] b2maybe
        in D.pRel (D.Rel he2 b2sub) : cs1

relkitGroup _ _ _ = Right C.relkitNothing



-- ----------------------  slice

-- $SliceExample
--
--  Add nested relation in term @\/p@.
--
--    > slice /p ( source P /a /b )
--
--  Add nested relation as meet of @\/p@ and @\/q@.
--
--    > slice /r ( ^/p | meet ^/q )
--

consSlice :: (D.CRel c) => C.RopCons c
consSlice med =
  do n    <- Op.getTerm   med "-term"
     rmap <- Op.getOptRelmap C.relmapId med "-relmap"
     Right $ relmapSlice med n rmap

relmapSlice :: (D.CRel c) => C.Intmed c -> S.TermName -> B.Map (C.Relmap c)
relmapSlice med n = C.relmapNest med . bin where
    bin = C.relmapBinary med $ relkitSlice n

relkitSlice :: (D.CRel c) => S.TermName -> C.RelkitBinary c
relkitSlice n (C.Relkit _ (Just he2) kitb2) (Just he1) = Right kit3 where
    he3   = D.headConsNest n he2 he1
    kit3  = C.relkitJust he3 $ C.RelkitOneToAbOne False kitf3 [kitb2]
    kitf3 bmaps cs1 =
        do let [bmap2] = bmaps
           bo2 <- bmap2 [cs1]
           Right $ D.pRel (D.Rel he2 bo2) : cs1
relkitSlice _ _ _ = Right C.relkitNothing


-- ----------------------  slice-up

consSliceUp :: (D.CRel c) => C.RopCons c
consSliceUp med =
  do rmap <- Op.getOptRelmap C.relmapId med "-relmap"
     Right $ relmapSliceUp med rmap

relmapSliceUp :: (D.CRel c) => C.Intmed c -> B.Map (C.Relmap c)
relmapSliceUp med = C.relmapNest med . bin where
    bin = C.relmapBinary med relkitSliceUp

relkitSliceUp :: (D.CRel c) => C.RelkitBinary c
relkitSliceUp (C.Relkit _ (Just he2) kitb2) _ = Right kit3 where
    kit3  = C.relkitJust he2 $ C.RelkitOneToAbMany False kitf3 [kitb2]
    kitf3 bmaps cs1 = do let [bmap2] = bmaps
                         bmap2 [cs1]
relkitSliceUp _ _ = Right C.relkitNothing

