{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Nest.Confl
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

import qualified Koshucode.Baala.Base          as B
import qualified Koshucode.Baala.Core          as C
import qualified Koshucode.Baala.Op.Builtin    as Op
import qualified Koshucode.Baala.Op.Nest.Flow  as Op



-- ----------------------  copy

-- $CopyExample
--
--  Equivalent operation for @down@ @\/r@.
--
--    > copy r ( dee | slice /r r )

consCopy :: C.RopCons c
consCopy use =
  do n    <- Op.getWord   use "-with"
     rmap <- Op.getRelmap use "-relmap"
     Right $ C.relmapCopy use n rmap



-- ----------------------  for

-- $ForExample
--
--  Remove term @\/c@ from a nested relation in term @\/r@.
--
--    > for /r ( cut /c )

consFor :: (C.CRel c) => C.RopCons c
consFor use =
  do n    <- Op.getTerm   use "-term"
     rmap <- Op.getRelmap use "-relmap"
     with <- Op.getOption [] Op.getWithTerms use "-with"
     Right $ relmapFor use with n rmap

relmapFor :: (C.CRel c) => C.RopUse c -> [B.Terminal String] -> B.TermName -> B.Map (C.Relmap c)
relmapFor use with n rmap = relmapForInner use with n (Op.relmapUp use n `B.mappend` rmap)

relmapForInner :: (C.CRel c) => C.RopUse c -> [B.Terminal String] -> B.TermName -> B.Map (C.Relmap c)
relmapForInner use with n = C.relmapWith use with . bin where
    bin = C.relmapBinary use $ relkitFor n

relkitFor :: forall c. (C.CRel c) => B.TermName -> C.RelkitBinary c
relkitFor n (C.Relkit (Just he2) kitb2) (Just he1) = Right kit3 where
    ns1   =  B.headNames he1
    ind1  =  [n] `B.snipIndex` ns1
    cut1  =  B.snipOff  ind1
    he3   =  B.headConsNest n he2 $ B.headMap cut1 he1
    kit3  =  C.relkitJust he3 $ C.RelkitOneToAbOne False kitf3 [kitb2]

    kitf3 :: [C.Relbmap c] -> [c] -> B.Ab [c]
    kitf3 bmaps cs1 =
        do let [bmap2] = bmaps
           bo2 <- bmap2 [cs1]
           Right $ C.pRel (B.Rel he2 bo2) : cut1 cs1
relkitFor _ _ _ = Right C.relkitNothing



-- ----------------------  group

-- $GroupExample
--
--  Split relation from @b@ by relation from @a@.
--  In other words, group @b@ by @a@.
--  Result relations are nested in term @\/r@.
--
--    > a | group /r b

consGroup :: (Ord c, C.CRel c) => C.RopCons c
consGroup use =
  do n    <- Op.getTerm   use "-term"
     rmap <- Op.getRelmap use "-relmap"
     Right $ relmapGroup use n rmap

relmapGroup :: (Ord c, C.CRel c) => C.RopUse c -> B.TermName -> B.Map (C.Relmap c)
relmapGroup use = C.relmapBinary use . relkitGroup

relkitGroup :: forall c. (Ord c, C.CRel c) => B.TermName -> C.RelkitBinary c
relkitGroup n (C.Relkit (Just he2) kitb2) (Just he1) = Right kit3 where

    ind1, ind2 :: [Int]
    (ind1, ind2) = B.headNames he1 `B.snipPair` B.headNames he2

    share1, share2 :: B.Map [c]
    share1 = B.snipFrom ind1
    share2 = B.snipFrom ind2

    toMap2 bo2 = Right $ B.gatherToMap $ map kv bo2
    kv cs2     = (share2 cs2, cs2)

    he3        = B.headConsNest n he2 he1
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



-- ----------------------  slice

-- $SliceExample
--
--  Add nested relation in term @\/p@.
--
--    > slice /p ( source P /a /b )
--
--  Add nested relation as meet of @\/p@ and @\/q@.
--
--    > slice /r ( p | meet q ) -with /p /q
--

consSlice :: (C.CRel c) => C.RopCons c
consSlice use =
  do n    <- Op.getTerm   use "-term"
     rmap <- Op.getOptRelmap C.relmapId use "-relmap"
     with <- Op.getOption [] Op.getWithTerms use "-with"
     Right $ relmapSlice use with n rmap

relmapSlice :: (C.CRel c) => C.RopUse c -> [B.Terminal String] -> B.TermName -> B.Map (C.Relmap c)
relmapSlice use with n = C.relmapWith use with . bin where
    bin = C.relmapBinary use $ relkitSlice n

relkitSlice :: (C.CRel c) => B.TermName -> C.RelkitBinary c
relkitSlice n (C.Relkit (Just he2) kitb2) (Just he1) = Right kit3 where
    he3   = B.headConsNest n he2 he1
    kit3  = C.relkitJust he3 $ C.RelkitOneToAbOne False kitf3 [kitb2]
    kitf3 bmaps cs1 =
        do let [bmap2] = bmaps
           bo2 <- bmap2 [cs1]
           Right $ C.pRel (B.Rel he2 bo2) : cs1
relkitSlice _ _ _ = Right C.relkitNothing


-- ----------------------  slice-up

consSliceUp :: (C.CRel c) => C.RopCons c
consSliceUp use =
  do rmap <- Op.getOptRelmap C.relmapId use "-relmap"
     with <- Op.getOption [] Op.getWithTerms use "-with"
     Right $ relmapSliceUp use with rmap

relmapSliceUp :: (C.CRel c) => C.RopUse c -> [B.Terminal String] -> B.Map (C.Relmap c)
relmapSliceUp use with = C.relmapWith use with . bin where
    bin = C.relmapBinary use relkitSliceUp

relkitSliceUp :: (C.CRel c) => C.RelkitBinary c
relkitSliceUp (C.Relkit (Just he2) kitb2) _ = Right kit3 where
    kit3  = C.relkitJust he2 $ C.RelkitOneToAbMany False kitf3 [kitb2]
    kitf3 bmaps cs1 = do let [bmap2] = bmaps
                         bmap2 [cs1]
relkitSliceUp _ _ = Right C.relkitNothing

