{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Confluent operators for nested relations.

module Koshucode.Baala.Rop.Nest.Confl
  ( -- * copy
    consCopy,
    -- * for
    consFor, relmapFor, relkitFor,
    -- * group
    consGroup, relmapGroup, relkitGroup,
    -- * slice
    consSlice, relmapSlice, relkitSlice,
    -- * slice-up
    consSliceUp, relmapSliceUp, relkitSliceUp,
  ) where

import qualified Koshucode.Baala.Overture          as O
import qualified Koshucode.Baala.Base              as B
import qualified Koshucode.Baala.Syntax            as S
import qualified Koshucode.Baala.Data              as D
import qualified Koshucode.Baala.Core              as C
import qualified Koshucode.Baala.Rop.Base          as Rop
import qualified Koshucode.Baala.Rop.Flat          as Rop
import qualified Koshucode.Baala.Rop.Nest.Flow     as Rop
import qualified Koshucode.Baala.Rop.Flat.Message  as Msg


-- ----------------------  copy

-- | __copy N R__
--
--   Copy input relation and referenced by ^N in R.
--
--   For example, @down@ @\/r@ can be rewrite to
--   @copy r ( dee | slice \/r ^r )@.
--
consCopy :: C.RopCons c
consCopy med =
  do n    <- Rop.getWord   med "-var"
     rmap <- Rop.getRelmap med "-relmap"
     Right $ C.relmapCopy med n rmap


-- ----------------------  for

-- | __for \/P R__
-- 
--  Map nested relation at \/P by relmap R.
--
--  The relamp @for \/r ( cut \/c )@ removes
--  term @\/c@ from a nested relation at the term @\/r@.
--
consFor :: (D.CRel c) => C.RopCons c
consFor med =
    do n    <- Rop.getTerm   med "-term"
       rmap <- Rop.getRelmap med "-relmap"
       Right $ relmapFor med n rmap

-- | Create @for@ relmap.
relmapFor :: (D.CRel c) => C.Intmed c -> S.TermName -> O.Map (C.Relmap c)
relmapFor med n rmap = relmapForInner med n (Rop.relmapUp med n O.++ rmap)

relmapForInner :: (D.CRel c) => C.Intmed c -> S.TermName -> O.Map (C.Relmap c)
relmapForInner med n = C.relmapNest med . bin where
    bin = C.relmapBinary med $ relkitFor n

-- | Create @for@ relkit.
relkitFor :: forall c. (D.CRel c) => S.TermName -> C.RelkitBinary c
relkitFor n (C.Relkit _ (Just he2) kitb2) (Just he1) = Right kit3 where
    lr    = D.termPicker [n] he1
    side  = D.ssRSide lr
    he3   = D.headConsNest n he2 $ D.headMap side he1
    kit3  = C.relkitJust he3 $ C.RelkitAbLinear False kitf3 [kitb2]

    kitf3 :: [C.BodyMap c] -> B.AbMap [c]
    kitf3 bmaps cs1 =
        do let [bmap2] = bmaps
           bo2 <- bmap2 [cs1]
           Right $ D.pRel (D.Rel he2 bo2) : side cs1

relkitFor _ _ _ = Right C.relkitNothing



-- ----------------------  group

-- | __group R -to \/N__
--
--  Group relation from relmap @R@ by input relation.
--  Grouped relations are added as content of term \/N
--  to each input tuples.
--
consGroup :: (Ord c, D.CRel c) => C.RopCons c
consGroup med =
  do rmap <- Rop.getRelmap med "-relmap"
     n    <- Rop.getTerm   med "-to"
     sh   <- Rop.getMaybe Rop.getTerms med "-share"
     Right $ relmapGroup med sh n rmap

-- | Create @group@ relmap.
relmapGroup :: (Ord c, D.CRel c) => C.Intmed c -> Rop.SharedTerms -> S.TermName -> O.Map (C.Relmap c)
relmapGroup med sh = C.relmapBinary med . relkitGroup sh

-- | Create @group@ relkit.
relkitGroup :: forall c. (Ord c, D.CRel c) => Rop.SharedTerms -> S.TermName -> C.RelkitBinary c
relkitGroup sh n (C.Relkit _ (Just he2) kitb2) (Just he1) = kit3 where
    lr      = D.termPicker he1 he2
    toMap2  = B.gatherToMap . map (D.ssRAssoc lr)
    he3     = D.headConsNest n he2 he1
    kit3    = case Rop.unmatchShare sh lr of
                Nothing     -> Right $ C.relkitJust he3 $ C.RelkitAbFull False kitf3 [kitb2]
                Just (e, a) -> Msg.unmatchShare e a

    kitf3 bmaps bo1 =
        do let [bmap2] = bmaps
           bo2  <- bmap2 bo1
           let map2 = toMap2 bo2
           Right $ map (add map2) bo1

    add map2 cs1 =
        let b2maybe = B.lookupMap (D.ssLShare lr cs1) map2
            b2sub   = B.fromMaybe [] b2maybe
        in D.pRel (D.Rel he2 b2sub) : cs1

relkitGroup _ _ _ _ = Right C.relkitNothing



-- ----------------------  slice

-- | __slice \/N [R]__
--
--  Slice input relation.
--  Each slices have single tuple of input relation,
--  and added as content of term \/N to each input tuples.
--  If R is given, slices are mapped by R.
--  If input relation has a nested relation at term \/P,
--  the relation can be referenced by ^\/P in R.
--
consSlice :: (D.CRel c) => C.RopCons c
consSlice med =
  do n    <- Rop.getTerm   med "-term"
     rmap <- Rop.getOptRelmap C.relmapId med "-relmap"
     Right $ relmapSlice med n rmap

-- | Create @slice@ relmap.
relmapSlice :: (D.CRel c) => C.Intmed c -> S.TermName -> O.Map (C.Relmap c)
relmapSlice med n = C.relmapNest med . bin where
    bin = C.relmapBinary med $ relkitSlice n

-- | Create @slice@ relkit.
relkitSlice :: (D.CRel c) => S.TermName -> C.RelkitBinary c
relkitSlice n (C.Relkit _ (Just he2) kitb2) (Just he1) = Right kit3 where
    he3   = D.headConsNest n he2 he1
    kit3  = C.relkitJust he3 $ C.RelkitAbLinear False kitf3 [kitb2]
    kitf3 bmaps cs1 =
        do let [bmap2] = bmaps
           bo2 <- bmap2 [cs1]
           Right $ D.pRel (D.Rel he2 bo2) : cs1
relkitSlice _ _ _ = Right C.relkitNothing


-- ----------------------  slice-up

-- | __slice-up R__
consSliceUp :: (D.CRel c) => C.RopCons c
consSliceUp med =
  do rmap <- Rop.getOptRelmap C.relmapId med "-relmap"
     Right $ relmapSliceUp med rmap

-- | Create @slice-up@ relmap.
relmapSliceUp :: (D.CRel c) => C.Intmed c -> O.Map (C.Relmap c)
relmapSliceUp med = C.relmapNest med . bin where
    bin = C.relmapBinary med relkitSliceUp

-- | Create @slice-up@ relkit.
relkitSliceUp :: (D.CRel c) => C.RelkitBinary c
relkitSliceUp (C.Relkit _ (Just he2) kitb2) _ = Right kit3 where
    kit3  = C.relkitJust he2 $ C.RelkitAbMany False kitf3 [kitb2]
    kitf3 bmaps cs1 = do let [bmap2] = bmaps
                         bmap2 [cs1]
relkitSliceUp _ _ = Right C.relkitNothing

