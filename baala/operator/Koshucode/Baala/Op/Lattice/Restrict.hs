{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Restrict by relmap

module Koshucode.Baala.Op.Lattice.Restrict
  ( -- * some
    consSome, relmapSome, relkitSome,
    -- * none
    consNone, relmapNone, relkitNone,
    -- * some-meet
    consSomeMeet, relmapSomeMeet,
    -- * none-meet
    consNoneMeet, relmapNoneMeet,
    -- * sub
    consSub, relmapSub, relkitSub,
    -- * compose
    consCompose, relmapCompose, relkitCompose,
  ) where

import qualified Data.Set                             as Set
import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Data                 as B
import qualified Koshucode.Baala.Core                 as C
import qualified Koshucode.Baala.Op.Builtin           as Op
import qualified Koshucode.Baala.Op.Lattice.Tropashko as Op
import qualified Koshucode.Baala.Op.Term              as Op



-- ----------------------  some

consSome :: (Ord c) => C.RopCons c
consSome med = 
    do rmap <- Op.getRelmap med "-relmap"
       Right $ relmapSome med rmap

relmapSome :: (Ord c) => C.Intmed c -> B.Map (C.Relmap c)
relmapSome med = C.relmapBinary med relkitSome

relkitSome :: (Ord c) => C.RelkitBinary c
relkitSome = relkitSemi False

relkitSemi :: (Ord c) => Bool -> C.RelkitBinary c
relkitSemi isEmpty (C.Relkit _ _ kitb2) he1 =
    Right $ C.relkit he1 $ C.RelkitAbSemi p kitb2
    where p bo2 = Right $ null bo2 == isEmpty



-- ----------------------  none

consNone :: (Ord c) => C.RopCons c
consNone med =
    do rmap <- Op.getRelmap med "-relmap"
       Right $ relmapNone med rmap

relmapNone :: (Ord c) => C.Intmed c -> B.Map (C.Relmap c)
relmapNone med = C.relmapBinary med relkitNone

relkitNone :: (Ord c) => C.RelkitBinary c
relkitNone = relkitSemi True



-- ----------------------  some-meet & none-meet

consSomeMeet :: (Ord c) => C.RopCons c
consSomeMeet med =
  do rmap <- Op.getRelmap med "-relmap"
     Right $ relmapSomeMeet med rmap

relmapSomeMeet :: (Ord c) => C.Intmed c -> C.Relmap c -> C.Relmap c
relmapSomeMeet med = C.relmapBinary med $ relkitFilterMeet True

consNoneMeet :: (Ord c) => C.RopCons c
consNoneMeet med =
  do rmap <- Op.getRelmap med "-relmap"
     Right $ relmapNoneMeet med rmap

relmapNoneMeet :: (Ord c) => C.Intmed c -> C.Relmap c -> C.Relmap c
relmapNoneMeet med = C.relmapBinary med $ relkitFilterMeet False

relkitFilterMeet :: forall c. (Ord c) => Bool -> C.RelkitBinary c
relkitFilterMeet which (C.Relkit _ (Just he2) kitb2) (Just he1) = Right kit3 where
    lr     = B.headNames he1 `B.headLR` B.headNames he2
    kit3   = C.relkitJust he1 $ C.RelkitAbFull False kitf3 [kitb2]

    kitf3 :: [C.BodyMap c] -> C.BodyMap c
    kitf3 bmaps bo1 =
        do let [bmap2] = bmaps
           bo2 <- bmap2 bo1
           Right $ test (toSet bo2) `filter` bo1

    toSet = Set.fromList . map (B.headRShare lr)
    test b2set cs1 = B.headLShare lr cs1 `Set.member` b2set == which

relkitFilterMeet _ _ _ = Right C.relkitNothing



-- ----------------------  sub

consSub :: (Ord c) => C.RopCons c
consSub med =
    do rmap <- Op.getRelmap med "-relmap"
       Right $ relmapSub med rmap

relmapSub :: (Ord c) => C.Intmed c -> B.Map (C.Relmap c)
relmapSub med = C.relmapBinary med relkitSub

relkitSub :: (Ord c) => C.RelkitBinary c
relkitSub kit2@(C.Relkit _ (Just he2) _) he1'@(Just he1)
    | B.isSuperhead he1 he2 = kit
    | otherwise = Right $ C.relkitJust he1 $ C.RelkitConst []
    where
      kit = do kit3 <- Op.relkitMeet kit2 he1'
               kit4 <- relkitSome kit3 he1'
               Right kit4
relkitSub _ _ = Right C.relkitNothing



-- ----------------------  compose

consCompose :: (Ord c) => C.RopCons c
consCompose med =
    do rmap <- Op.getRelmap med "-relmap"
       Right $ relmapCompose med rmap

relmapCompose :: (Ord c) => C.Intmed c -> B.Map (C.Relmap c)
relmapCompose med = C.relmapBinary med relkitCompose

relkitCompose :: forall c. (Ord c) => C.RelkitBinary c
relkitCompose kit2@(C.Relkit _ (Just he2) _) (Just he1) =
    do kitMeet <- Op.relkitMeet kit2 (Just he1)
       kitCut  <- Op.relkitCut (sharedNames he1 he2) (C.relkitOutput kitMeet)
       Right $ kitMeet `B.mappend` kitCut
relkitCompose _ _ = Right C.relkitNothing

sharedNames :: B.Head -> B.Head -> [B.TermName]
sharedNames he1 he2 = shared where
    ns1     = B.headNames he1
    ns2     = B.headNames he2
    lr      = B.headLR ns1 ns2
    shared  = B.headRShare lr ns2
