{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Restrict by relmap

module Koshucode.Baala.Rop.Flat.Lattice.Restrict
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

import qualified Data.Set                                   as Set
import qualified Koshucode.Baala.Base                       as B
import qualified Koshucode.Baala.Syntax                     as S
import qualified Koshucode.Baala.Data                       as D
import qualified Koshucode.Baala.Core                       as C
import qualified Koshucode.Baala.Rop.Base                   as Op
import qualified Koshucode.Baala.Rop.Flat.Lattice.Tropashko as Op
import qualified Koshucode.Baala.Rop.Flat.Term              as Op
import qualified Koshucode.Baala.Rop.Flat.Message           as Msg


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
       sh   <- Op.getMaybe Op.getTerms med "-share"
       Right $ relmapSomeMeet med sh rmap

relmapSomeMeet :: (Ord c) => C.Intmed c -> Op.SharedTerms -> C.Relmap c -> C.Relmap c
relmapSomeMeet med sh = C.relmapBinary med $ relkitFilterMeet True sh

consNoneMeet :: (Ord c) => C.RopCons c
consNoneMeet med =
    do rmap <- Op.getRelmap med "-relmap"
       sh   <- Op.getMaybe Op.getTerms med "-share"
       Right $ relmapNoneMeet med sh rmap

relmapNoneMeet :: (Ord c) => C.Intmed c -> Op.SharedTerms -> C.Relmap c -> C.Relmap c
relmapNoneMeet med sh = C.relmapBinary med $ relkitFilterMeet False sh

relkitFilterMeet :: forall c. (Ord c) => Bool -> Op.SharedTerms -> C.RelkitBinary c
relkitFilterMeet which sh (C.Relkit _ (Just he2) kitb2) (Just he1) = kit3 where
    lr     = D.headNames he1 `D.headLR` D.headNames he2
    kit3   = case Op.unmatchShare sh lr of
               Nothing     -> Right $ C.relkitJust he1 $ C.RelkitAbFull False kitf3 [kitb2]
               Just (e, a) -> Msg.unmatchShare e a

    kitf3 :: [C.BodyMap c] -> C.BodyMap c
    kitf3 bmaps bo1 =
        do let [bmap2] = bmaps
           bo2 <- bmap2 bo1
           Right $ test (toSet bo2) `filter` bo1

    toSet = Set.fromList . map (D.headRShare lr)
    test b2set cs1 = D.headLShare lr cs1 `Set.member` b2set == which

relkitFilterMeet _ _ _ _ = Right C.relkitNothing


-- ----------------------  sub

consSub :: (Ord c) => C.RopCons c
consSub med =
    do rmap <- Op.getRelmap med "-relmap"
       Right $ relmapSub med rmap

relmapSub :: (Ord c) => C.Intmed c -> B.Map (C.Relmap c)
relmapSub med = C.relmapBinary med relkitSub

relkitSub :: (Ord c) => C.RelkitBinary c
relkitSub kit2@(C.Relkit _ (Just he2) _) he1'@(Just he1)
    | D.isSuperhead he1 he2 = kit
    | otherwise = Right $ C.relkitJust he1 $ C.RelkitConst []
    where
      kit = do kit3 <- Op.relkitMeet Nothing kit2 he1'
               kit4 <- relkitSome kit3 he1'
               Right kit4
relkitSub _ _ = Right C.relkitNothing



-- ----------------------  compose

-- | Construct relmap for relational composition.
consCompose :: (Ord c) => C.RopCons c
consCompose med =
    do rmap <- Op.getRelmap med "-relmap"
       sh   <- Op.getMaybe Op.getTerms med "-share"
       Right $ relmapCompose med sh rmap

-- | Relational composition.
relmapCompose :: (Ord c) => C.Intmed c -> Op.SharedTerms -> B.Map (C.Relmap c)
relmapCompose med sh = C.relmapBinary med $ relkitCompose sh

-- | Calculate relational composition.
relkitCompose :: forall c. (Ord c) => Op.SharedTerms -> C.RelkitBinary c
relkitCompose sh kit2@(C.Relkit _ (Just he2) _) (Just he1) =
    do kitMeet <- Op.relkitMeet sh kit2 (Just he1)
       kitCut  <- Op.relkitCut (sharedNames he1 he2) (C.relkitOutput kitMeet)
       Right $ kitMeet `B.mappend` kitCut
relkitCompose _ _ _ = Right C.relkitNothing

sharedNames :: D.Head -> D.Head -> [S.TermName]
sharedNames he1 he2 = shared where
    ns1     = D.headNames he1
    ns2     = D.headNames he2
    lr      = D.headLR ns1 ns2
    shared  = D.headRShare lr ns2
