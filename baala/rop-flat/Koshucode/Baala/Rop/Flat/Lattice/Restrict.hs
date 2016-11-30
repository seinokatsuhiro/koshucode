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
import qualified Koshucode.Baala.Overture                   as O
import qualified Koshucode.Baala.Syntax                     as S
import qualified Koshucode.Baala.Data                       as D
import qualified Koshucode.Baala.Core                       as C
import qualified Koshucode.Baala.Rop.Base                   as Rop
import qualified Koshucode.Baala.Rop.Flat.Lattice.Tropashko as Rop
import qualified Koshucode.Baala.Rop.Flat.Term              as Rop
import qualified Koshucode.Baala.Rop.Flat.Message           as Msg


-- ----------------------  some

-- | Construct relmap of existential filter.
consSome :: (Ord c) => C.RopCons c
consSome med = 
    do rmap <- Rop.getRelmap med "-relmap"
       Right $ relmapSome med rmap

-- | Relmap of existential filter.
relmapSome :: (Ord c) => C.Intmed c -> O.Map (C.Relmap c)
relmapSome med = C.relmapBinary med relkitSome

-- | Calculate existential filter.
relkitSome :: (Ord c) => C.RelkitBinary c
relkitSome = relkitSemi False

relkitSemi :: (Ord c) => Bool -> C.RelkitBinary c
relkitSemi isEmpty (C.Relkit _ _ kitb2) he1 =
    Right $ C.relkit he1 $ C.RelkitAbSemi p kitb2
    where p bo2 = Right $ null bo2 == isEmpty



-- ----------------------  none

-- | Construct relmap of non-existential filter.
consNone :: (Ord c) => C.RopCons c
consNone med =
    do rmap <- Rop.getRelmap med "-relmap"
       Right $ relmapNone med rmap

-- | Relmap of non-existential filter.
relmapNone :: (Ord c) => C.Intmed c -> O.Map (C.Relmap c)
relmapNone med = C.relmapBinary med relkitNone

-- | Calculate non-existential filter.
relkitNone :: (Ord c) => C.RelkitBinary c
relkitNone = relkitSemi True



-- ----------------------  some-meet & none-meet

-- | Construct some-and-meet relmap.
consSomeMeet :: (Ord c) => C.RopCons c
consSomeMeet med =
    do rmap <- Rop.getRelmap med "-relmap"
       sh   <- Rop.getMaybe Rop.getTerms med "-share"
       Right $ relmapSomeMeet med sh rmap

-- | Some-and-meet relmap.
--
--   @some-meet R == some ( meet R )@
relmapSomeMeet :: (Ord c) => C.Intmed c -> Rop.SharedTerms -> C.Relmap c -> C.Relmap c
relmapSomeMeet med sh = C.relmapBinary med $ relkitFilterMeet True sh

-- | Construct none-and-meet relmap.
consNoneMeet :: (Ord c) => C.RopCons c
consNoneMeet med =
    do rmap <- Rop.getRelmap med "-relmap"
       sh   <- Rop.getMaybe Rop.getTerms med "-share"
       Right $ relmapNoneMeet med sh rmap

-- | None-and-meet relmap.
--
--   @ none-meet R == none ( meet R ) @
relmapNoneMeet :: (Ord c) => C.Intmed c -> Rop.SharedTerms -> C.Relmap c -> C.Relmap c
relmapNoneMeet med sh = C.relmapBinary med $ relkitFilterMeet False sh

relkitFilterMeet :: forall c. (Ord c) => Bool -> Rop.SharedTerms -> C.RelkitBinary c
relkitFilterMeet which sh (C.RelkitOutput he2 kitb2) (Just he1) = kit3 where
    lr     = D.termPicker he1 he2
    kit3   = case Rop.unmatchShare sh lr of
               Nothing     -> Right $ C.relkitJust he1 $ C.RelkitAbFull False kitf3 [kitb2]
               Just (e, a) -> Msg.unmatchShare e a

    kitf3 :: [C.BodyMap c] -> C.BodyMap c
    kitf3 bmaps bo1 =
        do let [bmap2] = bmaps
           bo2 <- bmap2 bo1
           Right $ test (toSet bo2) `filter` bo1

    toSet = Set.fromList . map (D.ssRShare lr)
    test b2set cs1 = D.ssLShare lr cs1 `Set.member` b2set == which

relkitFilterMeet _ _ _ _ = Right C.relkitNothing


-- ----------------------  sub

-- | Construct relmap for subrelation filter.
consSub :: (Ord c) => C.RopCons c
consSub med =
    do rmap <- Rop.getRelmap med "-relmap"
       sh   <- Rop.getMaybe Rop.getTerms med "-share"
       Right $ relmapSub med sh rmap

-- | Relmap for subrelation filter.
relmapSub :: (Ord c) => C.Intmed c -> Rop.SharedTerms -> O.Map (C.Relmap c)
relmapSub med sh = C.relmapBinary med $ relkitSub sh

-- | Calculate subrelation filter.
relkitSub :: (Ord c) => Rop.SharedTerms -> C.RelkitBinary c
relkitSub sh kit2@(C.RelkitOutput he2 _) he1'@(Just he1)
    | he1 `D.isSuperhead` he2 = kit
    | otherwise = case Rop.unmatchShare sh lr of
                    Nothing     -> Right $ C.relkitJust he1 $ C.RelkitConst []
                    Just (e, a) -> Msg.unmatchShare e a
    where
      lr  = D.termPicker he1 he2
      kit = relkitFilterMeet True sh kit2 he1'

relkitSub _ _ _ = Right C.relkitNothing



-- ----------------------  compose

-- | Construct relmap for relational composition.
consCompose :: (Ord c) => C.RopCons c
consCompose med =
    do rmap <- Rop.getRelmap med "-relmap"
       sh   <- Rop.getMaybe Rop.getTerms med "-share"
       Right $ relmapCompose med sh rmap

-- | Relational composition.
relmapCompose :: (Ord c) => C.Intmed c -> Rop.SharedTerms -> O.Map (C.Relmap c)
relmapCompose med sh = C.relmapBinary med $ relkitCompose Rop.relkitMeet sh

-- | Calculate relational composition.
relkitCompose :: forall c. (Ord c) => (Rop.SharedTerms -> C.RelkitBinary c) -> Rop.SharedTerms -> C.RelkitBinary c
relkitCompose m sh kit2@(C.RelkitOutput he2 _) (Just he1) =
    do kitMeet <- m sh kit2 (Just he1)
       kitCut  <- Rop.relkitCut (sharedNames he1 he2) (C.relkitOutput kitMeet)
       Right (kitMeet O.++ kitCut)
relkitCompose _ _ _ _ = Right C.relkitNothing

sharedNames :: D.Head -> D.Head -> [S.TermName]
sharedNames he1 he2 = shared where
    ns1     = D.getTermNames he1
    ns2     = D.getTermNames he2
    lr      = D.termPicker ns1 ns2
    shared  = D.ssRShare lr ns2
