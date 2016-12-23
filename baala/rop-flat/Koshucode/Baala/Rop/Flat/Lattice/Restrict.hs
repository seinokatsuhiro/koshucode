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
import qualified Koshucode.Baala.DataPlus                   as K
import qualified Koshucode.Baala.Core                       as C
import qualified Koshucode.Baala.Rop.Base                   as Rop
import qualified Koshucode.Baala.Rop.Flat.Lattice.Tropashko as Rop
import qualified Koshucode.Baala.Rop.Flat.Term              as Rop
import qualified Koshucode.Baala.Rop.Flat.Message           as Msg


-- ----------------------  some

-- | [some /R/] Existential filter.
consSome :: (Ord c) => C.RopCons c
consSome med = 
    do rmap <- Rop.getRelmap med "-relmap"
       Right $ relmapSome med rmap

-- | Relmap of existential filter.
relmapSome :: (Ord c) => C.Intmed c -> K.Map (C.Relmap c)
relmapSome med = C.relmapBinary med relkitSome

-- | Calculate existential filter.
relkitSome :: (Ord c) => C.RelkitBinary c
relkitSome = relkitSemi False

relkitSemi :: (Ord c) => Bool -> C.RelkitBinary c
relkitSemi _ _ Nothing = C.relkitUnfixed
relkitSemi isEmpty (C.Relkit _ _ kit2) (Just he1) = kit where
    kit = Right $ C.relkitConflFilter he1 test kit2
    test bo2 = Right $ null bo2 == isEmpty


-- ----------------------  none

-- | [none /R/] Non-existential filter.
consNone :: (Ord c) => C.RopCons c
consNone med =
    do rmap <- Rop.getRelmap med "-relmap"
       Right $ relmapNone med rmap

-- | Relmap of non-existential filter.
relmapNone :: (Ord c) => C.Intmed c -> K.Map (C.Relmap c)
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
    pk     = K.termPicker he1 he2
    kit3   = case Rop.unmatchShare sh pk of
               Nothing     -> Right $ C.relkitConflWhole False he1 f [kitb2]
               Just (e, a) -> Msg.unmatchShare e a

    f :: [C.BodyMap c] -> C.BodyMap c
    f bmaps bo1 =
        do let [bmap2] = bmaps
           bo2 <- bmap2 bo1
           Right $ test (toSet bo2) `filter` bo1

    toSet = Set.fromList . map (K.pkRShare pk)
    test b2set cs1 = K.pkLShare pk cs1 `Set.member` b2set == which

relkitFilterMeet _ _ _ _ = C.relkitUnfixed


-- ----------------------  sub

-- | Construct relmap for subrelation filter.
consSub :: (Ord c) => C.RopCons c
consSub med =
    do rmap <- Rop.getRelmap med "-relmap"
       sh   <- Rop.getMaybe Rop.getTerms med "-share"
       Right $ relmapSub med sh rmap

-- | Relmap for subrelation filter.
relmapSub :: (Ord c) => C.Intmed c -> Rop.SharedTerms -> K.Map (C.Relmap c)
relmapSub med sh = C.relmapBinary med $ relkitSub sh

-- | Calculate subrelation filter.
relkitSub :: (Ord c) => Rop.SharedTerms -> C.RelkitBinary c
relkitSub sh kit2@(C.RelkitOutput he2 _) he1'@(Just he1)
    | he1 `K.isSuperhead` he2 = kit
    | otherwise = case Rop.unmatchShare sh pk of
                    Nothing     -> Right $ C.relkitConst he1 []
                    Just (e, a) -> Msg.unmatchShare e a
    where
      pk  = K.termPicker he1 he2
      kit = relkitFilterMeet True sh kit2 he1'

relkitSub _ _ _ = C.relkitUnfixed


-- ----------------------  compose

-- | Construct relmap for relational composition.
consCompose :: (Ord c) => C.RopCons c
consCompose med =
    do rmap <- Rop.getRelmap med "-relmap"
       sh   <- Rop.getMaybe Rop.getTerms med "-share"
       Right $ relmapCompose med sh rmap

-- | Relational composition.
relmapCompose :: (Ord c) => C.Intmed c -> Rop.SharedTerms -> K.Map (C.Relmap c)
relmapCompose med sh = C.relmapBinary med $ relkitCompose Rop.relkitMeet sh

-- | Calculate relational composition.
relkitCompose :: forall c. (Ord c) => (Rop.SharedTerms -> C.RelkitBinary c) -> Rop.SharedTerms -> C.RelkitBinary c
relkitCompose m sh kit2@(C.RelkitOutput he2 _) (Just he1) =
    do kitMeet <- m sh kit2 (Just he1)
       kitCut  <- Rop.relkitCut (sharedTerms he1 he2) (C.relkitOutput kitMeet)
       Right (kitMeet K.++ kitCut)
relkitCompose _ _ _ _ = C.relkitUnfixed

-- | Calculate shared terms.
--
--   >>> sharedTerms "/a /c /b" "/b /c /d"
--   [TermName EQ "c", TermName EQ "b"]
--
sharedTerms :: (K.GetTermNames t1, K.GetTermNames t2) => t1 -> t2 -> [K.TermName]
sharedTerms t1 t2 = K.pkRShareNames $ K.termPicker t1 t2
