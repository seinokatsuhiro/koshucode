{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Restrict by relmap

module Koshucode.Baala.Op.Lattice.Restrict
  ( -- * some
    consSome, relmapSome, relkitSome,
    -- * none
    consNone, relmapNone, relkitNone,
    -- * sub
    consSub, relmapSub, relkitSub,
    -- * compose
    consCompose, relmapCompose, relkitCompose,
  ) where

import qualified Koshucode.Baala.Base                 as B
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
relkitSemi isEmpty (C.Relkit _ kitb2) he1 =
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



-- ----------------------  sub

consSub :: (Ord c) => C.RopCons c
consSub med =
    do rmap <- Op.getRelmap med "-relmap"
       Right $ relmapSub med rmap

relmapSub :: (Ord c) => C.Intmed c -> B.Map (C.Relmap c)
relmapSub med = C.relmapBinary med relkitSub

relkitSub :: (Ord c) => C.RelkitBinary c
relkitSub kit2@(C.Relkit (Just he2) _) he1'@(Just he1)
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
relkitCompose kit2@(C.Relkit (Just he2) _) (Just he1) =
    do kitMeet <- Op.relkitMeet kit2 (Just he1)
       kitCut  <- Op.relkitCut shared $ C.relkitHead kitMeet
       Right $ kitMeet `B.mappend` kitCut
    where
      ns1    = B.headNames he1
      ns2    = B.headNames he2
      ind    = B.snipIndex ns1 ns2
      shared = B.snipFrom  ind ns2
relkitCompose _ _ = Right C.relkitNothing
