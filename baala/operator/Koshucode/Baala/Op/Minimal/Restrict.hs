{-# OPTIONS_GHC -Wall #-}

{-| Restrict by relmap -}

module Koshucode.Baala.Op.Minimal.Restrict
( -- * some
  consSome, relmapSome, relkitSome,
  -- * none
  consNone, relmapNone, relkitNone,
  -- * sub
  consSub, relmapSub, relkitSub,
  -- * equal
  consEqual, relmapEqual, relkitEqual,
) where

import qualified Koshucode.Baala.Base    as B
import qualified Koshucode.Baala.Core    as C
import qualified Koshucode.Baala.Op.Builtin           as Op
import qualified Koshucode.Baala.Op.Minimal.Tropashko as Op



-- ----------------------  some

consSome :: (Ord c) => C.RopCons c
consSome use = 
    do rmap <- Op.getRelmap use
       Right $ relmapSome use rmap

relmapSome :: (Ord c) => C.RopUse c -> B.Map (C.Relmap c)
relmapSome use = C.relmapBinary use relkitSome

relkitSome :: (Ord c) => C.RelkitBinary c
relkitSome = relkitSemi False

relkitSemi :: (Ord c) => Bool -> C.RelkitBinary c
relkitSemi isEmpty (C.Relkit _ kitb2) he1 =
    Right $ C.relkit he1 $ C.RelkitAbSemi p kitb2
    where p bo2 = Right $ null bo2 == isEmpty



-- ----------------------  none

consNone :: (Ord c) => C.RopCons c
consNone use =
    do rmap <- Op.getRelmap use
       Right $ relmapNone use rmap

relmapNone :: (Ord c) => C.RopUse c -> B.Map (C.Relmap c)
relmapNone use = C.relmapBinary use relkitNone

relkitNone :: (Ord c) => C.RelkitBinary c
relkitNone = relkitSemi True



-- ----------------------  sub

consSub :: (Ord c) => C.RopCons c
consSub use =
    do rmap <- Op.getRelmap use
       Right $ relmapSub use rmap

relmapSub :: (Ord c) => C.RopUse c -> B.Map (C.Relmap c)
relmapSub use = C.relmapBinary use relkitSub

relkitSub :: (Ord c) => C.RelkitBinary c
relkitSub kit2@(C.Relkit (Just he2) _) he1'@(Just he1)
    | B.isSuperhead he1 he2 = kit
    | otherwise = Right $ C.relkitJust he1 $ C.RelkitConst []
    where
      kit = do kit3 <- Op.relkitMeet kit2 he1'
               kit4 <- relkitSome kit3 he1'
               Right kit4
relkitSub _ _ = Right C.relkitNothing



-- ----------------------  equal

consEqual :: (Ord c) => C.RopCons c
consEqual use =
    do rmap <- Op.getRelmap use
       Right $ relmapEqual use rmap

relmapEqual :: (Ord c) => C.RopUse c -> B.Map (C.Relmap c)
relmapEqual use = C.relmapBinary use relkitEqual

relkitEqual :: (Ord c) => C.RelkitBinary c
relkitEqual (C.Relkit (Just he2) kitb2) (Just he1) = Right kit3 where
    kit3 = C.relkitJust B.headEmpty $ C.RelkitAbFull False kitf3 [kitb2]
    kitf3 bmaps bo1 =
        do let [bmap2] = bmaps
           bo2 <- bmap2 bo1
           Right $ if B.Rel he1 bo1 == B.Rel he2 bo2
                   then [[]] else []
relkitEqual _ _ = Right C.relkitNothing

