{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Control
( ropsControl,

  -- * if
  consIf, relmapIf, relkitIf,

  -- * when & unless
  consWhen, consUnless,

  -- * fix & fix-join
  consFix, consFixJoin,

  -- * repeat
  consRepeat,

  -- * equal
  consEqual, relmapEqual, relkitEqual,
) where

import qualified Koshucode.Baala.Base       as B
import qualified Koshucode.Baala.Core       as C
import qualified Koshucode.Baala.Op.Builtin as Op
import qualified Koshucode.Baala.Op.Lattice as Op
import qualified Koshucode.Baala.Op.Message as Message


-- | Implementation of relational operators.
ropsControl :: (C.CContent c) => [C.Rop c]
ropsControl = Op.ropList "control"
    --         CONSTRUCTOR   USAGE          ATTRIBUTE
    [ Op.ropI  consEqual     "equal"        "-relmap"
    , Op.ropI  consFix       "fix R"        "-relmap"
    , Op.ropI  consFixJoin   "fix-join R"   "-relmap"
    , Op.ropV  consIf        "if R ..."     "-relmap"
    , Op.ropII consRepeat    "repeat N R"   "-count -relmap"
    , Op.ropV  consUnless    "unless R R"   "-relmap"
    , Op.ropV  consWhen      "when R R"     "-relmap"
    ]


-- ----------------------  if

--  'if T A B' is same as 'A' when T is an empty relation
--  or same as 'B' when T is not an empty relation.

consIf :: (Ord c) => C.RopCons c
consIf use =
  do rmaps <- Op.getRelmaps use
     Right $ relmapIf use rmaps

relmapIf :: (Ord c) => C.RopUse c -> [C.Relmap c] -> C.Relmap c
relmapIf use = C.relmapConfl use relkitIf

relkitIf :: (Ord c) => C.RelkitConfl c
relkitIf [C.Relkit _ kitbT, C.Relkit (Just heA) kitbA, C.Relkit (Just heB) kitbB] _
    | B.headEquiv heA heB = Right $ kit3
    | otherwise = Message.diffHead [heA, heB]
    where
      kit3 = C.relkitJust heA $ C.RelkitAbFull True kitf3 [kitbT, kitbA, kitbB]
      kitf3 bmaps bo1 =
          do let [bmapT, bmapA, bmapB] = bmaps
             boT <- bmapT bo1
             case boT of
               [] -> align $ bmapB bo1
               _  -> bmapA bo1
      align :: B.Map (B.Ab [[c]])
      align = fmap (B.headAlign heA heB `map`)

relkitIf [kitT@(C.Relkit _ _), kitA@(C.Relkit heA' kitbA), kitB@(C.Relkit heB' kitbB)] _
    | isNothing2 heA' heB' = Right C.relkitNothing
    | isNothing heA'       = relkitIf [kitT, C.Relkit heB' kitbA, kitB] Nothing
    | isNothing heB'       = relkitIf [kitT, kitA, C.Relkit heA' kitbB] Nothing
relkitIf _ _ = Message.unexpAttr "if T A b"

isNothing :: Maybe B.Relhead -> Bool
isNothing = (== Nothing)

isNothing2 :: Maybe B.Relhead -> Maybe B.Relhead -> Bool
isNothing2 a b = isNothing a && isNothing b



-- ----------------------  when & unless

consWhen :: (Ord c) => C.RopCons c
consWhen use =
  do [rmapT, rmapA] <- Op.getRelmaps use
     Right $ relmapIf use [rmapT, rmapA, C.relmapId]

consUnless :: (Ord c) => C.RopCons c
consUnless use =
  do [rmapT, rmapB] <- Op.getRelmaps use
     Right $ relmapIf use [rmapT, C.relmapId, rmapB]



-- ----------------------  fix & fix-join

consFix :: (Ord c) => C.RopCons c
consFix use =
  do rmap <- Op.getRelmap use
     Right $ relmapFix use rmap

consFixJoin :: (Ord c) => C.RopCons c
consFixJoin use =
  do rmap <- Op.getRelmap use
     Right $ relmapFix use (Op.relmapJoin use rmap)

relmapFix :: (Ord c) => C.RopUse c -> B.Map (C.Relmap c)
relmapFix use = C.relmapBinary use relkitFix

relkitFix :: forall c. (Ord c) => C.RelkitBinary c
relkitFix (C.Relkit (Just he2) kitb2) (Just he1)
    | B.headEquiv he1 he2 = Right $ kit3
    | otherwise = Message.diffHead [he1, he2]
    where
      kit3 = C.relkitJust he1 $ C.RelkitAbFull True kitf3 [kitb2]
      kitf3 bmaps = let [bmap2] = bmaps
                        bmap2'  = C.bmapAlign he2 he1 bmap2
                    in C.fixedRelation bmap2'
relkitFix _ _ = Right C.relkitNothing


-- ----------------------  repeat

consRepeat :: (Ord c) => C.RopCons c
consRepeat use =
  do cnt  <- Op.getInt    use "-count"
     rmap <- Op.getRelmap use
     Right $ relmapRepeat use cnt rmap

relmapRepeat :: (Ord c) => C.RopUse c -> Int -> B.Map (C.Relmap c)
relmapRepeat use cnt = C.relmapBinary use $ relkitRepeat cnt

relkitRepeat :: forall c. (Ord c) => Int -> C.RelkitBinary c
relkitRepeat cnt (C.Relkit (Just he2) kitb2) (Just he1)
    | B.headEquiv he1 he2 = Right $ kit3
    | otherwise = Message.diffHead [he1, he2]
    where
    kit3 = C.relkitJust he1 $ C.RelkitAbFull True kitf3 [kitb2]
    kitf3 bmaps bo1 =
        do let [bmap2] = bmaps
               bmap2'  = C.bmapAlign he2 he1 bmap2
           bo2 <- rep bmap2' cnt bo1
           Right bo2

    rep bmap2' = loop where
        loop c bo | c > 0     = loop (c - 1) =<< bmap2' bo
                  | otherwise = Right bo

relkitRepeat _ _ _ = Right C.relkitNothing


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
