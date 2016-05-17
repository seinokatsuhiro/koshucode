{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Rop.Flat.Control
  ( ropsControl,
  
    -- * if
    consIf, relmapIf, relkitIf,
  
    -- * when & unless
    consWhen, consUnless,
  
    -- * fix & fix-join
    consFix, consFixJoin,
  
    -- * equal
    consEqual, relmapEqual, relkitEqual,
  ) where

import qualified Koshucode.Baala.Base        as B
import qualified Koshucode.Baala.Data        as D
import qualified Koshucode.Baala.Core        as C
import qualified Koshucode.Baala.Rop.Base    as Op
import qualified Koshucode.Baala.Rop.Flat.Lattice as Op
import qualified Koshucode.Baala.Rop.Flat.Message as Msg


-- | Implementation of relational operators.
ropsControl :: (D.CContent c) => [C.Rop c]
ropsControl = Op.ropList "control"
    --        CONSTRUCTOR   USAGE          ATTRIBUTE
    [ Op.def  consEqual     "equal"        "-relmap/"
    , Op.def  consFix       "fix R"        "-relmap/"
    , Op.def  consFixJoin   "fix-join R"   "-relmap/"
    , Op.def  consIf        "if T A B"     "-test/ -then/ -else/"
    , Op.def  consUnless    "unless T B"   "-test/ -else/"
    , Op.def  consWhen      "when T A"     "-test/ -then/"
    ]


-- ----------------------  if

--  'if T A B' is same as 'A' when T is an empty relation
--  or same as 'B' when T is not an empty relation.

consIf :: (Ord c) => C.RopCons c
consIf med =
  do mt <- Op.getRelmap med "-test"
     ma <- Op.getRelmap med "-then"
     mb <- Op.getRelmap med "-else"
     Right $ relmapIf med (mt, ma, mb)

type Relmap3 c = (C.Relmap c, C.Relmap c, C.Relmap c)

relmapIf :: (Ord c) => C.Intmed c -> Relmap3 c -> C.Relmap c
relmapIf med (mt, ma, mb) = C.relmapConfl med relkitIf [mt, ma, mb]

relkitIf :: (Ord c) => C.RelkitConfl c
relkitIf [C.Relkit _ _ kitbT, C.Relkit _ (Just heA) kitbA, C.Relkit _ (Just heB) kitbB] _
    | D.headEquiv heA heB = Right $ kit3
    | otherwise = Msg.diffHead [heA, heB]
    where
      kit3 = C.relkitJust heA $ C.RelkitAbFull True kitf3 [kitbT, kitbA, kitbB]
      kitf3 bmaps bo1 =
          do let [bmapT, bmapA, bmapB] = bmaps
             boT <- bmapT bo1
             case boT of
               [] -> align $ bmapB bo1
               _  -> bmapA bo1
      align :: B.Map (B.Ab [[c]])
      align = fmap $ D.bodyAlign heA heB

relkitIf [kitT@(C.Relkit _ _ _), kitA@(C.Relkit hiA' hoA' kitbA), kitB@(C.Relkit hiB' hoB' kitbB)] _
    | isNothing2 hoA' hoB' = Right C.relkitNothing
    | isNothing hoA'       = relkitIf [kitT, C.Relkit hiB' hoB' kitbA, kitB] Nothing
    | isNothing hoB'       = relkitIf [kitT, kitA, C.Relkit hiA' hoA' kitbB] Nothing
relkitIf _ _ = Msg.unexpAttr "if T A b"

isNothing :: Maybe D.Head -> Bool
isNothing = (== Nothing)

isNothing2 :: Maybe D.Head -> Maybe D.Head -> Bool
isNothing2 a b = isNothing a && isNothing b



-- ----------------------  when & unless

consWhen :: (Ord c) => C.RopCons c
consWhen med =
  do rt <- Op.getRelmap med "-test"
     ra <- Op.getRelmap med "-then"
     Right $ relmapIf med (rt, ra, C.relmapId)

consUnless :: (Ord c) => C.RopCons c
consUnless med =
  do rt <- Op.getRelmap med "-test"
     rb <- Op.getRelmap med "-else"
     Right $ relmapIf med (rt, C.relmapId, rb)



-- ----------------------  fix & fix-join

consFix :: (Ord c) => C.RopCons c
consFix med =
  do rmap <- Op.getRelmap med "-relmap"
     Right $ relmapFix med rmap

consFixJoin :: (Ord c) => C.RopCons c
consFixJoin med =
  do rmap <- Op.getRelmap med "-relmap"
     Right $ relmapFix med (Op.relmapJoin med Nothing rmap)

relmapFix :: (Ord c) => C.Intmed c -> B.Map (C.Relmap c)
relmapFix med = C.relmapBinary med relkitFix

relkitFix :: forall c. (Ord c) => C.RelkitBinary c
relkitFix (C.Relkit _ (Just he2) kitb2) (Just he1)
    | D.headEquiv he1 he2 = Right $ kit3
    | otherwise = Msg.diffHead [he1, he2]
    where
      kit3 = C.relkitJust he1 $ C.RelkitAbFull True kitf3 [kitb2]
      kitf3 bmaps = let [bmap2] = bmaps
                        bmap2'  = C.bmapAlign he2 he1 bmap2
                    in C.fixedRelation bmap2'
relkitFix _ _ = Right C.relkitNothing


-- ----------------------  equal

consEqual :: (Ord c) => C.RopCons c
consEqual med =
    do rmap <- Op.getRelmap med "-relmap"
       Right $ relmapEqual med rmap

relmapEqual :: (Ord c) => C.Intmed c -> B.Map (C.Relmap c)
relmapEqual med = C.relmapBinary med relkitEqual

relkitEqual :: (Ord c) => C.RelkitBinary c
relkitEqual (C.Relkit _ (Just he2) kitb2) (Just he1) = Right kit3 where
    kit3 = C.relkitJust D.headEmpty $ C.RelkitAbFull False kitf3 [kitb2]
    kitf3 bmaps bo1 =
        do let [bmap2] = bmaps
           bo2 <- bmap2 bo1
           Right $ if D.Rel he1 bo1 == D.Rel he2 bo2
                   then [[]] else []
relkitEqual _ _ = Right C.relkitNothing
