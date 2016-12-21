{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Conditional relmaps.

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

import qualified Koshucode.Baala.Overture         as O
import qualified Koshucode.Baala.Base             as B
import qualified Koshucode.Baala.Data             as D
import qualified Koshucode.Baala.Core             as C
import qualified Koshucode.Baala.Rop.Base         as Rop
import qualified Koshucode.Baala.Rop.Flat.Lattice as Rop
import qualified Koshucode.Baala.Rop.Flat.Message as Msg


-- | Implementation of relational operators.
ropsControl :: (D.CContent c) => [C.Rop c]
ropsControl = Rop.rops "control"
    [ consEqual    O.& [ "equal"      O.& "-relmap/" ]
    , consFix      O.& [ "fix R"      O.& "-relmap/" ]
    , consFixJoin  O.& [ "fix-join R" O.& "-relmap/" ]
    , consIf       O.& [ "if T A B"   O.& "-test/ -then/ -else/" ]
    , consUnless   O.& [ "unless T B" O.& "-test/ -else/" ]
    , consWhen     O.& [ "when T A"   O.& "-test/ -then/" ]
    ]


-- ----------------------  if

--  'if T A B' is same as 'A' when T is an empty relation
--  or same as 'B' when T is not an empty relation.

-- | __if T A B__
consIf :: (Ord c) => C.RopCons c
consIf med =
  do mt <- Rop.getRelmap med "-test"
     ma <- Rop.getRelmap med "-then"
     mb <- Rop.getRelmap med "-else"
     Right $ relmapIf med (mt, ma, mb)

type Relmap3 c = (C.Relmap c, C.Relmap c, C.Relmap c)

-- | Create @if@ relmap.
relmapIf :: (Ord c) => C.Intmed c -> Relmap3 c -> C.Relmap c
relmapIf med (mt, ma, mb) = C.relmapConfl med relkitIf [mt, ma, mb]

-- | Create @if@ relkit.
relkitIf :: (Ord c) => C.RelkitConfl c
relkitIf [C.Relkit _ _ kitbT, C.RelkitOutput heA kitbA, C.RelkitOutput heB kitbB] _
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
      align :: O.Map (B.Ab [[c]])
      align = fmap $ D.bodyForward heA heB

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

-- | __when T A__
consWhen :: (Ord c) => C.RopCons c
consWhen med =
  do rt <- Rop.getRelmap med "-test"
     ra <- Rop.getRelmap med "-then"
     Right $ relmapIf med (rt, ra, C.relmapId)

-- | __unless T B__
consUnless :: (Ord c) => C.RopCons c
consUnless med =
  do rt <- Rop.getRelmap med "-test"
     rb <- Rop.getRelmap med "-else"
     Right $ relmapIf med (rt, C.relmapId, rb)



-- ----------------------  fix & fix-join

-- | __fix R__
consFix :: (Ord c) => C.RopCons c
consFix med =
  do rmap <- Rop.getRelmap med "-relmap"
     Right $ relmapFix med rmap

-- | __fix-join R__
consFixJoin :: (Ord c) => C.RopCons c
consFixJoin med =
  do rmap <- Rop.getRelmap med "-relmap"
     Right $ relmapFix med (Rop.relmapJoin med Nothing rmap)

-- | Create @fix@ relmap.
relmapFix :: (Ord c) => C.Intmed c -> O.Map (C.Relmap c)
relmapFix med = C.relmapBinary med relkitFix

-- | Create @fix@ relkit.
relkitFix :: forall c. (Ord c) => C.RelkitBinary c
relkitFix (C.RelkitOutput he2 kitb2) (Just he1)
    | D.headEquiv he1 he2 = Right $ kit3
    | otherwise = Msg.diffHead [he1, he2]
    where
      kit3 = C.relkitJust he1 $ C.RelkitAbFull True kitf3 [kitb2]
      kitf3 bmaps = let [bmap2] = bmaps
                        bmap2'  = C.bmapAlign he2 he1 bmap2
                    in C.fixedRelation bmap2'
relkitFix _ _ = Right C.relkitNothing


-- ----------------------  equal

-- | __equal R__
consEqual :: (Ord c) => C.RopCons c
consEqual med =
    do rmap <- Rop.getRelmap med "-relmap"
       Right $ relmapEqual med rmap

-- | Create @equal@ relmap.
relmapEqual :: (Ord c) => C.Intmed c -> O.Map (C.Relmap c)
relmapEqual med = C.relmapBinary med relkitEqual

-- | Create @equal@ relkit.
relkitEqual :: (Ord c) => C.RelkitBinary c
relkitEqual (C.RelkitOutput he2 kitb2) (Just he1) = Right kit3 where
    kit3 = C.relkitJust mempty $ C.RelkitAbFull False kitf3 [kitb2]
    kitf3 bmaps bo1 =
        do let [bmap2] = bmaps
           bo2 <- bmap2 bo1
           Right $ if D.Rel he1 bo1 == D.Rel he2 bo2
                   then [[]] else []
relkitEqual _ _ = Right C.relkitNothing
