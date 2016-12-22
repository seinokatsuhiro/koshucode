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

import qualified Koshucode.Baala.DataPlus         as K
import qualified Koshucode.Baala.Core             as C
import qualified Koshucode.Baala.Rop.Base         as Rop
import qualified Koshucode.Baala.Rop.Flat.Lattice as Rop
import qualified Koshucode.Baala.Rop.Flat.Message as Msg


-- | Implementation of relational operators.
ropsControl :: (K.CContent c) => [C.Rop c]
ropsControl = Rop.rops "control"
    [ consEqual    K.& [ "equal"      K.& "-relmap/" ]
    , consFix      K.& [ "fix R"      K.& "-relmap/" ]
    , consFixJoin  K.& [ "fix-join R" K.& "-relmap/" ]
    , consIf       K.& [ "if T A B"   K.& "-test/ -then/ -else/" ]
    , consUnless   K.& [ "unless T B" K.& "-test/ -else/" ]
    , consWhen     K.& [ "when T A"   K.& "-test/ -then/" ]
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
    | K.headEquiv heA heB = Right $ kit3
    | otherwise = Msg.diffHead [heA, heB]
    where
      kit3 = C.relkitConfl heA True confl [kitbT, kitbA, kitbB]
      confl bmaps bo1 =
          do let [bmapT, bmapA, bmapB] = bmaps
             boT <- bmapT bo1
             case boT of
               [] -> align $ bmapB bo1
               _  -> bmapA bo1
      align :: K.Map (K.Ab [[c]])
      align = fmap $ K.bodyForward heA heB

relkitIf [kitT@(C.Relkit _ _ _), kitA@(C.Relkit hiA' hoA' kitbA), kitB@(C.Relkit hiB' hoB' kitbB)] _
    | isNothing2 hoA' hoB' = Right C.relkitNothing
    | isNothing hoA'       = relkitIf [kitT, C.Relkit hiB' hoB' kitbA, kitB] Nothing
    | isNothing hoB'       = relkitIf [kitT, kitA, C.Relkit hiA' hoA' kitbB] Nothing
relkitIf _ _ = Msg.unexpAttr "if T A b"

isNothing :: Maybe K.Head -> Bool
isNothing = (== Nothing)

isNothing2 :: Maybe K.Head -> Maybe K.Head -> Bool
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
relmapFix :: (Ord c) => C.Intmed c -> K.Map (C.Relmap c)
relmapFix med = C.relmapBinary med relkitFix

-- | Create @fix@ relkit.
relkitFix :: forall c. (Ord c) => C.RelkitBinary c
relkitFix (C.RelkitOutput he2 kitb2) (Just he1)
    | K.headEquiv he1 he2 = Right $ kit3
    | otherwise = Msg.diffHead [he1, he2]
    where
      kit3 = C.relkitConfl he1 True confl [kitb2]
      confl bmaps = let [bmap2] = bmaps
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
relmapEqual :: (Ord c) => C.Intmed c -> K.Map (C.Relmap c)
relmapEqual med = C.relmapBinary med relkitEqual

-- | Create @equal@ relkit.
relkitEqual :: (Ord c) => C.RelkitBinary c
relkitEqual (C.RelkitOutput he2 kitb2) (Just he1) = Right kit3 where
    kit3 = C.relkitConfl mempty False confl [kitb2]
    confl bmaps bo1 =
        do let [bmap2] = bmaps
           bo2 <- bmap2 bo1
           Right $ if K.Rel he1 bo1 == K.Rel he2 bo2
                   then [[]] else []
relkitEqual _ _ = Right C.relkitNothing
