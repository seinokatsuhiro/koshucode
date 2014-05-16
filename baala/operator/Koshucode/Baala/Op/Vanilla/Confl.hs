{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Vanilla.Confl
( 
  -- * both
  consBoth, relmapBoth,
  -- * compose
  consCompose, relmapCompose, relkitCompose,
  -- * maybe
  consMaybe, relmapMaybe, relkitMaybe,
  -- * if
  consIf, relmapIf, relkitIf,
  -- * when & unless
  consWhen, consUnless,
  -- * fix & fix-join
  consFix,
  consFixJoin,
  -- * repeat
  consRepeat,
) where

import qualified Koshucode.Baala.Base       as B
import qualified Koshucode.Baala.Core       as C
import qualified Koshucode.Baala.Op.Builtin as Op
import qualified Koshucode.Baala.Op.Minimal as Op
import qualified Koshucode.Baala.Op.Message as Message



-- ----------------------  both

consBoth :: (Ord c, C.CRel c, C.CNil c) => C.RopCons c
consBoth use =
    do rmap <- Op.getRelmap use
       Right $ relmapBoth use rmap

relmapBoth :: (Ord c, C.CRel c, C.CNil c) => C.RopUse c -> B.Map (C.Relmap c)
relmapBoth use rmap = C.relmapCopy use "i" rmapBoth where
    rmapBoth = rmapL `B.mappend` Op.relmapJoin use rmapR
    rmapR    = rmap  `B.mappend` relmapMaybe use rmapIn
    rmapL    = relmapMaybe use rmap
    rmapIn   = C.relmapWithVar use "i"



-- ----------------------  compose

consCompose :: (Ord c, C.CNil c) => C.RopCons c
consCompose use =
    do rmap <- Op.getRelmap use
       Right $ relmapCompose use rmap

relmapCompose :: (Ord c, C.CNil c) => C.RopUse c -> B.Map (C.Relmap c)
relmapCompose use = C.relmapBinary use relkitCompose

relkitCompose :: forall c. (Ord c, C.CNil c) => C.RelkitBinary c
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



-- ----------------------  maybe

consMaybe :: (Ord c, C.CNil c) => C.RopCons c
consMaybe use =
    do rmap <- Op.getRelmap use
       Right $ relmapMaybe use rmap

relmapMaybe :: (Ord c, C.CNil c) => C.RopUse c -> B.Map (C.Relmap c)
relmapMaybe use = C.relmapBinary use relkitMaybe

relkitMaybe :: forall c. (Ord c, C.CNil c) => C.RelkitBinary c
relkitMaybe (C.Relkit (Just he2) kitb2) (Just he1) = Right kit3 where

    ind1, ind2 :: [Int]
    (ind1, ind2) = B.headNames he1 `B.snipPair` B.headNames he2

    share1, share2, right2 :: B.Map [c]
    share1 = B.snipFrom ind1
    share2 = B.snipFrom ind2
    right2 = B.snipOff  ind2

    kv cs2  = (share2 cs2, right2 cs2)

    he3  = he2 `B.mappend` he1
    kit3 = C.relkitJust he3 $ C.RelkitAbFull False kitf3 [kitb2]
    kitf3 :: [C.Relbmap c] -> C.Relbmap c
    kitf3 bmaps bo1 =
        do let [bmap2] = bmaps
           bo2 <- bmap2 bo1
           let b2map = B.gatherToMap $ map kv bo2
           Right $ step b2map `concatMap` bo1

    nils = replicate (B.headDegree he3 - B.headDegree he1) C.nil
    step b2map cs1 = case B.lookupMap (share1 cs1) b2map of
                       Just b2side -> map (++ cs1) b2side
                       Nothing     -> [nils ++ cs1]

relkitMaybe _ _ = Right C.relkitNothing



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

