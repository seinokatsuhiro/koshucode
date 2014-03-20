{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Vanilla.Confl
( 
  -- * compose
  consCompose, relmapCompose, relkitCompose,
  -- * maybe
  consMaybe, relmapMaybe, relkitMaybe,
  -- * full
  consFull, relmapFull, relkitFull,
  -- * group
  consGroup, relmapGroup, relkitGroup,
  -- * if
  consIf, relmapIf, relkitIf,
  -- * when & unless
  consWhen, consUnless,
  -- * fix & fix-join
  consFix,
  consFixJoin,
) where

import qualified Koshucode.Baala.Base       as B
import qualified Koshucode.Baala.Core       as C
import qualified Koshucode.Baala.Op.Builtin as Op
import qualified Koshucode.Baala.Op.Minimal as Op
import qualified Koshucode.Baala.Op.Message as Message



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
    pos     :: [B.TermPos]
    pos     =  he1 `B.posFrom` he2

    shared, sided :: [B.Termname]
    shared  =  B.posInnerNames pos
    sided   =  B.posOuterNames pos

    share1, share2, side2 :: [B.TermPos]
    share1  =  he1 `B.posFor` shared
    share2  =  he2 `B.posFor` shared
    side2   =  he2 `B.posFor` sided

    m2 bo2   = Right $ B.gatherToMap $ map kv bo2
    kv cs2  = ( B.posPick share2 cs2,
                B.posPick side2  cs2 )

    he3  = B.mappend he2 he1
    kit3 = C.relkitJust he3 $ C.RelkitAbFull False kitf3 [kitb2]
    kitf3 :: [C.Relbmap c] -> C.Relbmap c
    kitf3 bmaps bo1 =
        do let [bmap2] = bmaps
           bo2 <- bmap2 bo1
           m   <- m2 bo2
           Right $ concatMap (step m) bo1

    nils       = replicate (B.headDegree he3 - B.headDegree he1) C.nil
    step m cs1 = case B.lookupMap (B.posPick share1 cs1) m of
                   Just side -> map (++ cs1) side
                   Nothing   -> [nils ++ cs1]

relkitMaybe _ _ = Right C.relkitNothing


-- ----------------------  full

consFull :: (Ord c, C.CNil c) => C.RopCons c
consFull use =
    do [m1, m2] <- Op.getRelmaps use
       Right $ relmapFull use m1 m2

{-| like SQL's full join -}
relmapFull :: (Ord c, C.CNil c) => C.RopUse c
           -> C.Relmap c -> C.Relmap c -> C.Relmap c
relmapFull use m1 m2 = C.relmapConfl use fy [m1, m2] where
    fy [r1, r2] = relkitFull r1 r2
    fy _ = B.bug "relmapFull"

relkitFull :: (Ord c, C.CNil c) => C.Relkit c -> C.RelkitBinary c
relkitFull (C.Relkit he1 f1) (C.Relkit he2 f2) _ = 
    do C.Relkit h3 f3 <- relkitMaybe (C.Relkit he2 f2) he1
       C.Relkit h4 f4 <- relkitMaybe (C.Relkit he1 f1) he2

       bo1 <- C.relkitRun f1 []
       bo2 <- C.relkitRun f2 []
       bo3 <- C.relkitRun f3 bo1
       bo4 <- C.relkitRun f4 bo2
       C.Relkit he5 kit5 <- Op.relkitJoin (C.relkit h4 $ C.RelkitConst bo4) h3

       bo5 <- C.relkitRun kit5 bo3
       Right $ C.relkit he5 $ C.RelkitConst bo5



-- ----------------------  group

consGroup :: (Ord c, C.CRel c) => C.RopCons c
consGroup use =
  do n    <- Op.getTerm   use "-term"
     rmap <- Op.getRelmap use
     Right $ relmapGroup use n rmap

relmapGroup :: (Ord c, C.CRel c) => C.RopUse c -> String -> B.Map (C.Relmap c)
relmapGroup use = C.relmapBinary use . relkitGroup

-- | Grouping relation.
relkitGroup :: forall c. (Ord c, C.CRel c) => String -> C.RelkitBinary c
relkitGroup n (C.Relkit (Just he2) kitb2) (Just he1) = Right kit3 where
    shared     :: [B.Termname]
    shared     = B.posInnerNames $ he1 `B.posFrom` he2

    share1, share2 :: [B.TermPos]
    share1     = he1 `B.posFor` shared
    share2     = he2 `B.posFor` shared

    toMap2 bo2 = Right $ B.gatherToMap $ map kv bo2
    kv cs2     = (B.posPick share2 cs2, cs2)

    he3        = B.Nest n (B.headTerms he2) `B.headConsTerm` he1
    kit3       = C.relkitJust he3 (C.RelkitAbFull False kitf3 [kitb2])
    kitf3 bmaps bo1 =
        do let [bmap2] = bmaps
           bo2  <- bmap2 bo1
           map2 <- toMap2 bo2
           Right $ map (add map2) bo1

    add map2 cs1 =
        let b2maybe = B.lookupMap (B.posPick share1 cs1) map2
            b2sub   = B.maybeWith [] b2maybe
        in C.pRel (B.Rel he2 b2sub) : cs1

relkitGroup _ _ _ = Right C.relkitNothing



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
relkitIf [(C.Relkit _ kitbT), (C.Relkit (Just heA) kitbA), (C.Relkit (Just heB) kitbB)] _
    | B.headEquiv heA heB = Right $ kit3
    | otherwise = Message.unexpOperand $ "different headings: "
                    ++ showHead heA ++ " and " ++ showHead heB
    where
      showHead = show . B.doc
      kit3 = C.relkitJust heA $ C.RelkitAbFull True kitf3 [kitbT, kitbA, kitbB]
      kitf3 bmaps bo1 =
          do let [bmapT, bmapA, bmapB] = bmaps
             boT <- bmapT bo1
             case boT of
               [] -> align $ bmapB bo1
               _  -> bmapA bo1
      align :: B.Map (B.Ab [[c]])
      align = fmap (B.headArrange heA heB `map`)

relkitIf [kitT@(C.Relkit _ _), kitA@(C.Relkit heA' kitbA), kitB@(C.Relkit heB' kitbB)] _
    | isNothing2 heA' heB' = Right C.relkitNothing
    | isNothing heA'       = relkitIf [kitT, C.Relkit heB' kitbA, kitB] Nothing
    | isNothing heB'       = relkitIf [kitT, kitA, C.Relkit heA' kitbB] Nothing
relkitIf _ _ = Message.unexpOperand "if T A b"

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
relkitFix (C.Relkit (Just he2) kitb2) (Just he1) = Right kit3 where
    kit3 = C.relkitJust he1 $ C.RelkitAbFull True kitf3 [kitb2]
    kitf3 bmaps = let [bmap2] = bmaps
                      bmap2'  = C.bodyMapArrange he1 he2 bmap2
                  in C.fixedRelation bmap2'
relkitFix _ _ = Right C.relkitNothing

