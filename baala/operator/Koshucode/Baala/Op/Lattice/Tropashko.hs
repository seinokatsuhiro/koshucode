{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Fundamental operators in relational algebra.
--
--   Tropashko's relational lattice is a kind of relational algebra.
--   Relational algebra is an algebraic formulation for relational model.
--   In constrast to Codd's original relational algebra,
--   Tropashko lattice is in more conventional and strict ways.
--   The lattice has fundamental operators from which
--   other operators are derived.

module Koshucode.Baala.Op.Lattice.Tropashko
( -- * Fundamental operators
  -- $FundamentalOperators

  -- * meet (natural join)
  consMeet,
  relmapMeet,
  relkitMeet,
  -- $MeetImplementation

  -- * join (inner union)
  consJoin,
  relmapJoin,
  relmapJoinList,
  relkitJoin,
) where

import qualified Koshucode.Baala.Base       as B
import qualified Koshucode.Baala.Core       as C
import qualified Koshucode.Baala.Op.Builtin as Op



-- ----------------------  meet

consMeet :: (Ord c) => C.RopCons c
consMeet use =
  do rmap <- Op.getRelmap use
     Right $ relmapMeet use rmap

-- | Meet two relations.
relmapMeet :: (Ord c)
    => C.RopUse c     -- ^ Source infomation
    -> C.Relmap c     -- ^ Subrelmap of meet operator
    -> C.Relmap c     -- ^ Relmap of meet operator
relmapMeet use = C.relmapBinary use relkitMeet

-- | Meet two relations.
relkitMeet :: forall c. (Ord c) => C.RelkitBinary c
relkitMeet (C.Relkit (Just he2) kitb2) (Just he1) = Right kit3 where

    ind1, ind2 :: [Int]
    (ind1, ind2) = B.headNames he1 `B.snipPair` B.headNames he2

    share1, share2, right2 :: B.Map [c]
    share1 = B.snipFrom ind1
    share2 = B.snipFrom ind2
    right2 = B.snipOff  ind2

    kv cs2 = (share2 cs2, right2 cs2) -- shared and side contents

    he3    = he2 `B.mappend` he1
    kit3   = C.relkitJust he3 $ C.RelkitAbFull False kitf3 [kitb2]
    kitf3 :: [C.Relbmap c] -> C.Relbmap c
    kitf3 bmaps bo1 =
        do let [bmap2] = bmaps
           bo2 <- bmap2 bo1
           let b2map = B.gatherToMap $ map kv bo2
           Right $ step b2map `concatMap` bo1

    step b2map cs1 = case B.lookupMap (share1 cs1) b2map of
                       Just b2side -> map (++ cs1) b2side
                       Nothing     -> []

relkitMeet _ _ = Right C.relkitNothing



-- ----------------------  join

consJoin :: (Ord c) => C.RopCons c
consJoin use =
    do rmap <- Op.getRelmap use
       Right $ relmapJoin use rmap

-- | Join two relations.
relmapJoin
    :: (Ord c)
    => C.RopUse c     -- ^ Source infomation
    -> C.Relmap c     -- ^ Subrelmap of join operator
    -> C.Relmap c     -- ^ Relmap of join operator
relmapJoin use = C.relmapBinary use relkitJoin

relmapJoinList :: (Ord c) => C.RopUse c -> [C.Relmap c] -> C.Relmap c
relmapJoinList use [] = C.relmapConst use B.reldau
relmapJoinList _ [rmap] = rmap
relmapJoinList use (rmap : rmaps) = rmap `B.mappend` rmaps' where
    rmaps' = relmapJoin use $ relmapJoinList use rmaps

-- | Join two relations.
relkitJoin :: C.RelkitBinary c
relkitJoin (C.Relkit (Just he2) kitb2) (Just he1) = Right kit3 where

    ind1, ind2 :: [Int]
    (ind1, ind2) = B.headNames he1 `B.snipPair` B.headNames he2

    share1, share2 :: B.Map [c]
    share1 = B.snipFrom ind1
    share2 = B.snipFrom ind2

    he3    =  B.headChange share1 he1
    kit3   =  C.relkitJust he3 $ C.RelkitAbFull True kitf3 [kitb2]
    kitf3 bmaps bo1 =
        do let [bmap2] = bmaps
           bo2 <- bmap2 bo1
           Right $ map share1 bo1 ++ map share2 bo2

relkitJoin _ _ = Right C.relkitNothing




-- ------------------------------------------------------------------
-- $FundamentalOperators
--
--    In lattice theory, there are two operators /meet/ and /join/.
--
--    [R1 ^ R2]  Meet of /R1/ and /R2/.
--               This operator is one of generalized intersection,
--               as it were, a more-speaking-and-less-mentioning operation.
--               ''Meet'' is same as ''natural join'' in SQL.
--               /R1/ tuple is meetable to /R2/ tuple if and only if
--               shared terms of /R1/ tuple and /R2/ tuple
--               are same contents.
--
--    [R1 v R2]  Join of /R1/ and /R2/.
--               This operator is one of generalized union,
--               as it were, a less-speaking-and-more-mentioning operation.
--               Toropashko calls it ''inner union''.
--               Join of /R1/ and /R2/ is ordinary union of
--               shared-term-projection tuples in /R1/ and /R2/.
--

-- ------------------------------------------------------------------
-- $MeetImplementation
--
--    Summary of calculating meet of relations like
--    @(\/a \/b \/c)@ meet @(\/b \/c \/d)@ = @(\/d ++ \/a \/b \/c)@.
--    In this case, shared terms are @\/b@ and @\/c@,
--    left-sided term is @\/a@, and right-sided term is @\/d@.
--
--    [Input]    Relations @(Rel he1 bo1)@ and @(Rel he2 bo2)@
--
--    [Output]   Relation @(Rel he3 b3)@
--
--    1. Let @s1@ be shared-term information of @he1@.
--
--    2. Let @s2@ be shared-term information of @he2@.
--
--    3. Let @cs1@ :: @[c]@ be element of @bo1@.
--
--    4. Let @cs2@ :: @[c]@ be element of @bo2@.
--
--    5. Split each @cs2@, by @s2@,
--       into shared and sided terms :: @([c], [c])@.
--
--    6. For each same shared terms, collect the sided terms.
--       Let it be @(share2, sides2)@ :: @([c], [[c]])@.
--
--    7. Pick shared terms @share1@ :: @[c]@,
--       by @s1@, from each @cs1@.
--
--    8. Concat @cs1@ and each of @sides2@
--       where @share1@ equals @share2@,
--

