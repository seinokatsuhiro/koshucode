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
consMeet med =
  do rmap <- Op.getRelmap med "-relmap"
     Right $ relmapMeet med rmap

-- | Meet two relations.
relmapMeet :: (Ord c)
    => C.Intmed c     -- ^ Source infomation
    -> C.Relmap c     -- ^ Subrelmap of meet operator
    -> C.Relmap c     -- ^ Relmap of meet operator
relmapMeet med = C.relmapBinary med relkitMeet

-- | Meet two relations.
relkitMeet :: forall c. (Ord c) => C.RelkitBinary c
relkitMeet (C.Relkit _ (Just he2) kitb2) (Just he1) = Right kit3 where
    lr     = B.headNames he1 `B.headLR` B.headNames he2
    he3    = he2 `B.mappend` he1
    kit3   = C.relkitJust he3 $ C.RelkitAbFull False kitf3 [kitb2]

    kitf3 :: [C.Relbmap c] -> C.Relbmap c
    kitf3 bmaps bo1 =
        do let [bmap2] = bmaps
           bo2 <- bmap2 bo1
           case B.headLShareIndex lr of
             [] -> Right $ cartesian bo1 bo2
             _  -> let b2map = B.gatherToMap $ map (B.headRSplit lr) bo2
                   in Right $ step b2map `concatMap` bo1

    step b2map cs1 = case B.headLShare lr cs1 `B.lookupMap` b2map of
                       Just b2side -> map (++ cs1) b2side
                       Nothing     -> []

relkitMeet _ _ = Right C.relkitNothing

cartesian :: [[c]] -> [[c]] -> [[c]]
cartesian bo1 bo2 =
    do cs1 <- bo1
       cs2 <- bo2
       return $ cs2 ++ cs1


-- ----------------------  join

consJoin :: (Ord c) => C.RopCons c
consJoin med =
    do rmap <- Op.getRelmap med "-relmap"
       Right $ relmapJoin med rmap

-- | Join two relations.
relmapJoin
    :: (Ord c)
    => C.Intmed c     -- ^ Source infomation
    -> C.Relmap c     -- ^ Subrelmap of join operator
    -> C.Relmap c     -- ^ Relmap of join operator
relmapJoin med = C.relmapBinary med relkitJoin

relmapJoinList :: (Ord c) => C.Intmed c -> [C.Relmap c] -> C.Relmap c
relmapJoinList med [] = C.relmapConst med B.reldau
relmapJoinList _ [rmap] = rmap
relmapJoinList med (rmap : rmaps) = rmap `B.mappend` rmaps' where
    rmaps' = relmapJoin med $ relmapJoinList med rmaps

-- | Join two relations.
relkitJoin :: C.RelkitBinary c
relkitJoin (C.Relkit _ (Just he2) kitb2) (Just he1) = Right kit3 where
    lr     = B.headNames he1 `B.headLR` B.headNames he2
    he3    = B.headLShare lr `B.headMap` he1
    kit3   = C.relkitJust he3 $ C.RelkitAbFull True kitf3 [kitb2]
    kitf3 bmaps bo1 =
        do let [bmap2] = bmaps
               left    = map $ B.headLShare lr
               right   = map $ B.headRShare lr
           bo2 <- bmap2 bo1
           Right $ left bo1 ++ right bo2

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

