{-# OPTIONS_GHC -Wall #-}

-- | Fundamental operators in relational algebra.
--
--   Tropashko's relational lattice is a kind of relational algebra.
--   Relational algebra is an algebraic formulation for relational model.
--   In constrast to Codd's original relational algebra,
--   Tropashko lattice is in more conventional and strict ways.
--   The lattice has fundamental operators from which
--   other operators are derived.

module Koshucode.Baala.Minimal.Tropashko
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
  relkitJoin,
) where

import qualified Koshucode.Baala.Base    as B
import qualified Koshucode.Baala.Core    as C
import qualified Koshucode.Baala.Builtin as Rop



-- ----------------------  Meet

consMeet :: (Ord c) => C.RopCons c
consMeet use =
  do rmap <- Rop.getRelmap use
     Right $ relmapMeet use rmap

-- | Meet two relations.
relmapMeet :: (Ord c)
    => C.RopUse c     -- ^ Source infomation
    -> C.Relmap c     -- ^ Subrelmap of meet operator
    -> C.Relmap c     -- ^ Relmap of meet operator
relmapMeet use = C.relmapBinary use relkitMeet

-- | Meet two relations.
relkitMeet :: (Ord c) => C.RelkitBinary c
relkitMeet (C.Relkit (Just he2) kitb2) (Just he1) = Right kit3 where
    shared    :: [B.Termname]
    shared    =  B.posInnerNames $ he1 `B.posFrom` he2

    share1, share2 :: [B.TermPos]
    share1    =  he1 `B.posFor` shared
    share2    =  he2 `B.posFor` shared

    pick1, pick2, cut2 :: B.Map [c]
    pick1     =  B.posPick share1
    pick2     =  B.posPick share2
    cut2      =  B.posCut  share2

    he3       =  he2 `B.mappend` he1
    kv cs2    =  (pick2 cs2, cut2 cs2) -- shared contents and side contents
    kit3      =  C.relkitJust he3 $ C.RelkitOneToAbMany False kitf3 [kitb2]
    kitf3 bmaps cs1 =
        do let [bmap2] = bmaps
           bo2 <- bmap2 [cs1]
           let b2map = B.gatherToMap $ map kv bo2
           case B.lookupMap (pick1 cs1) b2map of
             Just b2side -> Right $ map (++ cs1) b2side
             Nothing     -> Right $ []

relkitMeet _ _ = Right C.relkitNothing



-- ----------------------  Join

consJoin :: (Ord c) => C.RopCons c
consJoin use =
    do rmap <- Rop.getRelmap use
       Right $ relmapJoin use rmap

-- | Join two relations.
relmapJoin
    :: (Ord c)
    => C.RopUse c     -- ^ Source infomation
    -> C.Relmap c     -- ^ Subrelmap of join operator
    -> C.Relmap c     -- ^ Relmap of join operator
relmapJoin use = C.relmapBinary use relkitJoin

-- | Join two relations.
relkitJoin :: C.RelkitBinary c
relkitJoin (C.Relkit (Just he2) kitb2) (Just he1) = Right kit3 where
    shared  :: [B.Termname]
    shared  =  B.posInnerNames $ he1 `B.posFrom` he2

    share1, share2 :: [B.TermPos]
    share1  =  he1 `B.posFor` shared
    share2  =  he2 `B.posFor` shared

    pick1, pick2 :: B.Map [c]
    pick1   =  B.posPick share1
    pick2   =  B.posPick share2

    he3     =  B.headChange pick1 he1
    kit3    =  C.relkitJust he3 $ C.RelkitAbFull True kitf3 [kitb2]
    kitf3 bmaps bo1 =
        do let [bmap2] = bmaps
           bo2 <- bmap2 bo1
           Right $ map pick1 bo1 ++ map pick2 bo2

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

