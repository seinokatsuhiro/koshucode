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

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C
import qualified Koshucode.Baala.Builtin as Rop



-- ----------------------  Meet

consMeet :: (Ord c) => C.RopCons c
consMeet use =
  do m <- Rop.getRelmap use
     Right $ relmapMeet use m

-- | Meet two relations.
relmapMeet :: (Ord c)
    => C.RopUse c     -- ^ Source infomation
    -> C.Relmap c     -- ^ Subrelmap of meet operator
    -> C.Relmap c     -- ^ Relmap of meet operator
relmapMeet use = C.relmapBinary use relkitMeet

-- | Meet two relations.
relkitMeet :: (Ord c) => C.RelkitBinary c
relkitMeet (C.Relkit (Just h2) f2) (Just h1) =
    Right (C.Relkit (Just h3) f3) where
    shared    :: [B.Termname]
    shared    =  B.posInnerNames $ h1 `B.posFrom` h2

    share1, share2 :: [B.TermPos]
    share1    =  h1 `B.posFor` shared
    share2    =  h2 `B.posFor` shared

    pick1, pick2, cut2 :: B.Map [c]
    pick1     =  B.posPick share1
    pick2     =  B.posPick share2
    cut2      =  B.posCut  share2

    h3        =  h2 `B.mappend` h1
    f3        =  B.Sourced [] $ C.RelkitOneToAbMany False meet [f2]
    meet sub cs1 = do let [b2'] = sub
                      b2 <- b2'
                      let m2 = B.gatherToMap $ map kv b2
                      case B.lookupMap (pick1 cs1) m2 of
                        Just b2side -> Right $ map (++ cs1) b2side
                        Nothing -> Right $ []
    kv cs2    =  ( pick2 cs2,  -- key is shared cs
                   cut2  cs2 ) -- value is side cs
relkitMeet _ _ = Right C.relkitNothing



-- ----------------------  Join

consJoin :: (Ord c) => C.RopCons c
consJoin use =
    do m <- Rop.getRelmap use
       Right $ relmapJoin use m

-- | Join two relations.
relmapJoin
    :: (Ord c)
    => C.RopUse c     -- ^ Source infomation
    -> C.Relmap c     -- ^ Subrelmap of join operator
    -> C.Relmap c     -- ^ Relmap of join operator
relmapJoin use = C.relmapBinary use relkitJoin

-- | Join two relations.
relkitJoin :: C.RelkitBinary c
relkitJoin (C.Relkit (Just h2) f2) (Just h1) =
    Right (C.relkitJust h3 $ C.RelkitAbFull True f3 [f2]) where
    shared  :: [B.Termname]
    shared  =  B.posInnerNames $ h1 `B.posFrom` h2

    share1, share2 :: [B.TermPos]
    share1  =  h1 `B.posFor` shared
    share2  =  h2 `B.posFor` shared

    pick1, pick2 :: B.Map [c]
    pick1   =  B.posPick share1
    pick2   =  B.posPick share2

    h3      =  B.headChange pick1 h1
    f3 sub b1 = do let [g2] = sub
                   b2 <- g2 b1
                   Right $ map pick1 b1 ++ map pick2 b2
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
--    [Input]    Relations @(Rel h1 b1)@ and @(Rel h2 b2)@
--
--    [Output]   Relation @(Rel h3 b3)@
--
--    1. Let @s1@ be shared-term information of @h1@.
--
--    2. Let @s2@ be shared-term information of @h2@.
--
--    3. Let @cs1@ :: @[c]@ be element of @b1@.
--
--    4. Let @cs2@ :: @[c]@ be element of @b2@.
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

