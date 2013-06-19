{-# OPTIONS_GHC -Wall #-}

-- | Fundamental operators in relational algebra.
-- 
--   Tropashko's relational lattice is a kind of relational algebra.
--   Relational algebra is an algebraic formulation for relational model.
--   In constrast to Codd's original relational algebra,
--   Tropashko lattice is in more conventional and strict ways.
--   The lattice has fundamental operators from which
--   other operators are derived.

module Koshucode.Baala.Minimal.Relmap.Tropashko
( -- * Fundamental operators
  -- $FundamentalOperators

  -- * Naming conventions
  -- $NamingConventions

  -- * Functions on rel
  relMeet,
  relJoin,

  -- * Functions on relmap
  relmapMeet,
  relmapJoin
) where

import Koshucode.Baala.Minimal.OpeKit as Kit



-- ----------------------  Functions on relation

-- | Meet two relations.
relMeet :: (Ord v)
    => Rel v    -- ^ Input relation /R1/
    -> Rel v    -- ^ Input relation /R2/
    -> Rel v    -- ^ Meet of /R1/ and /R2/
relMeet (Rel h1 b1) (Rel h2 b2) = Rel h3 b3 where
    posh12 = Kit.headPosh h1 h2
    share1 = Kit.headPoss h1 $ Kit.possInner posh12
    share2 = Kit.headPoss h2 $ Kit.possInner posh12
    side2  = Kit.headPoss h2 $ Kit.possOuter posh12

    m2 = gatherToMap $ map pair b2
    pair arg2 = (Kit.possPick share2 arg2,
                 Kit.possPick side2  arg2)

    h3 = Kit.mappend h2 h1
    b3 = concatMap step b1
    step arg1 = case lookupMap (Kit.possPick share1 arg1) m2 of
                  Just side -> map (++ arg1) side
                  Nothing   -> []

-- | Join two relations.
relJoin :: (Ord v)
    => Rel v    -- ^ Input relation /R1/
    -> Rel v    -- ^ Input relation /R2/
    -> Rel v    -- ^ Join of /R1/ and /R2/
relJoin (Rel h1 b1) (Rel h2 b2) = Rel h3 b3 where
    posh12 = Kit.possInner $ Kit.headPosh h1 h2
    pick1  = Kit.possPick  $ Kit.headPoss h1 posh12
    pick2  = Kit.possPick  $ Kit.headPoss h2 posh12

    h3 = Kit.rehead pick1 h1
    b3 = unique $
         map pick1 b1 ++
         map pick2 b2

-- h1 = Kit.headFrom $ words "a b c"
-- h2 = Kit.headFrom $ words "c b a"
-- e3 = Kit.headPosh h1 h2
-- e4 = Kit.possInner e3
-- e5 = Kit.headPoss h1 e4
-- e6 = Kit.headPoss h2 e4
-- p7 = Kit.possPick e5
-- p8 = Kit.possPick e6
-- h9 = Kit.rehead p7 h1



-- ----------------------  Functions on relmap

-- | Meet two relations.
relmapMeet :: (Ord v)
    => Kit.OpUse v      -- ^ Source infomation
    -> Kit.Relmap v     -- ^ Subrelmap of join operator
    -> Kit.Relmap v     -- ^ Relmap of join operator
relmapMeet use m = Kit.relmapConfl use "meet" sub [m] where
    sub [r2] r1 = relMeet r1 r2
    sub _ _ = undefined

-- | Join two relations.
relmapJoin
    :: (Ord v)
    => Kit.OpUse v      -- ^ Source infomation
    -> Kit.Relmap v     -- ^ Subrelmap of join operator
    -> Kit.Relmap v     -- ^ Relmap of join operator
relmapJoin use m = Kit.relmapConfl use "join" sub [m] where
    sub [r2] r1 = relJoin r1 r2
    sub _ _ = undefined



-- ----------------------
-- $FundamentalOperators
--
-- In lattice theory, there are two operators /meet/ and /join/.
--
-- [R1 ^ R2]
-- Meet of /R1/ and /R2/.
-- This operator is one of generalized intersection.
-- ''Meet'' is same as ''natural join'' in SQL.
-- /R1/ tuple is meetable to /R2/ tuple if and only if
-- shared terms of /R1/ tuple and /R2/ tuple are same contents.
-- 
-- [R1 v R2]
-- Join of /R1/ and /R2/.
-- This operator is one of generalized union.
-- Toropashko calls it ''inner union''.
-- Join of /R1/ and /R2/ is ordinary union of
-- shared-term-projection tuples in /R1/ and /R2/.

-- ----------------------
-- $NamingConventions
--
-- Functions for relational mapping are
-- named under some conventions.
-- Relational mapping is a kind of functions
-- that calculate relations from relations.
--
-- [@relXxx@]
-- Functions from 'Rel' to 'Rel'.
-- These are basic functions for relational operators.
--
-- [@relmapXxx@]
-- Functions from 'Kit.Relmap' to 'Kit.Relmap'.
-- 
-- [@consXxx@]
-- Functions of 'Koshucode.Baala.Base.Relmap.HalfRelmap.OpCons'.
-- These functions construct 'Kit.Relmap' from operator usages,
-- but constructions are failed if operators are misused.


