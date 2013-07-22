{-# OPTIONS_GHC -Wall #-}

{-| Fundamental operators in relational algebra.
 
    Tropashko's relational lattice is a kind of relational algebra.
    Relational algebra is an algebraic formulation for relational model.
    In constrast to Codd's original relational algebra,
    Tropashko lattice is in more conventional and strict ways.
    The lattice has fundamental operators from which
    other operators are derived. -}

module Koshucode.Baala.Minimal.Relmap.Tropashko
( -- * Fundamental operators
  -- $FundamentalOperators

  -- * Meet (Natural join)
  relopMeet, relmapMeet, relMeet,

  -- * Join (Inner union)
  relopJoin, relmapJoin, relJoin,
) where

import Koshucode.Baala.Base.Abort
import Koshucode.Baala.Minimal.OpKit as Kit
import Koshucode.Baala.Minimal.Relmap.Get


-- ----------------------  Meet

relopMeet :: (Ord v) => Relop v
relopMeet use = do
  m <- getRelmap use
  Right $ relmapMeet use m

{-| Meet two relations. -}
relmapMeet :: (Ord v)
    => Kit.OpUse v      -- ^ Source infomation
    -> Kit.Relmap v     -- ^ Subrelmap of meet operator
    -> Kit.Relmap v     -- ^ Relmap of meet operator
relmapMeet use m = Kit.relmapConfl use "meet" sub [m] where
    sub [r2] r1 = relMeet r1 r2
    sub _ _ = undefined

{-| Meet two relations. -}
relMeet :: (Ord v)
    => Rel v         -- ^ Input relation /R1/
    -> Rel v         -- ^ Input relation /R2/
    -> AbOr (Rel v)  -- ^ Meet of /R1/ and /R2/
relMeet (Rel h1 b1) (Rel h2 b2) = Right $ Rel h3 b3 where
    posh12 = h1 `Kit.posFrom` h2
    share1 = Kit.headPoss h1 $ Kit.termsInner posh12
    share2 = Kit.headPoss h2 $ Kit.termsInner posh12
    side2  = Kit.headPoss h2 $ Kit.termsOuter posh12

    m2 = gatherToMap $ map pair b2
    pair arg2 = (Kit.possPick share2 arg2,
                 Kit.possPick side2  arg2)

    h3 = Kit.mappend h2 h1
    b3 = concatMap step b1
    step arg1 = case lookupMap (Kit.possPick share1 arg1) m2 of
                  Just side -> map (++ arg1) side
                  Nothing   -> []




-- ----------------------  Join

{-| Join two relations. -}
relmapJoin
    :: (Ord v)
    => Kit.OpUse v      -- ^ Source infomation
    -> Kit.Relmap v     -- ^ Subrelmap of join operator
    -> Kit.Relmap v     -- ^ Relmap of join operator
relmapJoin use m = Kit.relmapConfl use "join" sub [m] where
    sub [r2] r1 = relJoin r1 r2
    sub _ _ = undefined

relopJoin :: (Ord v) => Relop v
relopJoin use = do
  m <- getRelmap use
  Right $ relmapJoin use m

{-| Join two relations. -}
relJoin :: (Ord v)
    => Rel v         -- ^ Input relation /R1/
    -> Rel v         -- ^ Input relation /R2/
    -> AbOr (Rel v)  -- ^ Join of /R1/ and /R2/
relJoin (Rel h1 b1) (Rel h2 b2) = Right $ Rel h3 b3 where
    posh12 = Kit.termsInner $ h1 `Kit.posFrom` h2
    pick1  = Kit.possPick  $ Kit.headPoss h1 posh12
    pick2  = Kit.possPick  $ Kit.headPoss h2 posh12

    h3 = Kit.headChange pick1 h1
    b3 = unique $
         map pick1 b1 ++
         map pick2 b2



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

