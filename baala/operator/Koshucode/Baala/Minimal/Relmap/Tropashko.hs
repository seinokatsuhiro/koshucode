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
  relopMeet,
  relmapMeet,
  relMeet,
  -- $MeetImplementation

  -- * Join (Inner union)
  relopJoin,
  relmapJoin,
  relJoin,
) where

import Koshucode.Baala.Base
import Koshucode.Baala.Builtin as Kit



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
relMeet :: (Ord c)
    => Rel c         -- ^ Input relation /R1/
    -> Rel c         -- ^ Input relation /R2/
    -> AbOr (Rel c)  -- ^ Meet of /R1/ and /R2/
relMeet (Rel h1 b1) (Rel h2 b2) = Right $ Rel h3 b3 where
    share1, share2 :: [TermPos]
    share1    =  h1 `Kit.posOf` shared
    share2    =  h2 `Kit.posOf` shared
    shared    =  Kit.termsInner $ h1 `Kit.posFrom` h2

    pick1,  pick2 :: Map [c]
    pick1     =  Kit.csPick share1
    pick2     =  Kit.csPick share2
    cut2      =  Kit.csCut  share2

    h3        =  Kit.mappend h2 h1
    b3        =  concatMap meet b1
    meet cs1  =  case lookupMap (pick1 cs1) m2 of
                   Just css2  ->  map (++ cs1) css2
                   Nothing    ->  []
    m2        =  gatherToMap $ map kv b2
    kv cs2    =  ( pick2 cs2,  -- key is shared cs
                   cut2  cs2 ) -- value is side cs



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
    share1, share2 :: [TermPos]
    share1  =  h1 `Kit.posOf` shared
    share2  =  h2 `Kit.posOf` shared
    shared  =  Kit.termsInner $ h1 `Kit.posFrom` h2

    pick1,  pick2 :: Map [c]
    pick1   =  Kit.csPick share1
    pick2   =  Kit.csPick share2

    h3      =  Kit.headChange pick1 h1
    b3      =  unique $
               map pick1 b1 ++
               map pick2 b2



-- ----------------------
{- $FundamentalOperators

   In lattice theory, there are two operators /meet/ and /join/.

   [R1 ^ R2]  Meet of /R1/ and /R2/.
              This operator is one of generalized intersection.
              ''Meet'' is same as ''natural join'' in SQL.
              /R1/ tuple is meetable to /R2/ tuple if and only if
              shared terms of /R1/ tuple and /R2/ tuple
              are same contents.

   [R1 v R2]  Join of /R1/ and /R2/.
              This operator is one of generalized union.
              Toropashko calls it ''inner union''.
              Join of /R1/ and /R2/ is ordinary union of
              shared-term-projection tuples in /R1/ and /R2/.

-}

-- ----------------------
{- $MeetImplementation

   Summary of calculating meet of relations like
   @(\/a \/b \/c)@ meet @(\/b \/c \/d)@ = @(\/d ++ \/a \/b \/c)@.
   In this case, shared terms are @\/b@ and @\/c@,
   left-sided term is @\/a@, and right-sided term is @\/d@.

   [Input]    Relations @(Rel h1 b1)@ and @(Rel h2 b2)@

   [Output]   Relation @(Rel h3 b3)@

   1. Let @s1@ be shared-term information of @h1@.

   2. Let @s2@ be shared-term information of @h2@.

   3. Let @cs1@ :: @[c]@ be element of @b1@.

   4. Let @cs2@ :: @[c]@ be element of @b2@.

   5. Split each @cs2@, by @s2@,
      into shared and sided terms :: @([c], [c])@.

   6. For each same shared terms, collect the sided terms.
      Let it be @(share2, sides2)@ :: @([c], [[c]])@.

   7. Pick shared terms @share1@ :: @[c]@,
      by @s1@, from each @cs1@.

   8. Concat @cs1@ and each of @sides2@
      where @share1@ equals @share2@,

-}

