{-# OPTIONS_GHC -Wall #-}

{-| Fundamental operators in relational algebra.
 
    Tropashko's relational lattice is a kind of relational algebra.
    Relational algebra is an algebraic formulation for relational model.
    In constrast to Codd's original relational algebra,
    Tropashko lattice is in more conventional and strict ways.
    The lattice has fundamental operators from which
    other operators are derived. -}

module Koshucode.Baala.Minimal.Tropashko
( -- * Fundamental operators
  -- $FundamentalOperators

  -- * Meet (Natural join)
  ropConsMeet,
  relmapMeet,
  relMeet,
  -- $MeetImplementation

  -- * Join (Inner union)
  ropConsJoin,
  relmapJoin,
  relJoin,
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C
import Koshucode.Baala.Builtin



-- ----------------------  Meet

ropConsMeet :: (Ord c) => C.RopCons c
ropConsMeet use =
  do m <- getRelmap use
     Right $ relmapMeet use m

{-| Meet two relations. -}
relmapMeet :: (Ord c)
    => C.RopUse c     -- ^ Source infomation
    -> C.Relmap c     -- ^ Subrelmap of meet operator
    -> C.Relmap c     -- ^ Relmap of meet operator
relmapMeet use m = C.relmapConfl use "meet" sub [m] where
    sub [r2] r1 = relMeet r1 r2
    sub _ _ = B.bug

{-| Meet two relations. -}
relMeet :: (Ord c)
    => B.Rel c           -- ^ Input relation /R1/
    -> B.Rel c           -- ^ Input relation /R2/
    -> B.AbOr (B.Rel c)  -- ^ Meet of /R1/ and /R2/
relMeet (B.Rel h1 b1) (B.Rel h2 b2) = Right $ B.Rel h3 b3 where
    share1, share2 :: [B.TermPos]
    share1    =  h1 `B.posOf` shared
    share2    =  h2 `B.posOf` shared
    shared    =  B.termsInner $ h1 `B.posFrom` h2

    pick1,  pick2 :: B.Map [c]
    pick1     =  B.csPick share1
    pick2     =  B.csPick share2
    cut2      =  B.csCut  share2

    h3        =  mappend h2 h1
    b3        =  concatMap meet b1
    meet cs1  =  case B.lookupMap (pick1 cs1) m2 of
                   Just css2  ->  map (++ cs1) css2
                   Nothing    ->  []
    m2        =  B.gatherToMap $ map kv b2
    kv cs2    =  ( pick2 cs2,  -- key is shared cs
                   cut2  cs2 ) -- value is side cs



-- ----------------------  Join

ropConsJoin :: (Ord c) => C.RopCons c
ropConsJoin use =
    do m <- getRelmap use
       Right $ relmapJoin use m

{-| Join two relations. -}
relmapJoin
    :: (Ord c)
    => C.RopUse c     -- ^ Source infomation
    -> C.Relmap c     -- ^ Subrelmap of join operator
    -> C.Relmap c     -- ^ Relmap of join operator
relmapJoin use m = C.relmapConfl use "join" sub [m] where
    sub [r2] r1 = relJoin r1 r2
    sub _ _ = B.bug

{-| Join two relations. -}
relJoin :: (Ord c)
    => B.Rel c           -- ^ Input relation /R1/
    -> B.Rel c           -- ^ Input relation /R2/
    -> B.AbOr (B.Rel c)  -- ^ Join of /R1/ and /R2/
relJoin (B.Rel h1 b1) (B.Rel h2 b2) = Right $ B.Rel h3 b3 where
    share1, share2 :: [B.TermPos]
    share1  =  h1 `B.posOf` shared
    share2  =  h2 `B.posOf` shared
    shared  =  B.termsInner $ h1 `B.posFrom` h2

    pick1, pick2 :: B.Map [c]
    pick1   =  B.csPick share1
    pick2   =  B.csPick share2

    h3      =  B.headChange pick1 h1
    b3      =  B.unique $
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

