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

module Koshucode.Baala.Rop.Flat.Lattice.Tropashko
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

    -- * Check shared terms
    SharedTerms,
    unmatchShare,
  ) where

import qualified Koshucode.Baala.DataPlus           as K
import qualified Koshucode.Baala.Core               as C
import qualified Koshucode.Baala.Rop.Base           as Rop
import qualified Koshucode.Baala.Rop.Flat.Message   as Msg


-- ----------------------  meet

-- | [meet /R/ &#x5B;-share \/P ...&#x5D;]
--     Meet input relation and /R/.
consMeet :: (Ord c) => C.RopCons c
consMeet med =
  do rmap <- Rop.getRelmap med "-relmap"
     sh   <- Rop.getMaybe Rop.getTerms med "-share"
     Right $ relmapMeet med sh rmap

-- | Meet two relations.
relmapMeet :: (Ord c)
    => C.Intmed c          -- ^ Intermediate relmap
    -> SharedTerms         -- ^ Shared terms from @-share@ option
    -> C.Relmap c          -- ^ Subrelmap /R/
    -> C.Relmap c          -- ^ Relmap of meet operator
relmapMeet med sh = C.relmapBinary med $ relkitMeet sh

-- | Meet two relations.
relkitMeet :: forall c. (Ord c) => SharedTerms -> C.RelkitBinary c
relkitMeet sh (C.RelkitOutput he2 kitb2) (Just he1) = kit3 where
    pk     = K.termPicker he1 he2
    pick1  = K.pkLShare pk
    split2 = K.pkRSplit pk

    he3    = he2 K.++ he1
    kit3   = case unmatchShare sh pk of
               Nothing     -> Right $ C.relkitJust he3 $ C.RelkitAbFull False kitf3 [kitb2]
               Just (e, a) -> Msg.unmatchShare e a

    kitf3 :: [C.BodyMap c] -> C.BodyMap c
    kitf3 bmaps bo1 =
        do let [bmap2] = bmaps
           bo2 <- bmap2 bo1
           case K.pkDisjoint pk of
             True  -> Right $ cartesian bo1 bo2
             False -> let b2map = K.gatherToMap (split2 <$> bo2)
                      in Right $ step b2map `concatMap` bo1

    step b2map cs1 = case pick1 cs1 `K.lookupMap` b2map of
                       Just b2prop -> map (++ cs1) b2prop
                       Nothing     -> []

relkitMeet _ _ _ = Right C.relkitNothing

-- | Cartesian product.
cartesian :: [[c]] -> [[c]] -> [[c]]
cartesian bo1 bo2 =
    do cs1 <- bo1
       cs2 <- bo2
       return $ cs2 ++ cs1


-- ----------------------  join

-- | [join /R/ &#x5B;-share \/P ...&#x5D;]
--     Join input relation and /R/.
consJoin :: (Ord c) => C.RopCons c
consJoin med =
    do rmap <- Rop.getRelmap med "-relmap"
       sh   <- Rop.getMaybe Rop.getTerms med "-share"
       Right $ relmapJoin med sh rmap

-- | Join two relations.
relmapJoin
    :: (Ord c)
    => C.Intmed c          -- ^ Intermediate relmap
    -> SharedTerms         -- ^ Shared terms from @-share@ option
    -> C.Relmap c          -- ^ Subrelmap /R/
    -> C.Relmap c          -- ^ Relmap of join operator
relmapJoin med sh = C.relmapBinary med $ relkitJoin sh

-- | Join multiple relations.
relmapJoinList :: (Ord c) => C.Intmed c -> [C.Relmap c] -> C.Relmap c
relmapJoinList med [] = C.relmapConst med K.reldau
relmapJoinList _ [rmap] = rmap
relmapJoinList med (rmap : rmaps) = rmap K.++ rmaps' where
    rmaps' = relmapJoin med Nothing $ relmapJoinList med rmaps

-- | Join two relations.
relkitJoin :: SharedTerms -> C.RelkitBinary c
relkitJoin sh (C.RelkitOutput he2 kitb2) (Just he1) = kit3 where
    pk     = K.termPicker he1 he2
    pick1  = K.pkLShare pk
    pick2  = K.pkRShare pk

    he3    = pick1 `K.headMap` he1
    kit3   = case unmatchShare sh pk of
               Nothing     -> Right $ C.relkitJust he3 $ C.RelkitAbFull True kitf3 [kitb2]
               Just (e, a) -> Msg.unmatchShare e a

    kitf3 :: [C.BodyMap c] -> C.BodyMap c
    kitf3 bmaps bo1 =
        do let [bmap2] = bmaps
           bo2 <- bmap2 bo1
           Right $ map pick1 bo1 ++ map pick2 bo2

relkitJoin _ _ _ = Right C.relkitNothing


-- ----------------------  Check shared terms

-- | Shared terms for composing check.
type SharedTerms = Maybe [K.TermName]

-- | Calculate unmatch shared terms.
unmatchShare :: SharedTerms -> K.TermPicker c -> Maybe ([K.TermName], [K.TermName])
unmatchShare (Nothing) _              = Nothing
unmatchShare (Just sh) pk | e == a    = Nothing
                          | otherwise = Just (e, a)
                          where e = K.setList sh
                                a = K.setList $ K.preTerms pk


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
--    left-proper term is @\/a@, and right-proper term is @\/d@.
--
--    [Input]    Relations @(Rel head1 body1)@ and @(Rel heea2 body2)@ :: @Rel c@
--    [Output]   Relation @(Rel head3 body3)@ :: @Rel c@
--
--    1. Let @s1@ be the shared-term information of @head1@.
--
--    2. Let @s2@ be the shared-term information of @head2@.
--
--    3. Let @cs1@ :: @[c]@ be an element of @body1@.
--
--    4. Let @cs2@ :: @[c]@ be an element of @body2@.
--
--    5. Using @s2@, split each @cs2@ into
--       shared and proper terms :: @([c], [c])@.
--
--    6. For each same shared terms, collect the proper terms.
--       Let it be @(share2, propers2)@ :: @([c], [[c]])@.
--
--    7. Using @s1@, pick shared terms @share1@ :: @[c]@ from each @cs1@.
--
--    8. Concatenate each of @propers2@ and @cs1@
--       where @share2@ equals @share1@,
--

