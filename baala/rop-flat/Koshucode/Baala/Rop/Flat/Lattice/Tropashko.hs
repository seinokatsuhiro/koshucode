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

import qualified Koshucode.Baala.Overture           as O
import qualified Koshucode.Baala.Base               as B
import qualified Koshucode.Baala.Syntax             as S
import qualified Koshucode.Baala.Data               as D
import qualified Koshucode.Baala.Core               as C
import qualified Koshucode.Baala.Rop.Base           as Rop
import qualified Koshucode.Baala.Rop.Flat.Message   as Msg


-- ----------------------  meet

-- | Construct relmap to meet relation.
consMeet :: (Ord c) => C.RopCons c
consMeet med =
  do rmap <- Rop.getRelmap med "-relmap"
     sh   <- Rop.getMaybe Rop.getTerms med "-share"
     Right $ relmapMeet med sh rmap

-- | Meet two relations.
relmapMeet :: (Ord c)
    => C.Intmed c          -- ^ Source infomation
    -> SharedTerms         -- ^ Shared terms
    -> C.Relmap c          -- ^ Subrelmap of meet operator
    -> C.Relmap c          -- ^ Relmap of meet operator
relmapMeet med sh = C.relmapBinary med $ relkitMeet sh

-- | Meet two relations.
relkitMeet :: forall c. (Ord c) => SharedTerms -> C.RelkitBinary c
relkitMeet sh (C.Relkit _ (Just he2) kitb2) (Just he1) = kit3 where
    lr     = D.termPicker he1 he2
    he3    = he2 O.++ he1
    kit3   = case unmatchShare sh lr of
               Nothing     -> Right $ C.relkitJust he3 $ C.RelkitAbFull False kitf3 [kitb2]
               Just (e, a) -> Msg.unmatchShare e a

    kitf3 :: [C.BodyMap c] -> C.BodyMap c
    kitf3 bmaps bo1 =
        do let [bmap2] = bmaps
           bo2 <- bmap2 bo1
           case D.ssLShareIndex lr of
             [] -> Right $ cartesian bo1 bo2
             _  -> let b2map = B.gatherToMap $ map (D.ssRSplit lr) bo2
                   in Right $ step b2map `concatMap` bo1

    step b2map cs1 = case D.ssLShare lr cs1 `B.lookupMap` b2map of
                       Just b2side -> map (++ cs1) b2side
                       Nothing     -> []

relkitMeet _ _ _ = Right C.relkitNothing

cartesian :: [[c]] -> [[c]] -> [[c]]
cartesian bo1 bo2 =
    do cs1 <- bo1
       cs2 <- bo2
       return $ cs2 ++ cs1


-- ----------------------  join

-- | Construct relmap to join relation.
consJoin :: (Ord c) => C.RopCons c
consJoin med =
    do rmap <- Rop.getRelmap med "-relmap"
       sh   <- Rop.getMaybe Rop.getTerms med "-share"
       Right $ relmapJoin med sh rmap

-- | Join two relations.
relmapJoin
    :: (Ord c)
    => C.Intmed c          -- ^ Source infomation
    -> SharedTerms         -- ^ Shared terms
    -> C.Relmap c          -- ^ Subrelmap of join operator
    -> C.Relmap c          -- ^ Relmap of join operator
relmapJoin med sh = C.relmapBinary med $ relkitJoin sh

-- | Join multiple relations.
relmapJoinList :: (Ord c) => C.Intmed c -> [C.Relmap c] -> C.Relmap c
relmapJoinList med [] = C.relmapConst med D.reldau
relmapJoinList _ [rmap] = rmap
relmapJoinList med (rmap : rmaps) = rmap O.++ rmaps' where
    rmaps' = relmapJoin med Nothing $ relmapJoinList med rmaps

-- | Join two relations.
relkitJoin :: SharedTerms -> C.RelkitBinary c
relkitJoin sh (C.Relkit _ (Just he2) kitb2) (Just he1) = kit3 where
    lr     = D.termPicker he1 he2
    he3    = D.ssLShare lr `D.headMap` he1
    kit3   = case unmatchShare sh lr of
               Nothing     -> Right $ C.relkitJust he3 $ C.RelkitAbFull True kitf3 [kitb2]
               Just (e, a) -> Msg.unmatchShare e a

    kitf3 :: [C.BodyMap c] -> C.BodyMap c
    kitf3 bmaps bo1 =
        do let [bmap2] = bmaps
               left    = map $ D.ssLShare lr
               right   = map $ D.ssRShare lr
           bo2 <- bmap2 bo1
           Right $ left bo1 ++ right bo2

relkitJoin _ _ _ = Right C.relkitNothing


-- ----------------------  Check shared terms

-- | Shared terms for composing check.
type SharedTerms = Maybe [S.TermName]

-- | Calculate unmatch shared terms.
unmatchShare :: SharedTerms -> D.TermPicker c -> Maybe ([S.TermName], [S.TermName])
unmatchShare (Nothing) _ = Nothing
unmatchShare (Just sh) lr =
    let e = B.setList sh
        a = B.setList $ D.ssRShareNames lr
    in if e == a
       then Nothing
       else Just (e, a)


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

