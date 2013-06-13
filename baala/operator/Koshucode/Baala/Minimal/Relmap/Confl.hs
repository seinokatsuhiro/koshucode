{-# OPTIONS_GHC -Wall #-}

-- | Confluent relational mapping

module Koshucode.Baala.Minimal.Relmap.Confl
( -- * Nested relation
  hang, selfhang
  -- * Restriction
, relmapSome, relmapMinus
, relSome, relMinus
) where
import Koshucode.Baala.Base.Data
import Prelude hiding (maybe)
import qualified Koshucode.Baala.Base.Kit as Kit


-- ----------------------  Fundamental operators

{-
-- | Meet two relations.
--   Meeting operation is a generalized intersection.
relmapMeet :: (Ord v) => Kit.HalfRelmap -> Kit.Relmap v -> Kit.Relmap v
relmapMeet h m = Kit.confl h "meet" sub [m] where
    sub [r2] r1 = relMeet r1 r2
    sub _ _ = undefined

relMeet :: (Ord v) => Rel v -> Rel v -> Rel v
relMeet r1 r2 = Rel h3 b3 where
    Rel h1 args1 = r1
    Rel h2 args2 = r2

    posh12 = Kit.headPosh h1 h2
    share1 = Kit.headPoss h1 $ Kit.possInner posh12
    share2 = Kit.headPoss h2 $ Kit.possInner posh12
    side2  = Kit.headPoss h2 $ Kit.possOuter posh12

    m2 = Kit.gatherToMap $ map pair args2
    pair arg2 = (Kit.possPick share2 arg2,
                 Kit.possPick side2  arg2)

    h3 = Kit.mappend h2 h1
    b3 = concatMap step args1
    step arg1 = case Kit.lookupMap (Kit.possPick share1 arg1) m2 of
                  Just side -> map (++ arg1) side
                  Nothing   -> []

-- | Join two relations.
--   Joining operation is a generalized union.
relmapJoin :: (Ord v) => Kit.HalfRelmap -> Kit.Relmap v -> Kit.Relmap v
relmapJoin h m = Kit.confl h "join" sub [m] where
    sub [r2] r1 = relJoin r1 r2
    sub _ _ = undefined

-- | Join two relations.
relJoin :: (Ord v) => Rel v -> Rel v -> Rel v
relJoin r1 r2 = Rel h3 b3 where
    Rel h1 args1 = r1
    Rel h2 args2 = r2

    posh12 = Kit.possInner $ Kit.headPosh h1 h2
    pick1  = Kit.possPick $ Kit.headPoss h1 posh12
    pick2  = Kit.possPick $ Kit.headPoss h2 posh12

    h3 = Kit.rehead pick1 h1
    b3 = Kit.unique $
         map pick1 args1 ++
         map pick2 args2

-}

-- ----------------------  Nested relation

-- | Hanging relation, like grouping.
hang :: (Ord v, RelValue v)
    => Kit.HalfRelmap -> String
    -> Kit.Relmap v -> Kit.Relmap v
hang h ns rm2 = Kit.relmapConfl h "hang" (Kit.withP2 relHang ns) [rm2]

relHang :: (Ord v, RelValue v)
    => [String] -> [Rel v] -> Rel v -> Rel v
relHang [n] [r2] r1 = Rel h3 b3 where
    Rel h1 args1 = r1
    Rel h2 args2 = r2

    posh12 = Kit.headPosh h1 h2
    share1 = Kit.headPoss h1 $ Kit.possInner posh12
    share2 = Kit.headPoss h2 $ Kit.possInner posh12
    --side2  = Kit.headPoss h2 $ Kit.possOuter posh12

    m2 = Kit.gatherToMap $ map pair args2
    pair arg2 = (Kit.possPick share2 arg2, arg2)

    h3 = Relhead $ Nest n (headTerms h2) : (headTerms h1)
    b3 = map step args1
    step arg1 = case Kit.lookupMap (Kit.possPick share1 arg1) m2 of
                  Just args2' -> (relValue $ Rel h2 args2') : arg1
                  Nothing     -> []

relHang _ _ _ = undefined

selfhang :: a
selfhang = undefined



-- ----------------------  Semijoin

relmapSome :: (Ord v) => Kit.HalfRelmap -> Kit.Relmap v -> Kit.Relmap v
relmapSome h m = Kit.relmapConfl h "minus" sub [m] where
    sub [r2] r1 = relSome r1 r2
    sub _ _     = undefined

relmapMinus :: (Ord v) => Kit.HalfRelmap -> Kit.Relmap v -> Kit.Relmap v
relmapMinus h m = Kit.relmapConfl h "minus" sub [m] where
    sub [r2] r1 = relMinus r1 r2
    sub _ _     = undefined

relSome :: (Ord v) => Rel v -> Rel v -> Rel v
relSome  = relSemi True

relMinus :: (Ord v) => Rel v -> Rel v -> Rel v
relMinus = relSemi False

relSemi :: (Ord v) => Bool -> Rel v -> Rel v -> Rel v
relSemi which r1 r2 = Rel h3 b3 where
    Rel h1 args1 = r1
    Rel h2 args2 = r2

    posh12 = Kit.headPosh h1 h2
    share1 = Kit.headPoss h1 $ Kit.possInner posh12
    share2 = Kit.headPoss h2 $ Kit.possInner posh12
    --side2  = Kit.headPoss h2 $ Kit.possOuter posh12

    m2 = Kit.gatherToMap $ map pair args2
    pair arg2 = (Kit.possPick share2 arg2, True)

    h3 = Kit.mappend h2 h1
    b3 = filter step args1
    step arg1 = case Kit.lookupMap (Kit.possPick share1 arg1) m2 of
                  Just _  -> which
                  Nothing -> not which

