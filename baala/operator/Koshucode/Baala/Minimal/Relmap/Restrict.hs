{-# OPTIONS_GHC -Wall #-}

-- | Restrict by relation

module Koshucode.Baala.Minimal.Relmap.Restrict
( -- * some
  relopSome, relmapSome, relSome,
  -- * minus
  relopMinus, relmapMinus, relMinus
) where

import Koshucode.Baala.Minimal.OpKit as Kit
import Koshucode.Baala.Minimal.Relmap.Get



-- ----------------------  Some

relopSome :: (Ord v) => Relop v
relopSome use = do
  m <- getRelmap use
  Right $ relmapSome use m

relmapSome :: (Ord v) => OpUse v -> Map (Relmap v)
relmapSome use m = Kit.relmapConfl use "minus" sub [m] where
    sub [r2] = relSome r2
    sub _    = bug

relSome
    :: (Ord v)
    => Rel v        -- ^ Matching relation
    -> Map (Rel v)  -- ^ Relation to relation
relSome  = relSemi True



-- ----------------------  Minus

relopMinus :: (Ord v) => Relop v
relopMinus use = do
  m <- getRelmap use
  Right $ relmapMinus use m

relmapMinus :: (Ord v) => OpUse v -> Map (Relmap v)
relmapMinus use m = Kit.relmapConfl use "minus" sub [m] where
    sub [r2] = relMinus r2
    sub _    = bug

relMinus
    :: (Ord v)
    => Rel v        -- ^ Unmatching relation
    -> Map (Rel v)  -- ^ Relation to relation
relMinus = relSemi False



-- ----------------------  Semi

relSemi :: (Ord v) => Bool -> Rel v -> Map (Rel v)
relSemi which r2 r1 = Rel h3 b3 where
    Rel h1 args1 = r1
    Rel h2 args2 = r2

    posh12 = Kit.headPosh h1 h2
    share1 = Kit.headPoss h1 $ Kit.possInner posh12
    share2 = Kit.headPoss h2 $ Kit.possInner posh12
    --side2  = Kit.headPoss h2 $ Kit.possOuter posh12

    m2 = gatherToMap $ map pair args2
    pair arg2 = (Kit.possPick share2 arg2, True)

    h3 = Kit.mappend h2 h1
    b3 = filter step args1
    step arg1 = case lookupMap (Kit.possPick share1 arg1) m2 of
                  Just _  -> which
                  Nothing -> not which

