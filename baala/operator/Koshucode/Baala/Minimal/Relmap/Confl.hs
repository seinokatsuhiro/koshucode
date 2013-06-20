{-# OPTIONS_GHC -Wall #-}

-- | Confluent relational mapping

module Koshucode.Baala.Minimal.Relmap.Confl
( relmapSome
, relmapMinus
, relSome
, relMinus
) where

import Koshucode.Baala.Minimal.OpKit as Kit

relmapSome :: (Ord v) => Kit.OpUse v -> Kit.Relmap v -> Kit.Relmap v
relmapSome use m = Kit.relmapConfl use "minus" sub [m] where
    sub [r2] r1 = relSome r1 r2
    sub _ _     = undefined

relmapMinus :: (Ord v) => Kit.OpUse v -> Kit.Relmap v -> Kit.Relmap v
relmapMinus use m = Kit.relmapConfl use "minus" sub [m] where
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

    m2 = gatherToMap $ map pair args2
    pair arg2 = (Kit.possPick share2 arg2, True)

    h3 = Kit.mappend h2 h1
    b3 = filter step args1
    step arg1 = case lookupMap (Kit.possPick share1 arg1) m2 of
                  Just _  -> which
                  Nothing -> not which

