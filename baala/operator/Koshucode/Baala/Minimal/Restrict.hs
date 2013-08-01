{-# OPTIONS_GHC -Wall #-}

-- | Restrict by relation

module Koshucode.Baala.Minimal.Restrict
( -- * some
  ropConsSome, relmapSome, relSome,
  -- * minus
  ropConsMinus, relmapMinus, relMinus
) where

import Koshucode.Baala.Base
import Koshucode.Baala.Core
import Koshucode.Baala.Builtin



-- ----------------------  some

ropConsSome :: (Ord c) => RopCons c
ropConsSome use = 
    do m <- getRelmap use
       Right $ relmapSome use m

relmapSome :: (Ord c) => OpUse c -> Map (Relmap c)
relmapSome use m = relmapConfl use "minus" sub [m] where
    sub [r2] = relSome r2
    sub _    = bug

relSome :: (Ord c)
    => Rel c          -- ^ Matching relation
    -> AbMap (Rel c)  -- ^ Relation to relation
relSome r = relSemi True r



-- ----------------------  minus

ropConsMinus :: (Ord c) => RopCons c
ropConsMinus use =
    do m <- getRelmap use
       Right $ relmapMinus use m

relmapMinus :: (Ord c) => OpUse c -> Map (Relmap c)
relmapMinus use m = relmapConfl use "minus" sub [m] where
    sub [r2] = relMinus r2
    sub _    = bug

relMinus :: (Ord c)
    => Rel c          -- ^ Unmatching relation
    -> AbMap (Rel c)  -- ^ Relation to relation
relMinus = relSemi False



-- ----------------------  Semi

relSemi :: (Ord c) => Bool -> Rel c -> AbMap (Rel c)
relSemi which r2 r1 = Right $ Rel h3 b3 where
    Rel h1 args1 = r1
    Rel h2 args2 = r2

    posh12 = h1 `posFrom` h2
    share1 = posOf h1 $ termsInner posh12
    share2 = posOf h2 $ termsInner posh12
    --side2  = posOf h2 $ termsOuter posh12

    m2 = gatherToMap $ map pair args2
    pair arg2 = (csPick share2 arg2, True)

    h3 = mappend h2 h1
    b3 = filter step args1
    step arg1 = case lookupMap (csPick share1 arg1) m2 of
                  Just _  -> which
                  Nothing -> not which

