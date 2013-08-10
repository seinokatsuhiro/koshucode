{-# OPTIONS_GHC -Wall #-}

-- | Restrict by relation

module Koshucode.Baala.Minimal.Restrict
( -- * some
  ropConsSome, relmapSome, relSome,
  -- * minus
  ropConsMinus, relmapMinus, relMinus
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C
import Koshucode.Baala.Builtin



-- ----------------------  some

ropConsSome :: (Ord c) => C.RopCons c
ropConsSome use = 
    do m <- getRelmap use
       Right $ relmapSome use m

relmapSome :: (Ord c) => C.RopUse c -> B.Map (C.Relmap c)
relmapSome use m = C.relmapConfl use "minus" sub [m] where
    sub [r2] = relSome r2
    sub _    = B.bug

relSome :: (Ord c)
    => B.Rel c            -- ^ Matching relation
    -> B.AbMap (B.Rel c)  -- ^ Relation to relation
relSome r = relSemi True r



-- ----------------------  minus

ropConsMinus :: (Ord c) => C.RopCons c
ropConsMinus use =
    do m <- getRelmap use
       Right $ relmapMinus use m

relmapMinus :: (Ord c) => C.RopUse c -> B.Map (C.Relmap c)
relmapMinus use m = C.relmapConfl use "minus" sub [m] where
    sub [r2] = relMinus r2
    sub _    = B.bug

relMinus :: (Ord c)
    => B.Rel c            -- ^ Unmatching relation
    -> B.AbMap (B.Rel c)  -- ^ Relation to relation
relMinus = relSemi False



-- ----------------------  Semi

relSemi :: (Ord c) => Bool -> B.Rel c -> B.AbMap (B.Rel c)
relSemi which r2 r1 = Right $ B.Rel h3 b3 where
    B.Rel h1 args1 = r1
    B.Rel h2 args2 = r2

    posh12 = h1 `B.posFrom` h2
    share1 = B.posOf h1 $ B.termsInner posh12
    share2 = B.posOf h2 $ B.termsInner posh12
    --side2  = posOf h2 $ termsOuter posh12

    m2 = B.gatherToMap $ map pair args2
    pair arg2 = (B.csPick share2 arg2, True)

    h3 = mappend h2 h1
    b3 = filter step args1
    step arg1 = case B.lookupMap (B.csPick share1 arg1) m2 of
                  Just _  -> which
                  Nothing -> not which

