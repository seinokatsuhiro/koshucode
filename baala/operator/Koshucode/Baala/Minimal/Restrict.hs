{-# OPTIONS_GHC -Wall #-}

-- | Restrict by relation

module Koshucode.Baala.Minimal.Restrict
( -- * some
  ropConsSome, relmapSome, relSome,
  -- * none
  ropConsNone, relmapNone, relNone
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
relmapSome use m = C.relmapConfl use "some" sub gen [m] where
    sub [r2] = relSome r2
    sub _    = B.bug
    gen [r2] = relgenSome r2
    gen _    = B.bug

relgenSome :: (Ord c) => C.Relgen c -> B.Relhead -> B.Ab (C.Relgen c)
relgenSome (C.Relgen _ g2) h1 = Right $ C.Relgen h1 (C.RelgenAbPred p) where
    p a1 = do b2 <- C.runRelgen g2 [a1]
              Right $ not $ null b2

relSome :: (Ord c)
    => B.Rel c            -- ^ Matching relation
    -> B.AbMap (B.Rel c)  -- ^ Relation to relation
relSome r = relSemi True r



-- ----------------------  none

ropConsNone :: (Ord c) => C.RopCons c
ropConsNone use =
    do m <- getRelmap use
       Right $ relmapNone use m

relmapNone :: (Ord c) => C.RopUse c -> B.Map (C.Relmap c)
relmapNone use m = C.relmapConfl use "none" sub gen [m] where
    sub [r2] = relNone r2
    sub _    = B.bug
    gen [r2] = relgenNone r2
    gen _    = B.bug

relgenNone :: (Ord c) => C.Relgen c -> B.Relhead -> B.Ab (C.Relgen c)
relgenNone (C.Relgen _ g2) h1 = Right $ C.Relgen h1 (C.RelgenAbPred p) where
    p a1 = do b2 <- C.runRelgen g2 [a1]
              Right $ null b2

relNone :: (Ord c)
    => B.Rel c            -- ^ Unmatching relation
    -> B.AbMap (B.Rel c)  -- ^ Relation to relation
relNone = relSemi False



-- ----------------------  Semi

relSemi :: (Ord c) => Bool -> B.Rel c -> B.AbMap (B.Rel c)
relSemi which r2 r1 = Right $ B.Rel h3 b3 where
    B.Rel h1 args1 = r1
    B.Rel h2 args2 = r2

    posh12 = h1 `B.posFrom` h2
    share1 = h1 `B.posNest` B.posInner posh12
    share2 = h2 `B.posNest` B.posInner posh12
    --side2  = posNest h2 $ posOuter posh12

    m2 = B.gatherToMap $ map pair args2
    pair arg2 = (B.posPick share2 arg2, True)

    h3 = mappend h2 h1
    b3 = filter step args1
    step arg1 = case B.lookupMap (B.posPick share1 arg1) m2 of
                  Just _  -> which
                  Nothing -> not which

