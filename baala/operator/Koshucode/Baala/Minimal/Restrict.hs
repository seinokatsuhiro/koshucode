{-# OPTIONS_GHC -Wall #-}

{-| Restrict by relmap -}

module Koshucode.Baala.Minimal.Restrict
( -- * some
  ropConsSome, relmapSome, relfySome,
  -- * none
  ropConsNone, relmapNone, relfyNone,
  -- * sub
  ropConsSub, relmapSub, relfySub,
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C
import qualified Koshucode.Baala.Builtin as Rop
import qualified Koshucode.Baala.Minimal.Tropashko as Rop



-- ----------------------  some

ropConsSome :: (Ord c) => C.RopCons c
ropConsSome use = 
    do m <- Rop.getRelmap use
       Right $ relmapSome use m

relmapSome :: (Ord c) => C.RopUse c -> B.Map (C.Relmap c)
relmapSome use m = C.relmapConfl use fy [m] where
    fy [r2] = relfySome r2
    fy _    = B.bug

relfySome :: (Ord c) => C.Relfy c -> B.Relhead -> B.Ab (C.Relfy c)
relfySome = relfySemi False


-- ----------------------  none

ropConsNone :: (Ord c) => C.RopCons c
ropConsNone use =
    do m <- Rop.getRelmap use
       Right $ relmapNone use m

relmapNone :: (Ord c) => C.RopUse c -> B.Map (C.Relmap c)
relmapNone use m = C.relmapConfl use fy [m] where
    fy [r2] = relfyNone r2
    fy _    = B.bug

relfyNone :: (Ord c) => C.Relfy c -> B.Relhead -> B.Ab (C.Relfy c)
relfyNone = relfySemi True


-- ----------------------  semi

relfySemi
    :: (Ord c)
    => Bool               -- ^ Is null
    -> C.Relfy c          -- ^ Relfier of subrelmap
    -> B.Relhead          -- ^ Heading of input relation
    -> B.Ab (C.Relfy c)   -- ^ Relfier for output relation
relfySemi isNull (C.Relfy _ f2) h1 =
    Right $ C.Relfy h1 (C.RelfyAbPred p)
    where p cs = do b2 <- C.relfy f2 [cs]
                    Right $ null b2 == isNull


-- ----------------------  sub

ropConsSub :: (Ord c) => C.RopCons c
ropConsSub use =
    do m <- Rop.getRelmap use
       Right $ relmapSub use m

relmapSub :: (Ord c) => C.RopUse c -> B.Map (C.Relmap c)
relmapSub use m = C.relmapConfl use fy [m] where
    fy [r2] = relfySub r2
    fy _    = B.bug

relfySub
    :: (Ord c)
    => C.Relfy c
    -> B.Relhead
    -> B.Ab (C.Relfy c)
relfySub r2@(C.Relfy h2 _) h1
    | B.isSuperhead h1 h2 = sub
    | otherwise = Right $ C.Relfy h1 (C.RelfyConst [])
    where
      sub = do r3 <- Rop.relfyMeet r2 h1
               f3 <- relfySome r3 h1
               Right f3

