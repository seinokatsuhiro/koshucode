{-# OPTIONS_GHC -Wall #-}

{-| Restrict by relmap -}

module Koshucode.Baala.Minimal.Restrict
( -- * some
  ropConsSome, relmapSome,
  -- * none
  ropConsNone, relmapNone,
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C
import qualified Koshucode.Baala.Builtin as Rop



-- ----------------------  some

ropConsSome :: (Ord c) => C.RopCons c
ropConsSome use = 
    do m <- Rop.getRelmap use
       Right $ relmapSome use m

relmapSome :: (Ord c) => C.RopUse c -> B.Map (C.Relmap c)
relmapSome use m = C.relmapConfl use "some" fy [m] where
    fy [r2] = relfySemi False r2
    fy _    = B.bug



-- ----------------------  none

ropConsNone :: (Ord c) => C.RopCons c
ropConsNone use =
    do m <- Rop.getRelmap use
       Right $ relmapNone use m

relmapNone :: (Ord c) => C.RopUse c -> B.Map (C.Relmap c)
relmapNone use m = C.relmapConfl use "none" fy [m] where
    fy [r2] = relfySemi True r2
    fy _    = B.bug



-- ----------------------

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

