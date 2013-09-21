{-# OPTIONS_GHC -Wall #-}

-- | Restrict by relmap

module Koshucode.Baala.Minimal.Restrict
( -- * some
  ropConsSome, relmapSome, relfySome,
  -- * none
  ropConsNone, relmapNone, relfyNone
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
relmapSome use m = C.relmapConfl use "some" fy [m] where
    fy [r2] = relfySome r2
    fy _    = B.bug

relfySome
    :: (Ord c)
    => C.Relfy c          -- ^ Generator of subrelmap
    -> B.Relhead           -- ^ Heading of input relation
    -> B.Ab (C.Relfy c)   -- ^ Generator for output relation
relfySome = relfySemi False



-- ----------------------  none

ropConsNone :: (Ord c) => C.RopCons c
ropConsNone use =
    do m <- getRelmap use
       Right $ relmapNone use m

relmapNone :: (Ord c) => C.RopUse c -> B.Map (C.Relmap c)
relmapNone use m = C.relmapConfl use "none" fy [m] where
    fy [r2] = relfyNone r2
    fy _    = B.bug

relfyNone :: (Ord c) => C.Relfy c -> B.Relhead -> B.Ab (C.Relfy c)
relfyNone = relfySemi True

relfySemi
    :: (Ord c)
    => Bool
    -> C.Relfy c          -- ^ Generator of subrelmap
    -> B.Relhead          -- ^ Heading of input relation
    -> B.Ab (C.Relfy c)   -- ^ Generator for output relation
relfySemi b (C.Relfy _ f2) h1 =
    Right $ C.Relfy h1 (C.RelfyAbPred p)
    where p cs = do b2 <- C.relfy f2 [cs]
                    Right $ null b2 == b

