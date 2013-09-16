{-# OPTIONS_GHC -Wall #-}

-- | Restrict by relation

module Koshucode.Baala.Minimal.Restrict
( -- * some
  ropConsSome, relmapSome, relgenSome,
  -- * none
  ropConsNone, relmapNone, relgenNone
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
relmapSome use m = C.relmapConfl use "some" gen [m] where
    gen [r2] = relgenSome r2
    gen _    = B.bug

relgenSome
    :: (Ord c)
    => C.Relgen c          -- ^ Generator of subrelmap
    -> B.Relhead           -- ^ Heading of input relation
    -> B.Ab (C.Relgen c)   -- ^ Generator for output relation
relgenSome = relgenSemi False



-- ----------------------  none

ropConsNone :: (Ord c) => C.RopCons c
ropConsNone use =
    do m <- getRelmap use
       Right $ relmapNone use m

relmapNone :: (Ord c) => C.RopUse c -> B.Map (C.Relmap c)
relmapNone use m = C.relmapConfl use "none" gen [m] where
    gen [r2] = relgenNone r2
    gen _    = B.bug

relgenNone :: (Ord c) => C.Relgen c -> B.Relhead -> B.Ab (C.Relgen c)
relgenNone = relgenSemi True

relgenSemi
    :: (Ord c)
    => Bool
    -> C.Relgen c          -- ^ Generator of subrelmap
    -> B.Relhead           -- ^ Heading of input relation
    -> B.Ab (C.Relgen c)   -- ^ Generator for output relation
relgenSemi b (C.Relgen _ g2) h1 = Right $ C.Relgen h1 (C.RelgenAbPred p) where
    p a1 = do b2 <- C.runRelgenBody g2 [a1]
              Right $ null b2 == b

