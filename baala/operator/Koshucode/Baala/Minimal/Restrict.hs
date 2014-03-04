{-# OPTIONS_GHC -Wall #-}

{-| Restrict by relmap -}

module Koshucode.Baala.Minimal.Restrict
( -- * some
  consSome, relmapSome, relkitSome,
  -- * none
  consNone, relmapNone, relkitNone,
  -- * sub
  consSub, relmapSub, relkitSub,
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C
import qualified Koshucode.Baala.Builtin as Rop
import qualified Koshucode.Baala.Minimal.Tropashko as Rop



-- ----------------------  some

consSome :: (Ord c) => C.RopCons c
consSome use = 
    do m <- Rop.getRelmap use
       Right $ relmapSome use m

relmapSome :: (Ord c) => C.RopUse c -> B.Map (C.Relmap c)
relmapSome use = C.relmapBinary use relkitSome

relkitSome :: (Ord c) => C.RelkitBinary c
relkitSome = relkitSemi False


-- ----------------------  none

consNone :: (Ord c) => C.RopCons c
consNone use =
    do m <- Rop.getRelmap use
       Right $ relmapNone use m

relmapNone :: (Ord c) => C.RopUse c -> B.Map (C.Relmap c)
relmapNone use = C.relmapBinary use relkitNone

relkitNone :: (Ord c) => C.RelkitBinary c
relkitNone = relkitSemi True


-- ----------------------  semi

relkitSemi :: (Ord c) => Bool -> C.RelkitBinary c
relkitSemi isNull (C.Relkit _ f2) h1 =
    Right $ C.relkit h1 (C.RelkitAbSemi f2 p)
    where p b2 = Right $ null b2 == isNull

-- relkitSemi :: (Ord c) => Bool -> C.RelkitBinary c
-- relkitSemi isNull (C.Relkit _ f2) h1 =
--     Right $ C.relkit h1 (C.RelkitAbPred p)
--     where p cs = do b2 <- C.relkitRun f2 [cs]
--                     Right $ null b2 == isNull


-- ----------------------  sub

consSub :: (Ord c) => C.RopCons c
consSub use =
    do m <- Rop.getRelmap use
       Right $ relmapSub use m

relmapSub :: (Ord c) => C.RopUse c -> B.Map (C.Relmap c)
relmapSub use = C.relmapBinary use relkitSub

relkitSub :: (Ord c) => C.RelkitBinary c
relkitSub r2@(C.Relkit h2 _) h1
    | B.isSuperhead h1 h2 = sub
    | otherwise = Right $ C.relkit h1 (C.RelkitConst [])
    where
      sub = do r3 <- Rop.relkitMeet r2 h1
               f3 <- relkitSome r3 h1
               Right f3

