{-# OPTIONS_GHC -Wall #-}

{-| Restrict by relmap -}

module Koshucode.Baala.Minimal.Restrict
( -- * some
  consSome, relmapSome, relkitSome,
  -- * none
  consNone, relmapNone, relkitNone,
  -- * sub
  consSub, relmapSub, relkitSub,
  -- * equal
  consEqual, relmapEqual, relkitEqual,
) where

import qualified Koshucode.Baala.Base    as B
import qualified Koshucode.Baala.Core    as C
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

relkitSemi :: (Ord c) => Bool -> C.RelkitBinary c
relkitSemi isNull (C.Relkit _ f2) h1 =
    Right $ C.relkit h1 (C.RelkitAbSemi p f2)
    where p b2 = Right $ null b2 == isNull



-- ----------------------  none

consNone :: (Ord c) => C.RopCons c
consNone use =
    do m <- Rop.getRelmap use
       Right $ relmapNone use m

relmapNone :: (Ord c) => C.RopUse c -> B.Map (C.Relmap c)
relmapNone use = C.relmapBinary use relkitNone

relkitNone :: (Ord c) => C.RelkitBinary c
relkitNone = relkitSemi True



-- ----------------------  sub

consSub :: (Ord c) => C.RopCons c
consSub use =
    do m <- Rop.getRelmap use
       Right $ relmapSub use m

relmapSub :: (Ord c) => C.RopUse c -> B.Map (C.Relmap c)
relmapSub use = C.relmapBinary use relkitSub

relkitSub :: (Ord c) => C.RelkitBinary c
relkitSub r2@(C.Relkit (Just h2) _) (Just h1)
    | B.isSuperhead h1 h2 = sub
    | otherwise = Right $ C.relkitJust h1 (C.RelkitConst [])
    where
      sub = do r3 <- Rop.relkitMeet r2 (Just h1)
               f3 <- relkitSome r3 (Just h1)
               Right f3
relkitSub _ _ = Right C.relkitNothing



-- ----------------------  equal

consEqual :: (Ord c) => C.RopCons c
consEqual use =
    do m <- Rop.getRelmap use
       Right $ relmapEqual use m

relmapEqual :: (Ord c) => C.RopUse c -> B.Map (C.Relmap c)
relmapEqual use = C.relmapBinary use relkitEqual

relkitEqual :: (Ord c) => C.RelkitBinary c
relkitEqual (C.Relkit (Just h2) f2) (Just h1) =
    Right $ C.relkitJust h2 $ C.RelkitAbFull False equal [f2]
    where equal sub b1 =
              do let [g2] = sub
                 b2 <- g2 b1
                 Right $ if B.Rel h1 b1 == B.Rel h2 b2
                         then [[]] else []
relkitEqual _ _ = Right C.relkitNothing

