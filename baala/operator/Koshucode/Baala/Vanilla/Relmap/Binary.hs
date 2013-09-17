{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Vanilla.Relmap.Binary
( 
  -- * maybe
  ropConsMaybe, relmapMaybe, relfyMaybe,
  -- * maybe-both
  ropConsMaybeBoth, relmapMaybeBoth,
  -- * group
  ropConsGroup, relmapGroup, relfyGroup,
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C
import qualified Koshucode.Baala.Builtin as Builtin
import Koshucode.Baala.Vanilla.Type



-- ----------------------  maybe

ropConsMaybe :: C.RopCons VContent
ropConsMaybe use =
    do m <- Builtin.getRelmap use
       Right $ relmapMaybe use m

relmapMaybe :: (Ord c, C.CNil c) => C.RopUse c -> B.Map (C.Relmap c)
relmapMaybe use m = C.relmapConfl use "maybe" fy [m] where
    fy [r2] = relfyMaybe r2
    fy _    = B.bug

relfyMaybe :: (Ord c, C.CNil c) => C.Relfy c -> B.Relhead -> B.Ab (C.Relfy c)
relfyMaybe (C.Relfy h2 f2) h1 =
    Right $ C.Relfy h3 (C.RelfyAbFull f3)
    where
      posh12  =  h1 `B.posFrom` h2
      share1  =  h1 `B.posNest` B.posInner posh12
      share2  =  h2 `B.posNest` B.posInner posh12
      side2   =  h2 `B.posNest` B.posOuter posh12

      m2 b1   = do b2 <- C.relfy f2 b1
                   Right $ B.gatherToMap $ map kv b2
      kv cs2  = ( B.posPick share2 cs2,
                  B.posPick side2  cs2 )

      h3 = Builtin.mappend h2 h1
      f3 b1 = do m <- m2 b1
                 Right $ concatMap (step m) b1
      nils = replicate (B.headDegree h3 - B.headDegree h1) C.nil
      step m cs1 = case B.lookupMap (B.posPick share1 cs1) m of
                     Just side -> map (++ cs1) side
                     Nothing   -> [nils ++ cs1]



-- ----------------------  maybe-both

ropConsMaybeBoth :: C.RopCons VContent
ropConsMaybeBoth use =
    do m <- Builtin.getRelmap use
       Right $ relmapMaybeBoth use m

{-| like SQL's full join -}
relmapMaybeBoth :: (Ord c, C.CNil c) => C.RopUse c -> B.Map (C.Relmap c)
relmapMaybeBoth use m = C.relmapConfl use "maybe-both" fy [m] where
--     sub [r2] r1 = do r12 <- relMaybe r1 r2
--                      r21 <- relMaybe r2 r1
--                      Mini.relJoin r12 r21
--    sub _ _ = B.bug
    fy _ _ = B.bug



-- ----------------------  group

ropConsGroup :: C.RopCons VContent
ropConsGroup use =
  do n <- Builtin.getTerm   use "-term"
     m <- Builtin.getRelmap use
     Right $ relmapGroup use n m

relmapGroup :: (Ord c, C.CRel c) => C.RopUse c -> String -> B.Map (C.Relmap c)
relmapGroup use n m = C.relmapConfl use "group" fy [m] where
    fy [r2] = relfyGroup n r2
    fy _    = B.bug

{-| Grouping relation. -}
relfyGroup :: (Ord c, C.CRel c) => String -> C.Relfy c -> B.Relhead -> B.Ab (C.Relfy c)
relfyGroup n (C.Relfy h2 f2) h1 =
    Right $ C.Relfy h3 (C.RelfyAbFull f3)
    where
      posh12    =  h1 `B.posFrom` h2
      share1    =  h1 `B.posNest` B.posInner posh12
      share2    =  h2 `B.posNest` B.posInner posh12
      --side2  = posNest h2 $ posOuter posh12

      m2 b1     = do b2 <- C.relfy f2 b1
                     Right $ B.gatherToMap $ map kv b2
      kv cs2    = ( B.posPick share2 cs2, cs2 )

      h3        = B.Relhead $ B.Nest n (B.headTerms h2) : (B.headTerms h1)
      f3 b1     = do m <- m2 b1
                     Right $ map (step m) b1
      step m cs1 = case B.lookupMap (B.posPick share1 cs1) m of
                     Just cs2' -> (C.putRel $ B.Rel h2 cs2') : cs1
                     Nothing     -> []

