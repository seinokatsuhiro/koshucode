{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Vanilla.Relmap.Binary
( 
  -- * maybe
  ropConsMaybe, relmapMaybe, relgenMaybe,
  -- * maybe-both
  ropConsMaybeBoth, relmapMaybeBoth,
  -- * group
  ropConsGroup, relmapGroup, relgenGroup,
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
relmapMaybe use m = C.relmapConfl use "maybe" gen [m] where
    gen [r2] = relgenMaybe r2
    gen _    = B.bug

relgenMaybe :: (Ord c, C.CNil c) => C.Relgen c -> B.Relhead -> B.Ab (C.Relgen c)
relgenMaybe (C.Relgen h2 g2) h1 = Right $ C.Relgen h3 (C.RelgenAbBody g3) where
    posh12  =  h1 `B.posFrom` h2
    share1  =  h1 `B.posNest` B.posInner posh12
    share2  =  h2 `B.posNest` B.posInner posh12
    side2   =  h2 `B.posNest` B.posOuter posh12

    m2 b1 = do b2 <- C.runRelgenBody g2 b1
               Right $ B.gatherToMap $ map pair b2
    pair arg2 = ( B.posPick share2 arg2,
                  B.posPick side2  arg2 )

    h3 = Builtin.mappend h2 h1
    g3 b1 = do m <- m2 b1
               Right $ concatMap (step m) b1
    nils = replicate (B.headDegree h3 - B.headDegree h1) C.nil
    step m arg1 = case B.lookupMap (B.posPick share1 arg1) m of
                    Just side -> map (++ arg1) side
                    Nothing   -> [nils ++ arg1]



-- ----------------------  maybe-both

ropConsMaybeBoth :: C.RopCons VContent
ropConsMaybeBoth use =
    do m <- Builtin.getRelmap use
       Right $ relmapMaybeBoth use m

{-| like SQL's full join -}
relmapMaybeBoth :: (Ord c, C.CNil c) => C.RopUse c -> B.Map (C.Relmap c)
relmapMaybeBoth use m = C.relmapConfl use "maybe-both" gen [m] where
--     sub [r2] r1 = do r12 <- relMaybe r1 r2
--                      r21 <- relMaybe r2 r1
--                      Mini.relJoin r12 r21
--    sub _ _ = B.bug
    gen _ _ = B.bug



-- ----------------------  group

ropConsGroup :: C.RopCons VContent
ropConsGroup use =
  do n <- Builtin.getTerm   use "-term"
     m <- Builtin.getRelmap use
     Right $ relmapGroup use n m

relmapGroup :: (Ord c, C.CRel c) => C.RopUse c -> String -> B.Map (C.Relmap c)
relmapGroup use n m = C.relmapConfl use "group" gen [m] where
    gen [r2] = relgenGroup n r2
    gen _    = B.bug

{-| Grouping relation. -}
relgenGroup :: (Ord c, C.CRel c) => String -> C.Relgen c -> B.Relhead -> B.Ab (C.Relgen c)
relgenGroup n (C.Relgen h2 g2) h1 = Right $ C.Relgen h3 (C.RelgenAbBody g3) where
    posh12    =  h1 `B.posFrom` h2
    share1    =  h1 `B.posNest` B.posInner posh12
    share2    =  h2 `B.posNest` B.posInner posh12
    --side2  = posNest h2 $ posOuter posh12

    m2 b1     = do b2 <- C.runRelgenBody g2 b1
                   Right $ B.gatherToMap $ map pair b2
    pair arg2 = (B.posPick share2 arg2, arg2)

    h3        = B.Relhead $ B.Nest n (B.headTerms h2) : (B.headTerms h1)
    g3 b1     = do m <- m2 b1
                   Right $ map (step m) b1
    step m arg1 = case B.lookupMap (B.posPick share1 arg1) m of
                    Just args2' -> (C.putRel $ B.Rel h2 args2') : arg1
                    Nothing     -> []

