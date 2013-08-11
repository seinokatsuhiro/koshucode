{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Vanilla.Relmap.Binary
( 
  -- * maybe
  ropConsMaybe, relmapMaybe, relMaybe,
  -- * maybe-both
  ropConsMaybeBoth, relmapMaybeBoth,
  -- * hang
  ropConsHang, relmapHang, relHang,
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C
import qualified Koshucode.Baala.Builtin as Kit
import Koshucode.Baala.Vanilla.Type
import qualified Koshucode.Baala.Minimal as Mini



-- ----------------------  maybe

ropConsMaybe :: C.RopCons VContent
ropConsMaybe use =
    do m <- Kit.getRelmap use
       Right $ relmapMaybe use m

relmapMaybe :: (Ord c, C.CNil c) => C.RopUse c -> B.Map (C.Relmap c)
relmapMaybe use m = C.relmapConfl use "maybe" sub [m] where
    sub [r2] r1 = relMaybe r1 r2
    sub _ _     = B.bug

-- | like SQL's left join
relMaybe :: (Ord c, C.CNil c) => B.Rel c -> B.AbMap (B.Rel c)
relMaybe r1 r2 = Right $ B.Rel h3 b3 where
    B.Rel h1 args1 = r1
    B.Rel h2 args2 = r2

    posh12  =  h1  `B.posFrom`  h2
    share1  =  h1  `B.posOf`    B.termsInner posh12
    share2  =  h2  `B.posOf`    B.termsInner posh12
    side2   =  h2  `B.posOf`    B.termsOuter posh12

    m2 = B.gatherToMap $ map pair args2
    pair arg2 = (B.csPick share2 arg2,
                 B.csPick side2  arg2)

    h3 = Kit.mappend h2 h1
    b3 = concatMap step args1
    nils = replicate (B.headDegree h3 - B.headDegree h1) C.nil
    step arg1 = case B.lookupMap (B.csPick share1 arg1) m2 of
                  Just side -> map (++ arg1) side
                  Nothing   -> [nils ++ arg1]



-- ----------------------  maybe-both

ropConsMaybeBoth :: C.RopCons VContent
ropConsMaybeBoth use =
    do m <- Kit.getRelmap use
       Right $ relmapMaybeBoth use m

-- | like SQL's full join
relmapMaybeBoth :: (Ord c, C.CNil c) => C.RopUse c -> B.Map (C.Relmap c)
relmapMaybeBoth use m = C.relmapConfl use "mmaybe" sub [m] where
    sub [r2] r1 = do r12 <- relMaybe r1 r2
                     r21 <- relMaybe r2 r1
                     Mini.relJoin r12 r21
    sub _ _     = B.bug



-- ----------------------  hang

ropConsHang :: C.RopCons VContent
ropConsHang use =
  do n <- Kit.getTerm   use "-term"
     m <- Kit.getRelmap use
     Right $ relmapHang use n m

relmapHang :: (Ord c, C.CRel c) => C.RopUse c -> String -> B.Map (C.Relmap c)
relmapHang use n m = C.relmapConfl use "hang" sub [m] where
    sub [r2] r1 = relHang n r2 r1
    sub _ _     = B.bug

-- | Hanging relation, like grouping.
relHang :: (Ord c, C.CRel c) => String -> B.Rel c -> B.AbMap (B.Rel c)
relHang n r2 r1 = Right $ B.Rel h3 b3 where
    B.Rel h1 args1 = r1
    B.Rel h2 args2 = r2

    posh12  =  h1  `B.posFrom`  h2
    share1  =  h1  `B.posOf`    B.termsInner posh12
    share2  =  h2  `B.posOf`    B.termsInner posh12
    --side2  = posOf h2 $ termsOuter posh12

    m2 = B.gatherToMap $ map pair args2
    pair arg2 = (B.csPick share2 arg2, arg2)

    h3 = B.Relhead $ B.Nest n (B.headTerms h2) : (B.headTerms h1)
    b3 = map step args1
    step arg1 = case B.lookupMap (B.csPick share1 arg1) m2 of
                  Just args2' -> (C.putRel $ B.Rel h2 args2') : arg1
                  Nothing     -> []

