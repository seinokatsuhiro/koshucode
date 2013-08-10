{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Vanilla.Relmap.Binary
( 
  -- * maybe
  relopMaybe, relmapMaybe, relMaybe,
  -- * maybe-both
  relopMaybeBoth, relmapMaybeBoth,
  -- * hang
  relopHang, relmapHang, relHang,
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C
import Koshucode.Baala.Builtin
import Koshucode.Baala.Vanilla.Type
import qualified Koshucode.Baala.Minimal as Mini



-- ----------------------  maybe

relopMaybe :: C.RopCons VContent
relopMaybe use = Right $ relmapMaybe use

relmapMaybe :: (Ord c, C.CNil c) => C.RopUse c -> C.Relmap c
relmapMaybe use = C.relmapConfl use "maybe" sub ms where
    ms = C.ropSubmap use
    sub [r2] r1 = relMaybe r1 r2
    sub _ _     = undefined

-- | like SQL's left join
relMaybe :: (Ord c, C.CNil c) => B.Rel c -> B.AbMap (B.Rel c)
relMaybe r1 r2 = Right $ B.Rel h3 b3 where
    B.Rel h1 args1 = r1
    B.Rel h2 args2 = r2

    posh12 = h1 `B.posFrom` h2
    share1 = B.posOf h1 $ B.termsInner posh12
    share2 = B.posOf h2 $ B.termsInner posh12
    side2  = B.posOf h2 $ B.termsOuter posh12

    m2 = B.gatherToMap $ map pair args2
    pair arg2 = (B.csPick share2 arg2,
                 B.csPick side2  arg2)

    h3 = mappend h2 h1
    b3 = concatMap step args1
    nils = replicate (B.headDegree h3 - B.headDegree h1) C.nil
    step arg1 = case B.lookupMap (B.csPick share1 arg1) m2 of
                  Just side -> map (++ arg1) side
                  Nothing   -> [nils ++ arg1]



-- ----------------------  maybe-both

relopMaybeBoth :: C.RopCons VContent
relopMaybeBoth use = Right $ relmapMaybeBoth use

-- | like SQL's full join
relmapMaybeBoth :: (Ord c, C.CNil c) => C.RopUse c -> C.Relmap c
relmapMaybeBoth use = C.relmapConfl use "mmaybe" sub ms where
    ms = C.ropSubmap use
    sub [r2] r1 = do r12 <- relMaybe r1 r2
                     r21 <- relMaybe r2 r1
                     Mini.relJoin r12 r21
    sub _ _     = undefined



-- ----------------------  hang

relopHang :: C.RopCons VContent
relopHang use =
  do n <- getTerm use "-term"
     Right $ relmapHang use n

relmapHang :: (Ord c, C.CRel c) => C.RopUse c -> String -> C.Relmap c
relmapHang use n = C.relmapConfl use "hang" sub ms where
    ms = C.ropSubmap use
    sub [r2] r1 = relHang n r2 r1
    sub _ _     = undefined

-- | Hanging relation, like grouping.
relHang :: (Ord c, C.CRel c) => String -> B.Rel c -> B.AbMap (B.Rel c)
relHang n r2 r1 = Right $ B.Rel h3 b3 where
    B.Rel h1 args1 = r1
    B.Rel h2 args2 = r2

    posh12 = h1 `B.posFrom` h2
    share1 = B.posOf h1 $ B.termsInner posh12
    share2 = B.posOf h2 $ B.termsInner posh12
    --side2  = posOf h2 $ termsOuter posh12

    m2 = B.gatherToMap $ map pair args2
    pair arg2 = (B.csPick share2 arg2, arg2)

    h3 = B.Relhead $ B.Nest n (B.headTerms h2) : (B.headTerms h1)
    b3 = map step args1
    step arg1 = case B.lookupMap (B.csPick share1 arg1) m2 of
                  Just args2' -> (C.putRel $ B.Rel h2 args2') : arg1
                  Nothing     -> []

