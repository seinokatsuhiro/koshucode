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

import Koshucode.Baala.Base
import Koshucode.Baala.Core
import Koshucode.Baala.Builtin
import Koshucode.Baala.Vanilla.Type
import qualified Koshucode.Baala.Minimal as Mini



-- ----------------------  maybe

relopMaybe :: RopCons VContent
relopMaybe use = Right $ relmapMaybe use

relmapMaybe :: (Ord c, CNil c) => RopUse c -> Relmap c
relmapMaybe use = relmapConfl use "maybe" sub ms where
    ms = ropSubmap use
    sub [r2] r1 = relMaybe r1 r2
    sub _ _     = undefined

-- | like SQL's left join
relMaybe :: (Ord c, CNil c) => Rel c -> AbMap (Rel c)
relMaybe r1 r2 = Right $ Rel h3 b3 where
    Rel h1 args1 = r1
    Rel h2 args2 = r2

    posh12 = h1 `posFrom` h2
    share1 = posOf h1 $ termsInner posh12
    share2 = posOf h2 $ termsInner posh12
    side2  = posOf h2 $ termsOuter posh12

    m2 = gatherToMap $ map pair args2
    pair arg2 = (csPick share2 arg2,
                 csPick side2  arg2)

    h3 = mappend h2 h1
    b3 = concatMap step args1
    nils = replicate (headDegree h3 - headDegree h1) nil
    step arg1 = case lookupMap (csPick share1 arg1) m2 of
                  Just side -> map (++ arg1) side
                  Nothing   -> [nils ++ arg1]



-- ----------------------  maybe-both

relopMaybeBoth :: RopCons VContent
relopMaybeBoth use = Right $ relmapMaybeBoth use

-- | like SQL's full join
relmapMaybeBoth :: (Ord c, CNil c) => RopUse c -> Relmap c
relmapMaybeBoth use = relmapConfl use "mmaybe" sub ms where
    ms = ropSubmap use
    sub [r2] r1 = do r12 <- relMaybe r1 r2
                     r21 <- relMaybe r2 r1
                     Mini.relJoin r12 r21
    sub _ _     = undefined



-- ----------------------  hang

relopHang :: RopCons VContent
relopHang use =
  do n <- getTerm use "-term"
     Right $ relmapHang use n

relmapHang :: (Ord c, CRel c) => RopUse c -> String -> Relmap c
relmapHang use n = relmapConfl use "hang" sub ms where
    ms = ropSubmap use
    sub [r2] r1 = relHang n r2 r1
    sub _ _     = undefined

-- | Hanging relation, like grouping.
relHang :: (Ord c, CRel c) => String -> Rel c -> AbMap (Rel c)
relHang n r2 r1 = Right $ Rel h3 b3 where
    Rel h1 args1 = r1
    Rel h2 args2 = r2

    posh12 = h1 `posFrom` h2
    share1 = posOf h1 $ termsInner posh12
    share2 = posOf h2 $ termsInner posh12
    --side2  = posOf h2 $ termsOuter posh12

    m2 = gatherToMap $ map pair args2
    pair arg2 = (csPick share2 arg2, arg2)

    h3 = Relhead $ Nest n (headTerms h2) : (headTerms h1)
    b3 = map step args1
    step arg1 = case lookupMap (csPick share1 arg1) m2 of
                  Just args2' -> (putRel $ Rel h2 args2') : arg1
                  Nothing     -> []

