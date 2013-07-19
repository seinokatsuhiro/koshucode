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

import Koshucode.Baala.Base.Abort
import Koshucode.Baala.Minimal.OpKit as Kit
import Koshucode.Baala.Vanilla.Value.Relval
import qualified Koshucode.Baala.Minimal as Mini


-- ----------------------  maybe

relopMaybe :: Kit.Relop Val
relopMaybe use = Right $ relmapMaybe use

relmapMaybe :: (Ord v, Nil v) => Kit.OpUse v -> Kit.Relmap v
relmapMaybe use = Kit.relmapConfl use "maybe" sub ms where
    ms = Kit.opSubmap use
    sub [r2] r1 = relMaybe r1 r2
    sub _ _     = undefined

-- | like SQL's left join
relMaybe :: (Ord v, Nil v) => Rel v -> AbMap (Rel v)
relMaybe r1 r2 = Right $ Rel h3 b3 where
    Rel h1 args1 = r1
    Rel h2 args2 = r2

    posh12 = Kit.headPosh h1 h2
    share1 = Kit.headPoss h1 $ Kit.possInner posh12
    share2 = Kit.headPoss h2 $ Kit.possInner posh12
    side2  = Kit.headPoss h2 $ Kit.possOuter posh12

    m2 = Kit.gatherToMap $ map pair args2
    pair arg2 = (Kit.possPick share2 arg2,
                 Kit.possPick side2  arg2)

    h3 = Kit.mappend h2 h1
    b3 = concatMap step args1
    nils = replicate (headDegree h3 - headDegree h1) nil
    step arg1 = case Kit.lookupMap (Kit.possPick share1 arg1) m2 of
                  Just side -> map (++ arg1) side
                  Nothing   -> [nils ++ arg1]



-- ----------------------  maybe-both

relopMaybeBoth :: Kit.Relop Val
relopMaybeBoth use = Right $ relmapMaybeBoth use

-- | like SQL's full join
relmapMaybeBoth :: (Ord v, Nil v) => Kit.OpUse v -> Kit.Relmap v
relmapMaybeBoth use = Kit.relmapConfl use "mmaybe" sub ms where
    ms = Kit.opSubmap use
    sub [r2] r1 = do r12 <- relMaybe r1 r2
                     r21 <- relMaybe r2 r1
                     Mini.relJoin r12 r21
    sub _ _     = undefined



-- ----------------------  hang

relopHang :: Kit.Relop Val
relopHang use = do
  n <- Mini.getTerm use "-term"
  Right $ relmapHang use n

relmapHang :: (Ord v, RelValue v) => OpUse v -> String -> Relmap v
relmapHang use n = Kit.relmapConfl use "hang" sub ms where
    ms = Kit.opSubmap use
    sub [r2] r1 = relHang n r2 r1
    sub _ _     = undefined

-- | Hanging relation, like grouping.
relHang :: (Ord v, RelValue v) => String -> Rel v -> AbMap (Rel v)
relHang n r2 r1 = Right $ Rel h3 b3 where
    Rel h1 args1 = r1
    Rel h2 args2 = r2

    posh12 = Kit.headPosh h1 h2
    share1 = Kit.headPoss h1 $ Kit.possInner posh12
    share2 = Kit.headPoss h2 $ Kit.possInner posh12
    --side2  = Kit.headPoss h2 $ Kit.possOuter posh12

    m2 = Kit.gatherToMap $ map pair args2
    pair arg2 = (Kit.possPick share2 arg2, arg2)

    h3 = Relhead $ Nest n (headTerms h2) : (headTerms h1)
    b3 = map step args1
    step arg1 = case Kit.lookupMap (Kit.possPick share1 arg1) m2 of
                  Just args2' -> (relValue $ Rel h2 args2') : arg1
                  Nothing     -> []

