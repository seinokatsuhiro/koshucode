{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Relmap operators using term-content expressions.

module Koshucode.Baala.Op.Vanilla.Cox
(
  -- * add
  consAdd, relmapAdd,
  -- * subst
  consSubst, relmapSubst,
  -- * filter
  consFilter, relmapFilter, relkitFilter,
) where

import qualified Koshucode.Baala.Base       as B
import qualified Koshucode.Baala.Core       as C
import qualified Koshucode.Baala.Op.Builtin as Op


-- ----------------------  add

consAdd :: (C.CContent c) => C.RopCons c
consAdd use =
    do treesLet <- Op.getOption [] Op.getWordTrees use "-let"
       treesIn  <- Op.getTermTrees use "-in"
       coxLet   <- alphas use treesLet
       coxIn    <- alphas use treesIn
       let base = C.globalFunction $ C.ropGlobal use
       Right $ relmapAdd use (base, coxLet, coxIn)

relmapAdd :: (C.CList c, C.CRel c, B.Pretty c)
  => C.RopUse c -> ([C.Cop c], [C.NamedCox c], [C.NamedCox c]) -> C.Relmap c
relmapAdd use = C.relmapFlow use . relkitAdd B.headAppend

-- todo: shared term
relkitAdd :: (C.CList c, C.CRel c, B.Pretty c)
  => ([B.Termname] -> B.Map B.Relhead)
  -> ([C.Cop c], [C.NamedCox c], [C.NamedCox c])
  -> C.RelkitCalc c
relkitAdd _ _ Nothing = Right C.relkitNothing
relkitAdd headTrans (base, deriv, bodies) (Just he1) = Right kit2 where
    ns   = map fst bodies   -- term names
    es   = map snd bodies   -- term expression
    he2  = ns `headTrans` he1
    kit2 = C.relkitJust he2 $ C.RelkitOneToAbOne False kitf2 []
    kitf2 _ cs1 = do es2 <- C.coxBeta base deriv he1 `mapM` es
                     cs2 <- C.coxRun cs1 `mapM` es2
                     Right $ cs2 ++ cs1


-- ----------------------  subst

consSubst :: (C.CContent c) => C.RopCons c
consSubst use =
    do treesLet <- Op.getOption [] Op.getWordTrees use "-let"
       treesIn  <- Op.getTermTrees use "-in"
       coxLet   <- alphas use treesLet
       coxIn    <- alphas use treesIn
       let base = C.globalFunction $ C.ropGlobal use
       Right $ relmapSubst use (base, coxLet, coxIn)

relmapSubst :: (C.CList c, C.CRel c, B.Pretty c)
  => C.RopUse c -> ([C.Cop c], [C.NamedCox c], [C.NamedCox c]) -> C.Relmap c
relmapSubst use = C.relmapFlow use . relkitAdd B.headSubst



-- ----------------------  filter

consFilter :: (C.CContent c) => Bool -> C.RopCons c
consFilter b use =
    do treesLet <- Op.getOption [] Op.getWordTrees use "-let"
       treesIn  <- Op.getTrees use "-in"
       coxLet   <- alphas use treesLet
       coxIn    <- alpha  use $ B.treeWrap treesIn
       let base = C.globalFunction $ C.ropGlobal use
       Right $ relmapFilter use (b, base, coxLet, coxIn)

relmapFilter :: (C.CList c, C.CRel c, C.CBool c, B.Pretty c)
  => C.RopUse c -> (Bool, [C.Cop c], [C.NamedCox c], C.Cox c) -> C.Relmap c
relmapFilter use = C.relmapFlow use . relkitFilter

relkitFilter :: (C.CList c, C.CRel c, C.CBool c, B.Pretty c)
  => (Bool, [C.Cop c], [C.NamedCox c], C.Cox c) -> C.RelkitCalc c
relkitFilter _ Nothing = Right C.relkitNothing
relkitFilter (which, base, deriv, body) (Just he1) = Right kit2 where
    kit2 = C.relkitJust he1 $ C.RelkitAbPred p
    p cs1 = do e <- C.coxBeta base deriv he1 body
               c <- C.coxRun cs1 e
               case C.isBool c of
                 True  -> Right $ C.gBool c == which
                 False -> Left $ B.AbortAnalysis [] $ B.AAReqBoolean ""


-- ----------------------  alpha

alpha :: (C.CContent c) => C.RopUse c -> B.TokenTree -> B.Ab (C.Cox c)
alpha = C.coxAlpha . C.globalSyntax . C.ropGlobal

alphas :: (C.CContent c) => C.RopUse c -> [B.Named B.TokenTree] -> B.Ab [C.NamedCox c]
alphas use = mapM (B.namedMapM $ alpha use)

