{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-| Relmap operators using term-content expressions. -}

module Koshucode.Baala.Vanilla.Rop.Cox
(
  -- * add
  consAdd, relmapAdd, relkitAdd,
  -- * filter
  consFilter, relmapFilter, relkitFilter,
) where

import qualified Koshucode.Baala.Base    as B
import qualified Koshucode.Baala.Core    as C
import qualified Koshucode.Baala.Builtin as Rop


-- ----------------------  add

consAdd :: (C.CContent c) => C.RopCons c
consAdd use =
    do treesLet <- Rop.getOption [] Rop.getWordTrees use "-let"
       treesIn  <- Rop.getTermTrees use "-in"
       coxLet   <- alphas use treesLet
       coxIn    <- alphas use treesIn
       let base = C.globalFunction $ C.ropGlobal use
       Right $ relmapAdd use (base, coxLet, coxIn)

relmapAdd :: (C.CList c, C.CRel c, B.Pretty c)
  => C.RopUse c -> ([C.Cop c], [C.NamedCox c], [C.NamedCox c]) -> C.Relmap c
relmapAdd use = C.relmapCalc use . relkitAdd

-- todo: shared term
relkitAdd :: (C.CList c, C.CRel c, B.Pretty c)
  => ([C.Cop c], [C.NamedCox c], [C.NamedCox c]) -> C.RelkitCalc c
relkitAdd (base, deriv, bodies) h1 =
    Right $ C.relkit h2 (C.RelkitOneToAbOne False f) where
        ns = map fst bodies   -- term names
        es = map snd bodies   -- term expression
        h2 = B.headAppend ns h1
        f cs1 = do es2 <- mapM (C.coxBeta base deriv h1) es
                   cs2 <- C.coxRun cs1 `mapM` es2
                   Right $ cs2 ++ cs1


-- ----------------------  filter

consFilter :: (C.CContent c) => Bool -> C.RopCons c
consFilter b use =
    do treesLet <- Rop.getOption [] Rop.getWordTrees use "-let"
       treesIn  <- Rop.getTrees use "-in"
       coxLet   <- alphas use treesLet
       coxIn    <- alpha  use $ B.treeWrap treesIn
       let base = C.globalFunction $ C.ropGlobal use
       Right $ relmapFilter use (b, base, coxLet, coxIn)

relmapFilter :: (C.CList c, C.CRel c, C.CBool c, B.Pretty c)
  => C.RopUse c -> (Bool, [C.Cop c], [C.NamedCox c], C.Cox c) -> C.Relmap c
relmapFilter use = C.relmapCalc use . relkitFilter

relkitFilter :: (C.CList c, C.CRel c, C.CBool c, B.Pretty c)
  => (Bool, [C.Cop c], [C.NamedCox c], C.Cox c) -> C.RelkitCalc c
relkitFilter (b, base, deriv, body) h1 = Right $ C.relkit h1 (C.RelkitAbPred p) where
    p cs = do e <- C.coxBeta base deriv h1 body
              c <- C.coxRun cs e
              case c of
                x | C.isBool x -> Right $ b == C.gBool x
                _ -> Left $ B.AbortAnalysis [] $ B.AAReqBoolean ""


-- ----------------------  alpha

alpha :: (C.CContent c) => C.RopUse c -> B.TokenTree -> B.Ab (C.Cox c)
alpha = C.coxAlpha . C.globalSyntax . C.ropGlobal

alphas :: (C.CContent c) => C.RopUse c -> [B.Named B.TokenTree] -> B.Ab [C.NamedCox c]
alphas use = mapM (B.namedMapM $ alpha use)

