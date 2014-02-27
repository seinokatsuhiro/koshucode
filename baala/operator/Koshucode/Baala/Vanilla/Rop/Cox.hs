{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-| Relmap operators using term-content expressions. -}

module Koshucode.Baala.Vanilla.Rop.Cox
(
  -- * add
  consAdd, relmapAdd, relkitAdd,
  -- * hold
  consHold, relmapHold, relkitHold,
) where

import qualified Koshucode.Baala.Base    as B
import qualified Koshucode.Baala.Core    as C
import qualified Koshucode.Baala.Builtin as Rop


-- ----------------------  add

consAdd :: (C.CContent c) => C.RopCons c
consAdd use =
    do treesLet <- Rop.getOption [] Rop.getWordTrees use "-let"
       treesIn  <- Rop.getTermTrees use "-in"
       coxLet   <- namedAlpha use treesLet
       coxIn    <- namedAlpha use treesIn
       let base  = C.globalFunction $ C.ropGlobal use
       Right $ relmapAdd use (base, coxLet, coxIn)

relmapAdd :: (C.CContent c, B.Pretty c)
  => C.RopUse c -> ([C.Cop c], [C.NamedCox c], [C.NamedCox c]) -> C.Relmap c
relmapAdd use = C.relmapCalc use . relkitAdd

-- todo: shared term
relkitAdd :: forall c. (C.CContent c, B.Pretty c)
  => ([C.Cop c], [C.NamedCox c], [C.NamedCox c]) -> C.RelkitCalc c
relkitAdd (base, deriv, bodies) h1 =
    Right $ C.relkit h2 (C.RelkitOneToAbOne False f)
        where ns = map fst bodies   -- term names
              es = map snd bodies   -- term expression
              h2 = B.headAppend ns h1
              f cs1 = do es2 <- mapM (C.coxConsBeta base deriv h1) es
                         cs2 <- C.coxRun cs1 `mapM` es2
                         Right $ cs2 ++ cs1


-- ----------------------  hold

consHold :: (C.CContent c) => Bool -> C.RopCons c
consHold b use =
    do treesLet <- Rop.getOption [] Rop.getWordTrees use "-let"
       treesIn  <- Rop.getTrees use "-in"
       coxLet   <- namedAlpha use treesLet
       coxIn    <- alpha use $ B.treeWrap treesIn
       let base  = C.globalFunction $ C.ropGlobal use
       Right $ relmapHold use (b, base, coxLet, coxIn)

relmapHold :: (C.CContent c) => C.RopUse c -> (Bool, [C.Cop c], [C.NamedCox c], C.Cox c) -> C.Relmap c
relmapHold use = C.relmapCalc use . relkitHold

relkitHold :: (C.CContent c) => (Bool, [C.Cop c], [C.NamedCox c], C.Cox c) -> C.RelkitCalc c
relkitHold (b, base, deriv, body) h1 = Right $ C.relkit h1 (C.RelkitAbPred p) where
    p cs = do e <- C.coxConsBeta base deriv h1 body
              c <- C.coxRun cs e
              case c of
                x | C.isBool x -> Right $ b == C.gBool x
                _ -> Left $ B.AbortAnalysis [] $ B.AAReqBoolean ""


-- ----------------------  alpha

alpha :: (C.CContent c) => C.RopUse c -> B.TokenTree -> B.Ab (C.Cox c)
alpha = C.coxConsAlpha . C.globalSyntax . C.ropGlobal

namedAlpha :: (C.CContent c) => C.RopUse c -> [B.Named B.TokenTree] -> B.Ab [C.NamedCox c]
namedAlpha use = mapM (B.namedMapM $ alpha use)

