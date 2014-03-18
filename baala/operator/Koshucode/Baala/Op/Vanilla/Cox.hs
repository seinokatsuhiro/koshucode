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
import qualified Koshucode.Baala.Op.Message as Abort


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
relmapAdd use = C.relmapFlow use . relkitAdd

relkitAdd :: (C.CList c, C.CRel c, B.Pretty c)
  => ([C.Cop c], [C.NamedCox c], [C.NamedCox c]) -> C.RelkitCalc c
relkitAdd _ Nothing = Right C.relkitNothing
relkitAdd (base, deriv, bodies) (Just he1)
    | null ind  = Right kit2
    | otherwise = Abort.unexpTermName
    where
      (ns, xs)    = unzip bodies            -- names and expressions
      ns1         = B.headNames he1         -- term names of input relation
      ind         = ns `B.snipIndex` ns1    -- indicies for ns on input relation
      he2         = ns `B.headAppend` he1
      kit2        = C.relkitJust he2 $ C.RelkitOneToAbOne False kitf2 []
      kitf2 _ cs1 = do xs2 <- C.coxBeta base deriv he1 `mapM` xs
                       cs2 <- C.coxRun cs1 `mapM` xs2
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
relmapSubst use = C.relmapFlow use . relkitSubst

relkitSubst :: (C.CList c, C.CRel c, B.Pretty c)
  => ([C.Cop c], [C.NamedCox c], [C.NamedCox c]) -> C.RelkitCalc c
relkitSubst _ Nothing = Right C.relkitNothing
relkitSubst (base, deriv, bodies) (Just he1)
    | sameLength ns ind = Right kit2
    | otherwise         = Abort.unexpTermName
    where
      (ns, xs)    = unzip bodies                 -- names and expressions
      ns1         = B.headNames he1              -- term names of input relation
      ind         = ns `B.snipIndex` ns1         -- indicies for ns on input relation
      cut         = B.snipOff ind                -- cutting-ns function
      he2         = B.headFrom $ ns ++ cut ns1   -- heading of output relation
      kit2        = C.relkitJust he2 $ C.RelkitOneToAbOne True kitf2 []
      kitf2 _ cs1 = do xs2 <- C.coxBeta base deriv he1 `mapM` xs
                       cs2 <- C.coxRun cs1 `mapM` xs2
                       Right $ cs2 ++ cut cs1

sameLength :: [a] -> [a1] -> Bool
sameLength a b = length a == length b


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
                 False -> Abort.reqBool


-- ----------------------  alpha

alpha :: (C.CContent c) => C.RopUse c -> B.TokenTree -> B.Ab (C.Cox c)
alpha = C.coxAlpha . C.globalSyntax . C.ropGlobal

alphas :: (C.CContent c) => C.RopUse c -> [B.Named B.TokenTree] -> B.Ab [C.NamedCox c]
alphas use = mapM (B.namedMapM $ alpha use)

