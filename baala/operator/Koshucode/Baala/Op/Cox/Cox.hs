{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Relmap operators using term-content expressions.

module Koshucode.Baala.Op.Cox.Cox
(
  -- * add
  consAdd, relmapAdd,
  -- * subst
  consSubst, relmapSubst,
  -- * filter
  consFilter, relmapFilter, relkitFilter,
  -- * range
  consRange, relmapRange,
  -- $range
) where

import qualified Koshucode.Baala.Base       as B
import qualified Koshucode.Baala.Core       as C
import qualified Koshucode.Baala.Op.Builtin as Op
import qualified Koshucode.Baala.Op.Message as Message


-- ----------------------  add

consAdd :: (C.CContent c) => C.RopCons c
consAdd use =
    do treesLet <- Op.getOption [] Op.getWordTrees use "-let"
       treesIn  <- Op.getTermTrees use "-in"
       coxLet   <- alphas use treesLet
       coxIn    <- alphas use treesIn
       let base = C.globalFunction $ C.ropGlobal use
       Right $ relmapAdd use (base, coxLet, coxIn)

relmapAdd :: (C.CList c, C.CRel c, B.Write c)
  => C.RopUse c -> ([C.Cop c], [C.NamedCox c], [C.NamedCox c]) -> C.Relmap c
relmapAdd use = C.relmapFlow use . relkitAdd

relkitAdd :: (C.CList c, C.CRel c, B.Write c)
  => ([C.Cop c], [C.NamedCox c], [C.NamedCox c]) -> C.RelkitFlow c
relkitAdd _ Nothing = Right C.relkitNothing
relkitAdd (base, deriv, bodies) (Just he1)
    | null ind  = Right kit2
    | otherwise = Message.unexpTermName
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

relmapSubst :: (C.CList c, C.CRel c, B.Write c)
  => C.RopUse c -> ([C.Cop c], [C.NamedCox c], [C.NamedCox c]) -> C.Relmap c
relmapSubst use = C.relmapFlow use . relkitSubst

relkitSubst :: (C.CList c, C.CRel c, B.Write c)
  => ([C.Cop c], [C.NamedCox c], [C.NamedCox c]) -> C.RelkitFlow c
relkitSubst _ Nothing = Right C.relkitNothing
relkitSubst (base, deriv, bodies) (Just he1)
    | B.sameLength ns ind = Right kit2
    | otherwise           = Message.unexpTermName
    where
      (ns, xs)    = unzip bodies               -- names and expressions
      ns1         = B.headNames he1            -- term names of input relation
      ind         = ns `B.snipIndex` ns1       -- indicies for ns on input relation
      cut         = B.snipOff  ind             -- cutting-ns function
      fore        = B.snipFore ind             -- cutting-ns function
      he2         = B.headChange fore he1      -- heading of output relation
      kit2        = C.relkitJust he2 $ C.RelkitOneToAbOne True kitf2 []
      kitf2 _ cs1 = do xs2 <- C.coxBeta base deriv he1 `mapM` xs
                       cs2 <- C.coxRun cs1 `mapM` xs2
                       Right $ cs2 ++ cut cs1


-- ----------------------  filter

consFilter :: (C.CContent c) => Bool -> C.RopCons c
consFilter b use =
    do treesLet <- Op.getOption [] Op.getWordTrees use "-let"
       treesIn  <- Op.getTrees use "-in"
       coxLet   <- alphas use treesLet
       coxIn    <- alpha  use $ B.treeWrap treesIn
       let base = C.globalFunction $ C.ropGlobal use
       Right $ relmapFilter use (b, base, coxLet, coxIn)

relmapFilter :: (C.CList c, C.CRel c, C.CBool c, B.Write c)
  => C.RopUse c -> (Bool, [C.Cop c], [C.NamedCox c], C.Cox c) -> C.Relmap c
relmapFilter use = C.relmapFlow use . relkitFilter

relkitFilter :: (C.CList c, C.CRel c, C.CBool c, B.Write c)
  => (Bool, [C.Cop c], [C.NamedCox c], C.Cox c) -> C.RelkitFlow c
relkitFilter _ Nothing = Right C.relkitNothing
relkitFilter (which, base, deriv, body) (Just he1) = Right kit2 where
    kit2 = C.relkitJust he1 $ C.RelkitAbPred p
    p cs1 = do e <- C.coxBeta base deriv he1 body
               c <- C.coxRun cs1 e
               case C.isBool c of
                 True  -> Right $ C.gBool c == which
                 False -> Message.reqBool


-- ----------------------  alpha

alpha :: (C.CContent c) => C.RopUse c -> B.TokenTree -> B.Ab (C.Cox c)
alpha = C.coxAlpha . C.globalSyntax . C.ropGlobal

alphas :: (C.CContent c) => C.RopUse c -> [B.Named B.TokenTree] -> B.Ab [C.NamedCox c]
alphas use = mapM (B.namedMapM $ alpha use)


-- ----------------------  range

-- $range
--
--  Add term @\/n@ @0@, @\/n@ @1@, ..., and @\/n@ @9@.
--  
--    > range /n -from 0 -to 9

consRange :: (C.CDec c) => C.RopCons c
consRange use =
  do term <- Op.getTerm use "-term"
     low  <- Op.getInt  use "-from"
     high <- Op.getInt  use "-to"
     Right $ relmapRange use (term, low, high)

relmapRange :: (C.CDec c) => C.RopUse c -> (B.TermName, Int, Int) -> C.Relmap c
relmapRange use = C.relmapFlow use . relkitRange

relkitRange :: (C.CDec c) => (B.TermName, Int, Int) -> C.RelkitFlow c
relkitRange _ Nothing = Right C.relkitNothing
relkitRange (n, low, high) (Just he1) = Right kit2 where
    he2      = B.headCons n he1
    kit2     = C.relkitJust he2 $ C.RelkitOneToMany False kitf2
    kitf2 cs = map (: cs) decs
    decs     = map C.pDecFromInt [low .. high]
