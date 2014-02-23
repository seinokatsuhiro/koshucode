{-# OPTIONS_GHC -Wall #-}

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
    do treesLet  <- Rop.getMaybe Rop.getWordTrees use "-let"
       treesTerm <- Rop.getTermTrees use "-term"
       coxLet    <- maybe (Right []) (ncox use []) treesLet
       coxTerm   <- ncox use coxLet treesTerm
       Right $ relmapAdd use coxTerm

relmapAdd :: (C.CRel c, C.CList c)=> C.RopUse c -> [C.NamedCox c] -> C.Relmap c
relmapAdd use = C.relmapCalc use . relkitAdd

-- todo: shared term
relkitAdd :: (C.CRel c, C.CList c) => [C.NamedCox c] -> C.RelkitCalc c
relkitAdd coxTerm h1 =
    Right $ C.relkit h2 (C.RelkitOneToAbOne False f)
        where ns    = map fst coxTerm   -- term names
              es    = map (C.coxPosition . snd) coxTerm   -- term expressions
              h2    = B.headAppend ns h1
              f cs1 = do cs2 <- C.coxRun h1 cs1 `mapM` es
                         Right $ cs2 ++ cs1

ncox :: (C.CContent c) => C.RopUse c -> [C.NamedCox c]
  -> [B.Named B.TokenTree] -> B.Ab [C.NamedCox c]
ncox use dict = mapM (B.namedMapM $ cox use dict)

cox :: (C.CContent c) => C.RopUse c -> [C.NamedCox c]
  -> B.TokenTree -> B.Ab (C.Cox c)
cox use dict = C.coxCons cops dict where
    cops = C.globalCops $ C.ropGlobal use


-- ----------------------  hold

consHold :: (C.CContent c) => C.RopCons c
consHold use =
    do treesLet  <- Rop.getMaybe Rop.getWordTrees use "-let"
       treesExpr <- Rop.getTrees use "-expr"
       coxLet    <- maybe (Right []) (ncox use []) treesLet
       coxExpr   <- cox use coxLet $ B.treeWrap treesExpr
       Right $ relmapHold use (True, coxExpr)

relmapHold :: (C.CContent c) => C.RopUse c -> (Bool, C.Cox c) -> C.Relmap c
relmapHold use = C.relmapCalc use . relkitHold

relkitHold :: (C.CContent c) => (Bool, C.Cox c) -> C.RelkitCalc c
relkitHold (b, coxExpr) h1 = Right $ C.relkit h1 (C.RelkitAbPred p) where
    p cs = do c <- C.coxRun h1 cs (C.coxPosition coxExpr)
              case c of
                x | C.isBool x -> Right $ b == C.getBool x
                _ -> Left $ B.AbortAnalysis [] $ B.AAReqBoolean ""

