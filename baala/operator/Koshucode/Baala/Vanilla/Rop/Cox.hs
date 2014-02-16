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

consAdd :: (C.CRel c, C.CList c) => C.RopCons c
consAdd use =
  do trees <- Rop.getTermTrees use "-term"
     coxes <- mapM (B.namedMapM $ ropCoxCons use) trees
     Right $ relmapAdd use coxes

relmapAdd :: (C.CRel c, C.CList c) => C.RopUse c -> [B.Named (C.CoxCons c)] -> C.Relmap c
relmapAdd use = C.relmapCalc use . relkitAdd

-- todo: shared term
relkitAdd :: (C.CRel c, C.CList c) => [B.Named (C.CoxCons c)] -> C.RelkitCalc c
relkitAdd coxes h1 = Right $ C.relkit h2 (C.RelkitOneToAbOne False f) where
    ns    = map fst coxes   -- term names
    es    = map snd coxes   -- term expressions
    h2    = B.headAppend ns h1
    f cs1 = do cs2 <- mapM (C.coxRun h1 cs1) es
               Right $ cs2 ++ cs1

ropCoxCons :: C.RopUse c -> B.TokenTree -> B.Ab (C.CoxCons c)
ropCoxCons = C.globalCoxCons . C.ropGlobal


-- ----------------------  hold

consHold :: (C.CContent c) => C.RopCons c
consHold use =
    do trees <- Rop.getTrees use "-expr"
       cox   <- ropCoxCons use $ B.treeWrap trees
       Right $ relmapHold use True cox

relmapHold :: (C.CContent c) => C.RopUse c -> Bool -> C.CoxCons c -> C.Relmap c
relmapHold use b cox = C.relmapCalc use $ relkitHold b cox

relkitHold :: (C.CContent c) => Bool -> C.CoxCons c -> C.RelkitCalc c
relkitHold b cox h1 = Right $ C.relkit h1 (C.RelkitAbPred p) where
    p cs = do c <- C.coxRun h1 cs cox
              case c of
                x | C.isBool x -> Right $ b == C.getBool x
                _ -> Left $ B.AbortAnalysis [] $ B.AAReqBoolean ""

