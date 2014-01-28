{-# OPTIONS_GHC -Wall #-}

{-| Relmap operators using term-content expressions. -}

module Koshucode.Baala.Vanilla.Rop.Cox
(
  -- * add
  ropConsAdd, relmapAdd, relkitAdd,
  -- * hold
  ropConsHold, relmapHold, relkitHold,
) where

import qualified Koshucode.Baala.Base    as B
import qualified Koshucode.Baala.Core    as C
import qualified Koshucode.Baala.Builtin as Rop


-- ----------------------  add

ropConsAdd :: (C.CRel c, C.CList c) => C.RopUse c -> B.Ab (C.Relmap c)
ropConsAdd use =
  do trees <- Rop.getTermTrees use "-term"
     coxes <- mapM (B.namedMapM $ ropCoxCons use) trees
     Right $ relmapAdd use coxes

relmapAdd :: (C.CRel c, C.CList c) => C.RopUse c -> [B.Named (C.CoxCons c)] -> C.Relmap c
relmapAdd use coxes = C.relmapCalc use $ relkitAdd coxes

-- todo: shared term
relkitAdd :: (C.CRel c, C.CList c) => [B.Named (C.CoxCons c)] -> B.Relhead -> B.Ab (C.Relkit c)
relkitAdd coxes h1 = Right $ C.relkit h2 (C.RelkitOneToAbOne False f) where
    ns    = map fst coxes   -- term names
    es    = map snd coxes   -- term expressions
    h2    = B.headFrom ns `B.mappend` h1
    f cs1 = do cs2 <- mapM (C.coxRun h1 cs1) es
               Right $ cs2 ++ cs1

ropCoxCons :: C.RopUse c -> B.TokenTree -> B.Ab (C.CoxCons c)
ropCoxCons = C.globalCoxCons . C.ropGlobal


-- ----------------------  hold

ropConsHold :: (C.CContent c) => C.RopUse c -> B.Ab (C.Relmap c)
ropConsHold use = do
  tree <- Rop.getTree use "-term"
  cox  <- ropCoxCons use tree
  Right $ relmapHold use True cox

relmapHold :: (C.CContent c) => C.RopUse c -> Bool -> C.CoxCons c -> C.Relmap c
relmapHold use b cox = C.relmapCalc use $ relkitHold b cox

relkitHold :: (C.CContent c) => Bool -> C.CoxCons c -> B.Relhead -> B.Ab (C.Relkit c)
relkitHold b cox h1 = Right $ C.relkit h1 (C.RelkitAbPred p) where
    p cs = do c <- C.coxRun h1 cs cox
              case c of
                x | C.isBool x -> Right $ b == C.getBool x
                _ -> Left $ B.AbortAnalysis [] $ B.AAReqBoolean (show c)

