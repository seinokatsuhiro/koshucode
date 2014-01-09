{-# OPTIONS_GHC -Wall #-}

{-|  -}

module Koshucode.Baala.Vanilla.Relmap.Cox
(
  -- * add
  ropConsAdd, relmapAdd, relfyAdd,
  -- * hold
  ropConsHold, relmapHold, relfyHold,
) where

import qualified Koshucode.Baala.Base         as B
import qualified Koshucode.Baala.Core         as C
import qualified Koshucode.Baala.Builtin      as Rop
import qualified Koshucode.Baala.Vanilla.Type as Rop
import qualified Koshucode.Baala.Vanilla.Cop  as Rop



-- ----------------------  add

ropConsAdd :: C.RopCons Rop.VContent
ropConsAdd use =
  do trees <- Rop.getTermTrees use "-term"
     coxes <- mapM Rop.vanillaCoxNamed trees
     Right $ relmapAdd use coxes

relmapAdd :: (C.CRel c, C.CList c) => C.RopUse c -> [B.Named (C.CoxCons c)] -> C.Relmap c
relmapAdd use coxes = C.relmapCalc use $ relfyAdd coxes

-- todo: shared term
relfyAdd :: (C.CRel c, C.CList c) => [B.Named (C.CoxCons c)] -> B.Relhead -> B.Ab (C.Relfy c)
relfyAdd coxes h1 = Right $ C.relfy h2 (C.RelfyOneToAbOne False f) where
    ns    = map fst coxes   -- term names
    es    = map snd coxes   -- term expressions
    h2    = B.headFrom ns `B.mappend` h1
    f cs1 = do cs2 <- mapM (C.coxRun h1 cs1) es
               Right $ cs2 ++ cs1



-- ----------------------  hold

ropConsHold :: C.RopCons Rop.VContent
ropConsHold use = do
  tree <- Rop.getTree use "-term"
  cox  <- Rop.vanillaCox tree
  Right $ relmapHold use True cox

relmapHold :: (C.CContent c) => C.RopUse c -> Bool -> C.CoxCons c -> C.Relmap c
relmapHold use b cox = C.relmapCalc use $ relfyHold b cox

relfyHold :: (C.CContent c) => Bool -> C.CoxCons c -> B.Relhead -> B.Ab (C.Relfy c)
relfyHold b cox h1 = Right $ C.relfy h1 (C.RelfyAbPred p) where
    p cs = do c <- C.coxRun h1 cs cox
              case c of
                x | C.isBool x -> Right $ b == C.getBool x
                _ -> Left $ B.AbortAnalysis [] $ B.AAReqBoolean (show c)

