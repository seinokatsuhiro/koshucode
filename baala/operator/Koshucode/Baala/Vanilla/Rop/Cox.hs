{-# OPTIONS_GHC -Wall #-}

{-| Relmap operators using term-content expressions. -}

module Koshucode.Baala.Vanilla.Rop.Cox
(
  -- * add
  ropConsAdd, relmapAdd, relfyAdd,
  -- * hold
  ropConsHold, relmapHold, relfyHold,
) where

import qualified Koshucode.Baala.Base         as B
import qualified Koshucode.Baala.Core         as C
import qualified Koshucode.Baala.Builtin      as Rop
import qualified Koshucode.Baala.Vanilla.Type as Type


-- ----------------------  add

ropConsAdd :: Type.VRopCons
ropConsAdd use =
  do trees <- Rop.getTermTrees use "-term"
     coxes <- mapM (B.namedMapM $ vanillaCox use) trees
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

vanillaCox :: C.RopUse Type.VContent -> B.TokenTree -> B.Ab (C.CoxCons Type.VContent)
vanillaCox = vanillaCoxFrom . C.globalCops . C.ropGlobal

vanillaCoxFrom
  :: (C.CContent c) => ([C.Cop c], [B.Named B.InfixHeight])
  -> B.TokenTree -> B.Ab (C.CoxCons c)
vanillaCoxFrom (cops, htab) tree =
    case B.infixToPrefix ht tree of
      Right tree2 -> C.coxCons cops $ B.undouble (== 1) tree2
      Left  xs    -> Left $ B.AbortSyntax (map snd xs)
                          $ B.ASAmbInfixes $ map detail xs
    where
      ht = B.infixHeight text htab

      text (B.TWord _ 0 w) = Just w
      text _ = Nothing

      detail (Right n, tok) = detailText tok "right" n
      detail (Left  n, tok) = detailText tok "left"  n
      detailText tok dir n = B.tokenContent tok ++ " : " ++ dir ++ " " ++ show n


-- ----------------------  hold

ropConsHold :: Type.VRopCons
ropConsHold use = do
  tree <- Rop.getTree use "-term"
  cox  <- vanillaCox use tree
  Right $ relmapHold use True cox

relmapHold :: (C.CContent c) => C.RopUse c -> Bool -> C.CoxCons c -> C.Relmap c
relmapHold use b cox = C.relmapCalc use $ relfyHold b cox

relfyHold :: (C.CContent c) => Bool -> C.CoxCons c -> B.Relhead -> B.Ab (C.Relfy c)
relfyHold b cox h1 = Right $ C.relfy h1 (C.RelfyAbPred p) where
    p cs = do c <- C.coxRun h1 cs cox
              case c of
                x | C.isBool x -> Right $ b == C.getBool x
                _ -> Left $ B.AbortAnalysis [] $ B.AAReqBoolean (show c)

