{-# OPTIONS_GHC -Wall #-}

{-|  -}

module Koshucode.Baala.Vanilla.Relmap.Calc
(
  -- * add
  ropConsAdd, relmapAdd, relfyAdd,
  -- * hold
  ropConsHold, relmapHold, relfyHold,
) where

import qualified Koshucode.Baala.Base    as B
import qualified Koshucode.Baala.Core    as C
import qualified Koshucode.Baala.Builtin as Rop
import Koshucode.Baala.Vanilla.Type
import Koshucode.Baala.Vanilla.Cop



-- ----------------------  add

ropConsAdd :: C.RopCons VContent
ropConsAdd use =
  do trees <- Rop.getTermTrees use "-term"
     coxes <- mapM vanillaNamedCox trees
     Right $ relmapAdd use coxes

relmapAdd :: C.RopUse VContent -> [B.Named (C.CoxPos VContent)] -> C.Relmap VContent
relmapAdd use cs = C.relmapCalc use $ relfyAdd cs

-- todo: shared term
relfyAdd
    :: [B.Named (C.CoxPos VContent)]
    -> B.Relhead
    -> B.Ab (C.Relfy VContent)
relfyAdd xs h1 = Right $ C.relfy h2 (C.RelfyOneToAbOne False f) where
    ns    = map fst xs  -- term names
    es    = map snd xs  -- term expressions
    h2    = B.mappend (B.headFrom ns) h1
    f cs1 = do cs2 <- mapM (C.coxRun h1 cs1) es
               Right $ cs2 ++ cs1



-- ----------------------  hold

ropConsHold :: C.RopCons VContent
ropConsHold use = do
  tree <- Rop.getTree use "-term"
  cox  <- vanillaCox tree
  Right $ relmapHold use True cox

relmapHold :: C.RopUse VContent -> Bool -> (C.CoxPos VContent) -> C.Relmap VContent
relmapHold use b cox = C.relmapCalc use $ relfyHold b cox

relfyHold
    :: (C.CContent c, Show c)
    => Bool              -- ^ Criterion
    -> (C.CoxPos c)      -- ^ Predicate
    -> B.Relhead         -- ^ Heading of input relation
    -> B.Ab (C.Relfy c)  -- ^ Relfier for output relation
relfyHold b cox h1 = Right $ C.relfy h1 (C.RelfyAbPred p) where
    p cs = do c <- C.coxRun h1 cs cox
              case c of
                x | C.isBool x -> Right $ b == C.getBool x
                _ -> Left $ B.AbortAnalysis [] $ B.AAReqBoolean (show c)

