{-# OPTIONS_GHC -Wall #-}

{-|  -}

module Koshucode.Baala.Vanilla.Relmap.Calc
(
  -- * add
  ropConsAdd, relmapAdd, relgenAdd,
  -- * hold
  ropConsHold, relmapHold, relgenHold,
) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C
import qualified Koshucode.Baala.Builtin as Builtin
import Koshucode.Baala.Vanilla.Type
import Koshucode.Baala.Vanilla.Cop



-- ----------------------  add

ropConsAdd :: C.RopCons VContent
ropConsAdd use =
  do ts <- Builtin.getTermTrees use "-term"
     cs <- vanillaNamedContents use ts
     Right $ relmapAdd use cs

relmapAdd :: C.RopUse VContent -> [B.Named (C.PosCox VContent)] -> C.Relmap VContent
relmapAdd use cs = C.relmapCalc use "add" gen where
    gen _ = relgenAdd cs

-- todo: shared term
relgenAdd :: [B.Named (C.PosCox VContent)]
          -> B.Relhead
          -> B.Ab (C.Relgen VContent)
relgenAdd cs h1 = Right $ C.Relgen h3 (C.RelgenOneToAbOne run) where
    h2 = B.headFrom $ map fst cs
    h3 = Builtin.mappend h2 h1
    cs2 = map snd cs
    run arg = do cs2' <- mapM (C.runCoxH h1 arg) cs2
                 Right $ cs2' ++ arg



-- ----------------------  hold

ropConsHold :: C.RopCons VContent
ropConsHold use = do
  t <- Builtin.getTree use "-term"
  c <- vanillaContent use t
  Right $ relmapHold use True c

relmapHold :: C.RopUse VContent -> Bool -> (C.PosCox VContent) -> C.Relmap VContent
relmapHold use b cont = C.relmapCalc use "hold" gen where
    gen _ = relgenHold b cont

relgenHold
    :: (C.CContent c, Show c)
    => Bool               -- ^ Criterion
    -> (C.PosCox c)       -- ^ Predicate
    -> B.Relhead          -- ^ Heading of input relation
    -> B.Ab (C.Relgen c)  -- ^ Generator for output Relation
relgenHold b cont h1 = Right $ C.Relgen h1 (C.RelgenAbPred p) where
    p lits = do lit <- C.runCoxH h1 lits cont
                case lit of
                  x | C.isBool x -> Right $ b == C.getBool x
                  _ -> Left $ B.AbortReqBoolean (show lit)

