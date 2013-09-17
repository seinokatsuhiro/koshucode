{-# OPTIONS_GHC -Wall #-}

{-|  -}

module Koshucode.Baala.Vanilla.Relmap.Calc
(
  -- * add
  ropConsAdd, relmapAdd, relfyAdd,
  -- * hold
  ropConsHold, relmapHold, relfyHold,
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
relmapAdd use cs = C.relmapCalc use "add" fy where
    fy _ = relfyAdd cs

-- todo: shared term
relfyAdd
    :: [B.Named (C.PosCox VContent)]
    -> B.Relhead
    -> B.Ab (C.Relfy VContent)
relfyAdd xs h1 = Right $ C.Relfy h2 (C.RelfyOneToAbOne f) where
    ns    = map fst xs  -- term names
    es    = map snd xs  -- term expressions
    h2    = Builtin.mappend (B.headFrom ns) h1
    f cs1 = do cs2 <- mapM (C.runCoxH h1 cs1) es
               Right $ cs2 ++ cs1



-- ----------------------  hold

ropConsHold :: C.RopCons VContent
ropConsHold use = do
  t <- Builtin.getTree use "-term"
  c <- vanillaContent use t
  Right $ relmapHold use True c

relmapHold :: C.RopUse VContent -> Bool
           -> (C.PosCox VContent) -> C.Relmap VContent
relmapHold use b cont = C.relmapCalc use "hold" fy where
    fy _ = relfyHold b cont

relfyHold
    :: (C.CContent c, Show c)
    => Bool              -- ^ Criterion
    -> (C.PosCox c)      -- ^ Predicate
    -> B.Relhead         -- ^ Heading of input relation
    -> B.Ab (C.Relfy c)  -- ^ Relfier for output relation
relfyHold b cox h1 = Right $ C.Relfy h1 (C.RelfyAbPred p) where
    p cs = do c <- C.runCoxH h1 cs cox
              case c of
                x | C.isBool x -> Right $ b == C.getBool x
                _ -> Left $ B.AbortReqBoolean (show c)

