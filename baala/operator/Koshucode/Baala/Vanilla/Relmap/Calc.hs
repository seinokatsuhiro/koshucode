{-# OPTIONS_GHC -Wall #-}

{-|  -}

module Koshucode.Baala.Vanilla.Relmap.Calc
(
  -- * add
  ropConsAdd, relmapAdd, relAdd,
  -- * hold
  ropConsHold, relmapHold, relHold,
) where

import Control.Monad (filterM)

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Core as C
import qualified Koshucode.Baala.Builtin as Kit
import Koshucode.Baala.Vanilla.Type
import Koshucode.Baala.Vanilla.Cop



-- ----------------------  add

ropConsAdd :: C.RopCons VContent
ropConsAdd use =
  do ts <- Kit.getTermTrees use "-term"
     cs <- vanillaNamedContents use ts
     Right $ relmapAdd use cs

relmapAdd :: C.RopUse VContent -> [B.Named (C.PosCox VContent)] -> C.Relmap VContent
relmapAdd use cs = C.relmapCalc use "add" sub where
    sub _ r1 = relAdd cs r1

-- todo: shared term
relAdd :: [B.Named (C.PosCox VContent)] -> B.Rel VContent -> B.Ab (B.Rel VContent)
relAdd cs (B.Rel h1 b1) =
    do let h2 = B.headFrom $ map fst cs
           h3 = Kit.mappend h2 h1
           cs2 = map snd cs
       let run arg = do cs2' <- mapM (C.runCoxH h1 arg) cs2
                        Right $ cs2' ++ arg
       b3 <- mapM run b1
       Right $ B.Rel h3 b3



-- ----------------------  hold

ropConsHold :: C.RopCons VContent
ropConsHold use = do
  t <- Kit.getTree use "-term"
  c <- vanillaContent use t
  Right $ relmapHold use True c

relmapHold :: C.RopUse VContent -> Bool -> (C.PosCox VContent) -> C.Relmap VContent
relmapHold use b cont = C.relmapCalc use "hold" sub where
    sub _ r1 = relHold b cont r1

relHold
    :: (C.CContent c, Show c)
    => Bool                -- ^ Criterion
    -> (C.PosCox c)          -- ^ Predicate
    -> B.AbMap (B.Rel c)   -- ^ Relation-to-relation mapping
relHold b cont (B.Rel h1 b1) = Right . B.Rel h1 =<< filterM f b1 where
    f lits = do lit <- C.runCoxH h1 lits cont
                case lit of
                  x | C.isBool x -> Right $ b == C.getBool x
                  _            -> Left  $ B.AbortReqBoolean (show lit)

