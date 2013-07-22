{-# OPTIONS_GHC -Wall #-}

{-|  -}

module Koshucode.Baala.Vanilla.Relmap.Calc
(
  -- * add
  relopAdd, relmapAdd, relAdd,
  -- * hold
  relopHold, relmapHold, relHold,
) where

import Control.Monad (filterM)

import Koshucode.Baala.Base.Abort
import Koshucode.Baala.Base.Content
import Koshucode.Baala.Core.Relmap

import Koshucode.Baala.Minimal.OpKit as Kit
import qualified Koshucode.Baala.Minimal as Mini

import Koshucode.Baala.Vanilla.Value.Relval
import Koshucode.Baala.Vanilla.Cop



-- ----------------------  add

relopAdd :: Kit.Relop VContent
relopAdd use =
  do ts <- Mini.getTermTrees use "-term"
     cs <- vanillaNamedContents use ts
     Right $ relmapAdd use cs

relmapAdd :: OpUse VContent -> [Named (PosCox VContent)] -> Relmap VContent
relmapAdd use cs = Kit.relmapCalc use "add" sub where
    sub _ r1 = relAdd cs r1

-- todo: shared term
relAdd :: [Named (PosCox VContent)] -> Rel VContent -> AbOr (Rel VContent)
relAdd cs (Rel h1 b1) =
    do let h2 = Kit.headFrom $ map fst cs
           h3 = Kit.mappend h2 h1
           cs2 = map snd cs
       let run arg = do cs2' <- mapM (runCoxH h1 arg) cs2
                        Right $ cs2' ++ arg
       b3 <- mapM run b1
       Right $ Rel h3 b3



-- ----------------------  hold

relopHold :: Kit.Relop VContent
relopHold use = do
  t <- Mini.getTree use "-term"
  c <- vanillaContent use t
  Right $ relmapHold use True c

relmapHold :: OpUse VContent -> Bool -> (PosCox VContent) -> Relmap VContent
relmapHold use b cont = Kit.relmapCalc use "hold" sub where
    sub _ r1 = relHold b cont r1

relHold
    :: (CContent c, Show c)
    => Bool            -- ^ Criterion
    -> (PosCox c)      -- ^ Predicate
    -> AbMap (Rel c)   -- ^ Relation-to-relation mapping
relHold b cont (Rel h1 b1) = Right . Rel h1 =<< filterM f b1 where
    f lits = do lit <- runCoxH h1 lits cont
                case lit of
                  x | isBool x -> Right $ b == getBool x
                  _            -> Left  $ AbortReqBoolean (show lit)

