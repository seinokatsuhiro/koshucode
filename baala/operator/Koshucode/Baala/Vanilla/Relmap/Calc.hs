{-# OPTIONS_GHC -Wall #-}

-- | Relational mappers

module Koshucode.Baala.Vanilla.Relmap.Calc
(
-- * hold
  relopHold, relmapHold, relHold,
-- * add
  relopAdd, relmapAdd, relAdd,
-- * range
  relopRange, relmapRange,
-- * limit
  limit,
) where

import qualified Data.List as List
import Control.Monad (filterM)

import Koshucode.Baala.Base.Abort
import Koshucode.Baala.Base.Content
import Koshucode.Baala.Base.Relmap

import Koshucode.Baala.Minimal.OpKit as Kit
import Koshucode.Baala.Vanilla.Value.Relval
import Koshucode.Baala.Vanilla.Cop
import Koshucode.Baala.Vanilla.Order as Kit
import qualified Koshucode.Baala.Minimal as Mini



-- ----------------------  hold

relopHold :: Kit.Relop VContent
relopHold use = do
  t <- Mini.getTree use "-term"
  c <- vanillaContent use t
  Right $ relmapHold use True c

relmapHold :: OpUse VContent -> Bool -> (PosContent VContent) -> Relmap VContent
relmapHold use b cont = Kit.relmapCalc use "hold" sub where
    sub _ r1 = relHold b cont r1

relHold :: Bool -> (PosContent VContent) -> Rel VContent -> AbOr (Rel VContent)
relHold b cont (Rel h1 b1) =
    do b2 <- filterM f b1
       Right $ Rel h1 b2
    where
      f arg = do c <- runContent (cont h1) arg
                 case c of
                   VBool b' -> Right $ b == b'
                   _        -> Left $ AbortReqBoolean (show c)



-- ----------------------  add

relopAdd :: Kit.Relop VContent
relopAdd use =
  do ts <- Mini.getTermTrees use "-term"
     cs <- vanillaNamedContents use ts
     Right $ relmapAdd use cs

relmapAdd :: OpUse VContent -> [Named (PosContent VContent)] -> Relmap VContent
relmapAdd use cs = Kit.relmapCalc use "add" sub where
    sub _ r1 = relAdd cs r1

-- todo: shared term
relAdd :: [Named (PosContent VContent)] -> Rel VContent -> AbOr (Rel VContent)
relAdd cs (Rel h1 b1) =
    do let h2 = Kit.headFrom $ map fst cs
           h3 = Kit.mappend h2 h1
           cs2 = map g cs
           g (_, c) = c h1
           run arg = do cs2' <- mapM (`runContent` arg) cs2
                        Right $ cs2' ++ arg
       b3 <- mapM run b1
       Right $ Rel h3 b3



-- ----------------------  limit (quota)

-- | Keep leading tuples.
limit :: (Ord v) => Kit.OpUse v -> Int -> String -> Kit.Relmap v
limit use c ns = Kit.relmapCalc use "limit" (limit2 c ns)

limit2 :: (Ord v) => Int -> String -> a -> AbMap (Rel v)
limit2 c ns _ (Rel h1 b1) = Right $ Rel h1 b2 where
    b2   = List.take c $ Kit.sortByName ords (headNames h1) b1
    ords = Kit.orders ns



-- ----------------------  range

relopRange :: Kit.Relop VContent
relopRange use = do
  term <- Mini.getTerm use "-term"
  low  <- Mini.getInt  use "-from"
  high <- Mini.getInt  use "-to"
  Right $ relmapRange use term low high

relmapRange :: (CInt v) => OpUse v -> String -> Int -> Int -> Relmap v
relmapRange use n low high = Kit.relmapCalc use "range" sub where
    sub _ r1 = relRange n low high r1

relRange :: (CInt v) => String -> Int -> Int -> AbMap (Rel v)
relRange n low high (Rel h1 b1) = Right $ Rel h2 b2 where
    h2   = Kit.mappend (Kit.headFrom [n]) h1
    b2   = concatMap g b1
    ys   = map putInt [low .. high]
    g xs = map (: xs) ys

{-
divide :: String -> Kit.Relmap VContent
divide ns2 = Kit.flow "divide" $ Kit.withP divide2 ns2

divide2 :: [String] -> Rel VContent -> Rel VContent
divide2 ns2 (Rel h1 b1) = Rel h2 b2 where
    h2  = Kit.mappend (Kit.headFrom ns2) h1
    b2  = concatMap g b1

    p = ns2 `Kit.look` names (headTerms h1)
    g | p `Kit.like` "--vv"  = Kit.ap f
      | otherwise          = const []

    [_,_,x,y] = p
    f the arg = if the y == putInt 0
                then []
                else [binv quot (the x) (the y) :
                      binv rem  (the x) (the y) : arg]
-}

