{-# OPTIONS_GHC -Wall #-}

-- | Relational mappers

module Koshucode.Baala.Vanilla.Relmap.Calc
(
-- * hold
  relopHold, relmapHold, relHold,
-- * val
  relopAdd, relmapAdd, relAdd,
-- * range
  relopRange, relmapRange,
-- * limit
  limit,
) where

import Koshucode.Baala.Base.Content.Run
import Koshucode.Baala.Minimal.OpKit as Kit
import Koshucode.Baala.Vanilla.Value.Relval
import Koshucode.Baala.Vanilla.Order as Kit
import qualified Koshucode.Baala.Minimal as Mini
import qualified Data.List as List



runContent2 :: Content Val -> [Val] -> Val
runContent2 c arg =
    case runContent c arg of
      Right a  -> a
      Left _   -> Nov


-- ----------------------  hold

relopHold :: Kit.Relop Val
relopHold = relopHoldFor2 (==)

relopHoldFor2 :: (Val -> Val -> Bool) -> Kit.Relop Val
relopHoldFor2 test use = do
  tree <- Mini.getTree use "-term"
  cont <- vanillaContent [] tree
  Right $ relmapHold use test cont

relmapHold :: OpUse Val -> (Val -> Val -> Bool) -> (Relhead -> Content Val) -> Relmap Val
relmapHold use test cont = Kit.relmapCalc use "hold" sub where
    sub _ r1 = relHold test cont r1

relHold :: (Val -> Val -> Bool) -> (Relhead -> Content Val) -> Rel Val -> Rel Val
relHold test cont (Rel h1 b1) = Rel h1 b2 where
    b2      = filter f b1
    f arg   = runContent2 (cont h1) arg `test` boolValue True



-- ----------------------  add

relopAdd :: Kit.Relop Val
relopAdd use =
  do trees <- Mini.getTermTrees use "-term"
     cs    <- mapM (vanillaNamedContent []) trees
     Right $ relmapAdd use cs

relmapAdd :: OpUse Val -> [Named (Relhead -> Content Val)] -> Relmap Val
relmapAdd use cs = Kit.relmapCalc use "add" sub where
    sub _ r1 = relAdd cs r1

relAdd :: [Named (Relhead -> Content Val)] -> Map (Rel Val)
relAdd cs (Rel h1 b1) = Rel h3 b3 where
    h3      = Kit.mappend h2 h1
    b3      = map f b1
    f arg   = map (`runContent2` arg) cs2 ++ arg   -- todo: shared term
    cs2     = map g cs
    g (_,c) = c h1
    h2      = Kit.headFrom $ map fst cs



-- ----------------------  limit

-- | Keep leading tuples.
limit :: (Ord v) => Kit.OpUse v -> Int -> String -> Kit.Relmap v
limit use c ns = Kit.relmapCalc use "limit" (limit2 c ns)

limit2 :: (Ord v) => Int -> String -> a -> Rel v -> Rel v
limit2 c ns _ (Rel h1 b1) = Rel h1 b2 where
    b2   = List.take c $ Kit.sortByName ords (headNames h1) b1
    ords = Kit.orders ns



-- ----------------------  range

relopRange :: Kit.Relop Val
relopRange use = do
  term <- Mini.getTerm use "-term"
  low  <- Mini.getInt  use "-from"
  high <- Mini.getInt  use "-to"
  Right $ relmapRange use term low high

relmapRange :: (IntValue v) => OpUse v -> String -> Int -> Int -> Relmap v
relmapRange use n low high = Kit.relmapCalc use "range" sub where
    sub _ r1 = relRange n low high r1

relRange :: (IntValue v) => String -> Int -> Int -> Map (Rel v)
relRange n low high (Rel h1 b1) = Rel h2 b2 where
    h2   = Kit.mappend (Kit.headFrom [n]) h1
    b2   = concatMap g b1
    ys   = map intValue [low .. high]
    g xs = map (: xs) ys

{-
divide :: String -> Kit.Relmap Val
divide ns2 = Kit.flow "divide" $ Kit.withP divide2 ns2

divide2 :: [String] -> Rel Val -> Rel Val
divide2 ns2 (Rel h1 b1) = Rel h2 b2 where
    h2  = Kit.mappend (Kit.headFrom ns2) h1
    b2  = concatMap g b1

    p = ns2 `Kit.look` names (headTerms h1)
    g | p `Kit.like` "--vv"  = Kit.ap f
      | otherwise          = const []

    [_,_,x,y] = p
    f the arg = if the y == intValue 0
                then []
                else [binv quot (the x) (the y) :
                      binv rem  (the x) (the y) : arg]
-}

