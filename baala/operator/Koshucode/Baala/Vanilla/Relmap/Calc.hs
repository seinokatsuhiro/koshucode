{-# OPTIONS_GHC -Wall #-}

-- | Relational mappers

module Koshucode.Baala.Vanilla.Relmap.Calc
( holdBody
, valBody
, limit
--, divide, range
) where

import Koshucode.Baala.Minimal.OpKit as Kit
import Koshucode.Baala.Vanilla.Value.Relval
import Koshucode.Baala.Vanilla.Calc as Calc
import Koshucode.Baala.Vanilla.Order as Kit
import qualified Data.List as List



-- ----------------------  General calculations

-- | Make relmap function for @hold@ operator.
holdBody :: (Val -> Val -> Bool) -> TokenTree -> a -> RelmapFun Val
holdBody is e _ (Rel h1 b1) = Rel h1 b2 where
    b2      = filter f b1
    f arg   = calc arg `is` boolValue True
    calc    = Calc.makeCalc h1 e

-- | Make relmap function for @val@ operator.
valBody :: [TokenTree] -> a -> RelmapFun Val
valBody e _ (Rel h1 b1) = Rel h3 b3 where
    h3      = Kit.mappend h2 h1
    b3      = map f b1
    f arg   = map ($ arg) f2 ++ arg   -- todo: shared term
    (h2,f2) = Calc.makeHeadCalcs h1 e



-- ----------------------  Restriction

-- | Keep leading tuples.
limit :: (Ord v) => Kit.OpUse v -> Int -> String -> Kit.Relmap v
limit use c ns = Kit.relmapCalc use "limit" (limit2 c ns)

limit2 :: (Ord v) => Int -> String -> a -> Rel v -> Rel v
limit2 c ns _ (Rel h1 b1) = Rel h1 b2 where
    b2   = List.take c $ Kit.sortByName ords (headNames h1) b1
    ords = Kit.orders ns



-- ----------------------  Special calculations

{-
range :: String -> Kit.Relmap Val
range ns2 = Kit.flow "range" $ Kit.withP range2 ns2

range2 :: [String] -> Rel Val -> Rel Val
range2 ns2 (Rel h1 b1) = Rel h2 b2 where
    --h2  = unionUpTerm ns2 h1
    h2  = Kit.mappend (Kit.headFrom ns2) h1
    b2  = concatMap sel b1

    pos = ns2 `Kit.look` names (headTerms h1)
    sel | pos `Kit.like` "-vv"  = Kit.ap f
        | otherwise           = const []

    [_,from,to] = pos
    f the arg = map (: arg) $ valRangeMinMax (the from) (the to)

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

