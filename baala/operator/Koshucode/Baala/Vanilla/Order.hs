{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Vanilla.Order
( OrderCap (..),
  orders,
  sortByName,
) where

import qualified Data.List as List
import Koshucode.Baala.Base.Prelude
import qualified Koshucode.Baala.Base.Prelude.Position as Pos

-- ----------------------  Order

data OrderCap v 
    = Asc v | Desc v
      deriving (Show, Eq)

instance (Ord v) => Ord (OrderCap v) where
    compare (Asc  x) (Asc  y) = compare x y
    compare (Desc x) (Desc y) = compare y x
    compare (Asc  _) (Desc _) = LT
    compare (Desc _) (Asc  _) = GT

ascList :: [v -> OrderCap v]
ascList = Asc : ascList

orderCap :: OrderCap a -> b -> OrderCap b
orderCap (Asc  _) = Asc
orderCap (Desc _) = Desc

caps :: [v -> OrderCap v] -> [v] -> [OrderCap v]
caps ords = zipWith ($) (ords ++ ascList)

uncap :: OrderCap v -> v
uncap (Asc  x) = x
uncap (Desc x) = x

uncaps :: [OrderCap v] -> [v]
uncaps = map uncap

-- mapFirst :: (a -> b) -> [(a, c)] -> [(b, c)]
-- mapFirst f = map g where
--     g (k,v) = (f k, v)

sortBy
    :: (Ord a, Ord b)
    => [a -> OrderCap a]
    -> [([a], b)]
    -> [([OrderCap a], b)]
sortBy ords xs = xs3 where
    xs2 = mapmapFst (caps ords) xs
    xs3 = List.sort xs2

sortByName :: (Ord v, Eq n) => [OrderCap n] -> [n] -> Map [[v]]
sortByName ords ns xs = xs3 where
    ords2 = map orderCap ords
    ns2   = uncaps ords
    p     = ns2 `Pos.sharedIndex` ns
    xs2   = map f xs
    f x   = (Pos.indexPick p x, x)
    xs3   = map snd $ sortBy ords2 xs2

-- pull :: [Int] -> [v] -> [v]
-- pull ns xs = Pos.indexPick ns xs ++ Pos.drop ns xs

{-# ANN orders "HLint: ignore Use String" #-}
orders :: String -> [OrderCap [Char]]
orders ns = map ord ns2 where
    ns2 = words ns
    ord (      '/' : n) = Asc  $ '/' : n
    ord ('+' : '/' : n) = Asc  $ '/' : n
    ord ('-' : '/' : n) = Desc $ '/' : n
    ord n               = error $ "unknown name: " ++ n

