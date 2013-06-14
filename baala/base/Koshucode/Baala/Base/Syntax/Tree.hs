{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Syntax.Tree
( -- * Paren table
  ParenType, TypeParen, parenTable,
  -- * Parsing
  Tree (..), tree, trees,
  untree, untrees, undouble
) where

import Data.Generics (Data, Typeable)
import Koshucode.Baala.Base.Prelude

-- ----------------------  Paren table

-- | Get a paren type
type ParenType a = a -> Int

-- | Get paren from type
type TypeParen a = Int -> (a, a)

-- | Make 'ParenType' and 'TypeParen' functions
--   from a type-open-close table.
-- 
--   Make paren/type functions from @()@ and @[]@.
-- 
--   >>> let (pt, tp) = parenTable [(1,'(',')'), (2,'[',']')]
-- 
--   Get paren types for each chars.
--   Types of open parens are positive integer,
--   and closes are negative.
-- 
--   >>> map pt "ab(cd[ef])g"
--   [0,0,1,0,0,2,0,0,-2,-1,0]
-- 
--   Get an open-close pair of parens from its type.
-- 
--   >>> tp 2
--   ('[', ']')
-- 
parenTable :: (Eq a) => [(Int, a, a)] -> (ParenType a, TypeParen a)
parenTable xs = (parenType, typeParen) where
    parenTypeTable = map parenOpen xs ++ map parenClose xs
    parenOpen  (n,o,_) = (o,n)
    parenClose (n,_,c) = (c,-n)
    parenType p =
        case lookup p parenTypeTable of
          Just n  -> n
          Nothing -> 0

    typeParenTable = map typeOpenClose xs
    typeOpenClose (n,o,c) = (n,(o,c))
    typeParen n =
        case lookup n typeParenTable of
          Just p  -> p
          Nothing -> error $ "unknown paren type: " ++ show n

-- ----------------------  Tree

-- | Tree of leaf and branch
data Tree a
    = TreeL a             -- ^ Terminal of a tree
    | TreeB Int [Tree a]  -- ^ Paren-type and subtrees
      deriving (Show, Eq, Ord, Data, Typeable)

instance Functor Tree where
    fmap f (TreeL x)     = TreeL (f x)
    fmap f (TreeB n xs) = TreeB n $ map (fmap f) xs

-- | Convert a list of elements to a single tree
tree :: (Show a) => ParenType a -> [a] -> Tree a
tree p = TreeB 0 . trees p

-- | Convert a list of elements to trees
trees :: (Show a) => ParenType a -> [a] -> [Tree a]
trees parenType xs = fst $ loop xs 0 where
    add a xs2 p = mapFst (a :) $ loop xs2 p

    loop [] _ = ([], [])
    loop (x : xs2) p
        -- non paren
        | px == 0  = add (TreeL x) xs2 p
        -- open paren
        | px > 0   = let (trees2, xs3) = loop xs2 px
                     in  add (TreeB px trees2) xs3 p
        -- close paren
        | px < 0 && px == -p = ([], xs2)
        -- unknown token
        | otherwise = error $ "mismatched paren: " ++ show x

        where px = parenType x

-- ----------------------  Utility

-- | Convert tree to list of tokens
untrees :: TypeParen a -> [Tree a] -> [a]
untrees typeParen = concatMap (untree typeParen)

-- | Convert tree to list of tokens
untree :: TypeParen a -> Tree a -> [a]
untree typeParen = loop where
    loop (TreeL x) = [x]
    loop (TreeB n xs) =
        let (open, close) = typeParen n
        in [open] ++ concatMap loop xs ++ [close]

-- | Simplify tree by removing double parens,
--   like @((a))@ to @(a)@.
undouble :: Tree a -> Tree a
undouble (TreeB n xs) =
    case map undouble xs of
      [x] -> x
      xs2 -> TreeB n xs2
undouble x = x

-- e1 = TreeB 2 [TreeB 1 [TreeB 0 [TreeL 0]]]
-- e2 = TreeB 2 [e1, TreeB 1 [TreeB 0 [TreeL 0]]]
-- e3 = undouble e2

