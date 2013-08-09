{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Syntax.Tree
( 
  -- * Data type
  Tree (..),

  -- * Parsing
  tree, trees,
  untree, untrees,
  undouble,

  -- * Paren table
  ParenType, TypeParen,
  parenTable
) where

import Data.Generics (Data, Typeable)
import Koshucode.Baala.Base.Prelude



-- ----------------------  Tree

{-| Tree of leaf and branch. -}
data Tree a
    = TreeL a             -- ^ Leaf. Terminal of tree.
    | TreeB Int [Tree a]  -- ^ Branch. Paren-type and subtrees.
      deriving (Show, Eq, Ord, Data, Typeable)

instance Functor Tree where
    fmap f (TreeL x)    = TreeL (f x)
    fmap f (TreeB n xs) = TreeB n $ map (fmap f) xs

{-| Convert a list of elements to a single tree. -}
tree :: (Show a) => ParenType a -> [a] -> Tree a
tree p = TreeB 1 . trees p

{-| Convert a list of elements to trees. -}
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

{-| Convert tree to list of tokens. -}
untrees :: TypeParen a -> [Tree a] -> [a]
untrees typeParen = concatMap (untree typeParen)

{-| Convert tree to list of tokens. -}
untree :: TypeParen a -> Tree a -> [a]
untree typeParen = loop where
    loop (TreeL x) = [x]
    loop (TreeB n xs) =
        let (open, close) = typeParen n
        in [open] ++ concatMap loop xs ++ [close]

{-| Simplify tree by removing double parens,
    like @((a))@ to @(a)@.

    >>> undouble $ TreeB 0 [Tree 0 [TreeL "A", TreeL "B"]]
    Tree 0 [TreeL "A", TreeL "B"]
  -}
undouble :: Map (Tree a)
undouble (TreeB n xs) =
    case map undouble xs of
      [x] -> x
      xs2 -> TreeB n xs2
undouble x = x

-- e1 = TreeB 2 [TreeB 1 [TreeB 0 [TreeL 0]]]
-- e2 = TreeB 2 [e1, TreeB 1 [TreeB 0 [TreeL 0]]]
-- e3 = undouble e2



-- ----------------------  Paren table

{-| Get a paren type. -}
type ParenType a = a -> Int

{-| Get parens from a type. -}
type TypeParen a = Int -> (a, a)

{-| Make 'ParenType' and 'TypeParen' functions
    from a type-open-close table.

    Make paren/type functions from @()@ and @[]@.

    >>> let (pt, tp) = parenTable [(1,'(',')'), (2,'[',']')]

    Get paren types for each chars.
    Types of open parens are positive integer,
    and closes are negative.

    >>> map pt "ab(cd[ef])g"
    [0,0,1,0,0,2,0,0,-2,-1,0]

    Get an open-close pair of parens from its type.

    >>> tp 2
    ('[', ']')
 -}

parenTable
    :: (Eq a)
    => [(Int, a -> Bool, a -> Bool)]  -- ^ List of (/type/, /opne/, /close/)
    -> ParenType a
parenTable xs = parenType where
    parenTypeTable = map parenOpen xs ++ map parenClose xs
    parenOpen  (n, open, _)  = (open,   n)
    parenClose (n, _, close) = (close, -n)
    parenType a =
        case lookupSatisfy a parenTypeTable of
          Just n  -> n
          Nothing -> 0
