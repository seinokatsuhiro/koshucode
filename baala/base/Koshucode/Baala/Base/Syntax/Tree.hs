{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Syntax.Tree
( 
  -- * Data type
  CodeTree (..),

  -- * Parsing
  tree, trees,
  treeWrap,
  untree, untrees,
  undouble,

  -- * Paren table
  ParenType,
  GetParenType,
  parenTable
) where

import qualified Data.Generics                as G
import qualified Koshucode.Baala.Base.Abort   as B
import qualified Koshucode.Baala.Base.Prelude as B
import qualified Koshucode.Baala.Base.Text    as B
import qualified Koshucode.Baala.Base.Message as Message



-- ----------------------  Tree

data Paren a
    = ParenNon a
    | ParenOpen a
    | ParenClose a
      deriving (Show, Eq, Ord, G.Data, G.Typeable)

-- | Tree of leaf and branch.
data CodeTree a
    = TreeL a       -- ^ Leaf. Terminal of tree.
    | TreeB ParenType (Maybe (a, a)) [CodeTree a] -- ^ Branch. Paren-type and subtrees.
      deriving (Show, Eq, Ord, G.Data, G.Typeable)

instance Functor CodeTree where
    fmap f (TreeL x)       = TreeL (f x)
    fmap f (TreeB n Nothing xs) = TreeB n Nothing $ map (fmap f) xs
    fmap f (TreeB n (Just (x, y)) xs) = TreeB n (Just (f x, f y)) $ map (fmap f) xs

-- treeG :: [CodeTree a] -> CodeTree a
-- treeG [TreeB 1 _ xs] = treeG xs
-- treeG [x] = x
-- treeG xs = TreeB 1 Nothing xs

-- | Convert a list of elements to a single tree.
tree :: (B.CodePointer a) => GetParenType a -> [a] -> B.Ab (CodeTree a)
tree p = Right . treeWrap B.<=< trees p

-- |  Convert a list of elements to trees.
trees :: forall a. (B.CodePointer a) => GetParenType a -> [a] -> B.Ab [CodeTree a]
trees parenType xs = result where
    result       = do (ts, _) <- loop xs 0
                      Right ts
    add a xs2 p  = do (ts, xs3) <- loop xs2 p
                      Right (a : ts, xs3)

    loop :: [a] -> Int -> B.Ab ([CodeTree a], [a])
    loop [] _        = Right ([], [])
    loop (x : xs2) p
        | ground     = add (TreeL x) xs2 p
        | open       = do (trees2, cxs3) <- loop xs2 px
                          case cxs3 of
                            c : xs3 -> add (TreeB px (Just (x, c)) trees2) xs3 p
                            _       -> abort Message.extraOpenParen
        | close      = Right ([], x : xs2)
        | otherwise  = abort Message.extraCloseParen
        where 
          open   = ( px >  0 )
          ground = ( px == 0 )
          close  = ( px == - p )
          px     = parenType x
          abort  = B.abortable "tree" [x]



-- ----------------------  Utility

treeWrap :: [CodeTree a] -> CodeTree a
treeWrap [x] = x
treeWrap xs  = TreeB 1 Nothing xs

-- | Convert tree to list of tokens.
untrees :: [CodeTree a] -> [a]
untrees = concatMap untree

-- | Convert tree to list of tokens.
untree :: CodeTree a -> [a]
untree = loop where
    loop (TreeL x) = [x]
    loop (TreeB _ Nothing xs) =
        concatMap loop xs
    loop (TreeB _ (Just (open, close)) xs) =
        open : concatMap loop xs ++ [close]

-- | Simplify tree by removing double parens,
--   like @((a))@ to @(a)@.
--
--   >>> undouble (== 0) $ TreeB 0 Nothing [TreeB 0 Nothing [TreeL "A", TreeL "B"]]
--   TreeB 0 Nothing [TreeL "A", TreeL "B"]
-- 
undouble :: B.Pred ParenType -> B.Map (CodeTree a)
undouble p = loop where
    loop (TreeB n pp xs) | p n =
        case map loop xs of
          [x] -> x
          xs2 -> TreeB n pp xs2
    loop x = x

-- e1 = TreeB 2 [TreeB 1 [TreeB 0 [TreeL 0]]]
-- e2 = TreeB 2 [e1, TreeB 1 [TreeB 0 [TreeL 0]]]
-- e3 = undouble e2



-- ----------------------  Paren table

type ParenType = Int

-- | Get a paren type.
type GetParenType a = a -> ParenType

-- | Make 'GetParenType' functions
--   from a type-open-close table.
--
--   Make paren/type functions from @()@ and @[]@.
--
--   >>> let paren n [a, b] = (n, (== a), (== b))
--   >>> let pt = parenTable [ paren 1 "()", paren 2 "[]" ]
--
--   Get paren types for each chars.
--   Types of open parens are positive integer,
--   and closes are negative.
--
--   >>> map pt "ab(cd[ef])g"
--   [0, 0, 1, 0, 0, 2, 0, 0, -2, -1, 0]
--
parenTable
    :: (Eq a)
    => [(ParenType, B.Pred a, B.Pred a)] -- ^ List of (/type/, /open/, /close/)
    -> GetParenType a
parenTable xs = parenType where
    parenTypeTable = map parenOpen xs ++ map parenClose xs
    parenOpen  (n, isOpen, _)  = (isOpen,   n)
    parenClose (n, _, isClose) = (isClose, -n)
    parenType a =
        case B.lookupSatisfy a parenTypeTable of
          Just n  -> n
          Nothing -> 0

