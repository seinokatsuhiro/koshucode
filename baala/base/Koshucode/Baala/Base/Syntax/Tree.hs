{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Syntax.Tree
  ( 
    -- * Data type
    Bracket (..),
    CodeTree (..),
  
    -- * Parsing
    tree, trees,
    treeWrap,
    untree, untrees,
    leaves, undouble,
    mapToLeaf,
  
    -- * Bracket table
    GetBracketType,
    bracketTable
  ) where

import qualified Data.Generics                as G
import qualified Koshucode.Baala.Base.Abort   as B
import qualified Koshucode.Baala.Base.Prelude as B
import qualified Koshucode.Baala.Base.Text    as B
import qualified Koshucode.Baala.Base.Message as Msg



-- ----------------------  Bracket

-- | Bracket type.
data Bracket p
    = BracketNone       -- ^ None bracket
    | BracketOpen  p    -- ^ Open bracket
    | BracketClose p    -- ^ Close bracket
      deriving (Show, Eq, Ord, G.Data, G.Typeable)

isNotBracket :: B.Pred (Bracket p)
isNotBracket (BracketNone)      = True
isNotBracket _                  = False

isOpenBracket :: B.Pred (Bracket p)
isOpenBracket (BracketOpen _)   = True
isOpenBracket _                 = False

isCloseBracket :: B.Pred (Bracket p)
isCloseBracket (BracketClose _) = True
isCloseBracket _                = False


-- ----------------------  Tree

-- | Tree of leaf and branch.
data CodeTree p a
    = TreeL a                                -- ^ Terminal of tree.
    | TreeB p (Maybe (a, a)) [CodeTree p a]  -- ^ Bracket-type and subtrees.
      deriving (Show, Eq, Ord, G.Data, G.Typeable)

instance Functor (CodeTree p) where
    fmap f (TreeL x)       = TreeL (f x)
    fmap f (TreeB n Nothing xs) = TreeB n Nothing $ map (fmap f) xs
    fmap f (TreeB n (Just (x, y)) xs) = TreeB n (Just (f x, f y)) $ map (fmap f) xs

-- | Convert a list of elements to a single tree.
tree :: (Ord p, B.CodePtr a) => GetBracketType p a -> Bracket p -> p -> [a] -> B.Ab (CodeTree p a)
tree bracketType zero one =
    Right . treeWrap one B.<=< trees bracketType zero

-- |  Convert a list of elements to trees.
trees :: forall a. forall p. (Ord p, B.CodePtr a)
      => GetBracketType p a -> Bracket p -> [a] -> B.Ab [CodeTree p a]
trees bracketType zero xs = result where
    result       = do (ts, _) <- loop xs zero
                      Right ts
    add xs2 p a  = do (ts, xs3) <- loop xs2 p
                      Right (a : ts, xs3)

    loop :: [a] -> Bracket p -> B.Ab ([CodeTree p a], [a])
    loop [] _ = Right ([], [])
    loop (x : xs2) p
        | isNotBracket   px  = add xs2 p $ TreeL x
        | isOpenBracket  px  =
            do (trees2, cxs3) <- loop xs2 px
               case (px, cxs3) of
                 (BracketOpen p2, c : xs3)
                            -> add xs3 p $ TreeB p2 (Just (x, c)) trees2
                 _          -> ab Msg.extraOpenBracket
        | isCloseBracket px  = Right ([], x : xs2)
        | otherwise          = ab Msg.extraCloseBracket
        where px  = bracketType x
              ab  = Msg.abTree [x]


-- ----------------------  Utility

treeWrap :: p -> [CodeTree p a] -> CodeTree p a
treeWrap _   [x] = x
treeWrap one xs  = TreeB one Nothing xs

-- | Convert tree to list of tokens.
untrees :: [CodeTree p a] -> [a]
untrees = concatMap untree

-- | Convert tree to list of tokens.
untree :: CodeTree p a -> [a]
untree = loop where
    loop (TreeL x) = [x]
    loop (TreeB _ Nothing xs) =
        concatMap loop xs
    loop (TreeB _ (Just (open, close)) xs) =
        open : concatMap loop xs ++ [close]

leaves :: CodeTree p a -> [a]
leaves (TreeB _ _ ts)  = concatMap leaves ts
leaves (TreeL x)       = [x]

-- | Simplify tree by removing double brackets,
--   like @((a))@ to @(a)@.
--
--   >>> undouble (== 0) $ TreeB 0 Nothing [TreeB 0 Nothing [TreeL "A", TreeL "B"]]
--   TreeB 0 Nothing [TreeL "A", TreeL "B"]
-- 
undouble :: B.Pred p -> B.Map (CodeTree p a)
undouble p = loop where
    loop (TreeB n pp xs) | p n =
        case map loop xs of
          [x] -> x
          xs2 -> TreeB n pp xs2
    loop x = x

-- e1 = TreeB 2 [TreeB 1 [TreeB 0 [TreeL 0]]]
-- e2 = TreeB 2 [e1, TreeB 1 [TreeB 0 [TreeL 0]]]
-- e3 = undouble e2

mapToLeaf :: (a -> a) -> B.Map (CodeTree p a)
mapToLeaf f = loop where
    loop (TreeB p aa sub)  = TreeB p aa $ map loop sub
    loop (TreeL a)        = TreeL $ f a


-- ----------------------  Bracket table

-- | Get a bracket type.
type GetBracketType p a = a -> Bracket p

-- | Make 'GetBracketType' functions
--   from a type-open-close table.
--
--   Make bracket/type functions from @()@ and @[]@.
--
--   >>> let bracket n [a, b] = (n, (== a), (== b))
--   >>> let pt = bracketTable [ bracket 1 "()", bracket 2 "[]" ]
--
--   Get bracket types for each chars.
--   Types of open brackets are positive integer,
--   and closes are negative.
--
--   >>> map pt "ab(cd[ef])g"
--   [0, 0, 1, 0, 0, 2, 0, 0, -2, -1, 0]
--
bracketTable
    :: (Eq a)
    => [(p, B.Pred a, B.Pred a)] -- ^ List of (/type/, /open/, /close/)
    -> GetBracketType p a
bracketTable xs = bracketType where
    bracketTypeTable = map bracketOpen xs ++ map bracketClose xs
    bracketOpen  (n, isOpen, _)  = (isOpen,  BracketOpen n)
    bracketClose (n, _, isClose) = (isClose, BracketClose n)
    bracketType a =
        case B.lookupSatisfy a bracketTypeTable of
          Just p  -> p
          Nothing -> BracketNone

