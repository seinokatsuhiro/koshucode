{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Tree structure.

module Koshucode.Baala.Base.Code.Tree
  ( -- * Tree
    CodeTree (..),
    tree, trees,

    -- * Transformation
    untree, untrees,
    treeLeaves,
    undouble,
    treeWrap,
    treeLeafMap, treeBranchMap,
  
    -- * Bracket
    Bracket (..),
    GetBracketType,
    bracketTable
  ) where

import qualified Koshucode.Baala.Overture              as O
import qualified Koshucode.Baala.Base.Abort            as B
import qualified Koshucode.Baala.Base.IO               as B
import qualified Koshucode.Baala.Base.List             as B
import qualified Koshucode.Baala.Base.Prelude          as B
import qualified Koshucode.Baala.Base.Code.Message     as Msg


-- ----------------------  Tree

-- | Tree of leaf and branch.
data CodeTree p a
    = TreeL a
      -- ^ __Leaf:__ Terminal of tree.
    | TreeB p (Maybe (a, a)) [CodeTree p a]
      -- ^ __Branch:__ Bracket-type, open/close leaves, and subtrees.
      deriving (Show, Eq, Ord)

instance Functor (CodeTree p) where
    fmap = mapToAllLeaf

instance (B.GetCodePos a) => B.GetCodePos (CodeTree p a) where
    getCPs t = B.getCP <$> untree t

-- | Convert a list of elements to a single tree.
tree :: (Ord p, B.GetCodePos a) => GetBracketType p a -> Bracket p -> p -> [a] -> B.Ab (CodeTree p a)
tree bracketType zero one =
    Right . treeWrap one B.<.> trees bracketType zero

-- | Convert a list of elements to trees.
trees :: forall a. forall p. (Ord p, B.GetCodePos a)
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
              ab  = Msg.abCode [x]


-- ----------------------  Utility

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

-- | Collect leaves in tree.
treeLeaves :: CodeTree p a -> [a]
treeLeaves (TreeB _ _ ts)  = concatMap treeLeaves ts
treeLeaves (TreeL x)       = [x]

-- | Simplify tree by removing double brackets,
--   like @((a))@ to @(a)@.
--
--   >>> undouble (== 0) $ TreeB 0 Nothing [TreeB 0 Nothing [TreeL "A", TreeL "B"]]
--   TreeB 0 Nothing [TreeL "A", TreeL "B"]
-- 
undouble :: O.Test p -> O.Map (CodeTree p a)
undouble p = loop where
    loop (TreeB n pp xs) | p n =
        case map loop xs of
          [x] -> x
          xs2 -> TreeB n pp xs2
    loop x = x

-- | Wrap trees into single tree.
treeWrap :: p -> [CodeTree p a] -> CodeTree p a
treeWrap _   [x] = x
treeWrap one xs  = TreeB one Nothing xs

mapToAllLeaf :: (a -> b) -> CodeTree p a -> CodeTree p b
mapToAllLeaf f = loop where
    loop (TreeL x)                   = TreeL $ f x
    loop (TreeB n (Nothing)     xs)  = TreeB n (Nothing)         $ map loop xs
    loop (TreeB n (Just (x, y)) xs)  = TreeB n (Just (f x, f y)) $ map loop xs

-- | Map function to all leaves.
--   This function is similar to 'fmap',
--   but not map to bracket leaf.
treeLeafMap :: (a -> a) -> O.Map (CodeTree p a)
treeLeafMap f = loop where
    loop (TreeL x)        = TreeL $ f x
    loop (TreeB p aa xs)  = TreeB p aa $ map loop xs

-- | Map function to all branches.
treeBranchMap :: (p -> Maybe (a, a) -> [CodeTree p a] -> CodeTree p a) -> O.Map (CodeTree p a)
treeBranchMap f = loop where
    loop (TreeL x)        = TreeL x
    loop (TreeB p aa xs)  = f p aa $ map loop xs


-- ----------------------  Bracket

-- | Bracket type.
data Bracket p
    = BracketNone       -- ^ None bracket
    | BracketOpen  p    -- ^ Open bracket
    | BracketClose p    -- ^ Close bracket
      deriving (Show, Eq, Ord)

isNotBracket :: O.Test (Bracket p)
isNotBracket (BracketNone)       = True
isNotBracket _                   = False

isOpenBracket :: O.Test (Bracket p)
isOpenBracket (BracketOpen _)    = True
isOpenBracket _                  = False

isCloseBracket :: O.Test (Bracket p)
isCloseBracket (BracketClose _)  = True
isCloseBracket _                 = False


-- ----------------------  Bracket table

-- | Get a bracket type.
type GetBracketType p a = a -> Bracket p

-- | Create 'GetBracketType' functions
-- from a type-open-close table.
--
-- /Example/
--
-- Create bracket/type functions from @()@ and @[]@.
--
--   >>> let bracket n [a, b] = (n, ((== a), (== b)))
--   >>> let pt = bracketTable [ bracket 1 "()", bracket 2 "[]" ]
--
-- Get bracket types for each chars.
-- Types of open brackets are positive integer,
-- and closes are negative.
--
--   >>> map pt "ab(cd[ef])g"
--   [BracketNone, BracketNone,
--    BracketOpen 1, BracketNone, BracketNone,
--     BracketOpen 2, BracketNone, BracketNone, BracketClose 2,
--    BracketClose 1, BracketNone]
--
bracketTable
    :: (Eq a)
    => [(p, (O.Test a, O.Test a))] -- ^ List of (/type/, (/open/, /close/))
    -> GetBracketType p a
bracketTable xs = bracketType where
    bracketTypeTable = map bracketOpen xs ++ map bracketClose xs
    bracketOpen  (n, (isOpen, _))  = (isOpen,  BracketOpen n)
    bracketClose (n, (_, isClose)) = (isClose, BracketClose n)
    bracketType a =
        case B.lookupSatisfy a bracketTypeTable of
          Just p  -> p
          Nothing -> BracketNone
