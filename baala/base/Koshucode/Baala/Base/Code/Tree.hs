{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

-- | Tree structure.

module Koshucode.Baala.Base.Code.Tree
  ( -- * Tree
    RawTree (..),
    treeMapL, treeMapB,
    treeLeaves,
    undouble,
    ppTree,

    -- * Code tree
    CodeTree,
    codeTree, codeTrees,
    codeTreeWrap,
    codeTreeFmap,
    untree, untrees,
  ) where

import qualified Koshucode.Baala.Overture              as O
import qualified Koshucode.Baala.Base.Abort            as B
import qualified Koshucode.Baala.Base.Prelude          as B
import qualified Koshucode.Baala.Base.Code.Bracket     as B
import qualified Koshucode.Baala.Base.Code.Message     as Msg


-- ============================================  Tree

-- | Tree of leaf and branch.
data RawTree b y z
    = TreeL z
      -- ^ __Leaf:__ Leaf element (@z@).
    | TreeB b y [RawTree b y z]
      -- ^ __Branch:__ Branch type (@b@), branch element (@y@), and subtrees.
      deriving (Show, Eq, Ord)

instance Functor (RawTree p k) where
    fmap = treeMapL

-- | Map function to all leaves.
treeMapL :: (z1 -> z2) -> RawTree b y z1 -> RawTree b y z2
treeMapL f = loop where
    loop (TreeL x)        = TreeL $ f x
    loop (TreeB p k xs)  = TreeB p k $ map loop xs

-- | Map function to all branches.
treeMapB :: (b -> y -> [RawTree b y z] -> RawTree b y z) -> O.Map (RawTree b y z)
treeMapB f = loop where
    loop (TreeL x)        = TreeL x
    loop (TreeB p k xs)  = f p k $ map loop xs

-- | Collect leaves in tree.
treeLeaves :: RawTree b y z -> [z]
treeLeaves (TreeB _ _ ts)  = treeLeaves O.<++> ts
treeLeaves (TreeL x)       = [x]

-- | Simplify tree by removing double brackets,
--   like @((a))@ to @(a)@.
--
--   >>> undouble (== 0) $ TreeB 0 Nothing [TreeB 0 Nothing [TreeL "A", TreeL "B"]]
--   TreeB 0 Nothing [TreeL "A", TreeL "B"]
-- 
undouble :: O.Test b -> O.Map (RawTree b y z)
undouble p = loop where
    loop (TreeB n pp xs) | p n =
        case map loop xs of
          [x] -> x
          xs2 -> TreeB n pp xs2
    loop x = x

-- | Pretty print of raw tree.
--   Branches are marked usign right arrow (@>@),
--   and leaves are marked using hyphen (@-@).
--
--   >>> mapM_ putStrLn $ ppTree $ TreeB () "Y1" [TreeL "Z1", TreeB () "Y2" [TreeL "Z2", TreeL "Z3"]]
--   > () "Y1"
--     - "Z1"
--     > () "Y2"
--       - "Z2"
--       - "Z3"
--
ppTree :: (Show b, Show y, Show z) => RawTree b y z -> [String]
ppTree = pp 0 where
    pp dp (TreeL z)      = [indent dp ++ "- " ++ show z]
    pp dp (TreeB b y zs) = (indent dp ++ "> " ++ show b ++ " " ++ show y)
                           : (pp (dp + 1) O.<++> zs)
    indent 0  = ""
    indent dp = replicate (2 * dp) ' '


-- ============================================  Code tree

-- | Tree with open/close brackets.
type CodeTree b a = RawTree b (Maybe (a, a)) a

instance (B.GetCodePos a) => B.GetCodePos (CodeTree p a) where
    getCPs t = B.getCP <$> untree t

-- | Mapping function for 'CodeTree'.
codeTreeFmap :: (a -> b) -> CodeTree p a -> CodeTree p b
codeTreeFmap f = loop where
    loop (TreeL x)                   = TreeL $ f x
    loop (TreeB n Nothing xs)        = TreeB n Nothing $ map loop xs
    loop (TreeB n (Just (x, y)) xs)  = TreeB n (Just (f x, f y)) $ map loop xs

-- | Convert code elements to a single code tree.
codeTree :: (Ord p, B.GetCodePos a) => B.GetBracket p a -> B.Bracket p -> p -> [a] -> B.Ab (CodeTree p a)
codeTree bracketType zero one =
    Right . codeTreeWrap one B.<.> codeTrees bracketType zero

-- | Convert code elements to code trees.
codeTrees :: forall a. forall p. (Ord p, B.GetCodePos a)
    => B.GetBracket p a         -- ^ Bracket definition
    -> B.Bracket p              -- ^ 'B.BracketNone'
    -> [a]                      -- ^ List of code elements
    -> B.Ab [CodeTree p a]      -- ^ Result code trees
codeTrees bracketType zero xs = result where
    result       = do (ts, _) <- loop xs zero
                      Right ts
    add xs2 p a  = do (ts, xs3) <- loop xs2 p
                      Right (a : ts, xs3)

    loop :: [a] -> B.Bracket p -> B.Ab ([CodeTree p a], [a])
    loop [] _ = Right ([], [])
    loop (x : xs2) p
        | isNone px  = add xs2 p $ TreeL x
        | isOpen px  = do (trees2, cxs3) <- loop xs2 px
                          case (px, cxs3) of
                            (B.BracketOpen p2, c : xs3)
                              -> add xs3 p $ TreeB p2 (Just (x, c)) trees2
                            _ -> ab Msg.extraOpenBracket
        | isClose px  = Right ([], x : xs2)
        | otherwise          = ab Msg.extraCloseBracket
        where px  = bracketType x
              ab  = Msg.abCode [x]

    isNone (B.BracketNone)     = True
    isNone _                   = False

    isOpen (B.BracketOpen _)   = True
    isOpen _                   = False

    isClose (B.BracketClose _) = True
    isClose _                  = False

-- | Wrap trees into single tree.
codeTreeWrap :: p -> [CodeTree p a] -> CodeTree p a
codeTreeWrap _   [x] = x
codeTreeWrap one xs  = TreeB one Nothing xs

-- | Convert tree to list of tokens.
untrees :: [CodeTree p a] -> [a]
untrees = concatMap untree

-- | Convert tree to list of tokens.
untree :: CodeTree p a -> [a]
untree = loop where
    loop (TreeL x) = [x]
    loop (TreeB _ Nothing xs) = loop O.<++> xs
    loop (TreeB _ (Just (open, close)) xs) =
        open : (loop O.<++> xs) ++ [close]

