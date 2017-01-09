{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

-- | Tree structure.

module Koshucode.Baala.Base.Code.Tree
  ( -- * Raw tree
    RawTree (..),
    treeHeight,
    treeListY, treeListZ, treePaths,
    treeMap, treeMapY, treeMapZ,
    undouble,
    partitionLB,

    -- * Print tree
    ppRawTree, ppRawTrees,
    printTree, printTrees,

    -- * Code tree
    CodeTree,
    codeTree, codeTrees,
    codeTreeWrap,
    codeTreeFmap,
    untree, untrees,
  ) where

import qualified Data.List                             as Ls
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
    fmap = treeMapZ

-- | Height of tree.
--
--   >>> treeHeight $ TreeB () "Y1" [TreeL "Z1", TreeL "Z2", TreeB () "Y2" [TreeL "Z3"]]
--   3
--
treeHeight :: RawTree b y z -> Int
treeHeight (TreeL _)      = 1
treeHeight (TreeB _ _ xs) = 1 + maximum (treeHeight <$> xs)

-- | Collect branch elements.
--
--   >>> treeListY $ TreeB () "Y1" [TreeL "Z1", TreeL "Z2", TreeB () "Y2" [TreeL "Z3"]]
--   ["Y1","Y2"]
--
treeListY :: RawTree b y z -> [y]
treeListY (TreeB _ y xs)  = y : (treeListY O.<++> xs)
treeListY (TreeL _)       = []

-- | Collect leaf elements.
--
--   >>> treeListZ $ TreeB () "Y1" [TreeL "Z1", TreeL "Z2", TreeB () "Y2" [TreeL "Z3"]]
--   ["Z1","Z2","Z3"]
--
treeListZ :: RawTree b y z -> [z]
treeListZ (TreeB _ _ xs)  = treeListZ O.<++> xs
treeListZ (TreeL z)       = [z]

-- | Convert tree to path list.
--
--   >>> treePaths $ TreeB () "Y1" [TreeL "Z1", TreeL "Z2", TreeB () "Y2" [TreeL "Z3"]]
--   [(["Y1"],"Z1"), (["Y1"],"Z2"), (["Y1","Y2"],"Z3")]
--
treePaths :: RawTree b y z -> [([y], z)]
treePaths = path [] where
    path p (TreeB _ y xs)  = path (y : p) O.<++> xs
    path p (TreeL z)       = [(reverse p, z)]

-- | Map function to tree elements.
treeMap :: (b1 -> b2) -> (y1 -> y2) -> (z1 -> z2) -> RawTree b1 y1 z1 -> RawTree b2 y2 z2
treeMap bmap ymap zmap = loop where
    loop (TreeL z)       = TreeL (zmap z)
    loop (TreeB b y xs)  = TreeB (bmap b) (ymap y) (loop <$> xs)

-- | Map function to branch element.
treeMapY :: (y1 -> y2) -> RawTree b y1 z -> RawTree b y2 z
treeMapY f = loop where
    loop (TreeL z)       = TreeL z
    loop (TreeB b y xs)  = TreeB b (f y) (loop <$> xs)

-- | Map function to leaf element.
treeMapZ :: (z1 -> z2) -> RawTree b y z1 -> RawTree b y z2
treeMapZ f = loop where
    loop (TreeL z)       = TreeL (f z)
    loop (TreeB b y xs)  = TreeB b y (loop <$> xs)

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

-- | Divide branch into leaves and branches.
partitionLB :: [RawTree b y z] -> ([RawTree b y z], [RawTree b y z])
partitionLB = Ls.partition isTreeL

isTreeL :: RawTree b y z -> Bool
isTreeL (TreeL _)      = True
isTreeL (TreeB _ _ _)  = False

-- ============================================  Print tree

-- | Convert single raw tree to printable lines.
--   Branches are marked usign right arrow (@>@),
--   and leaves are marked using hyphen (@-@).
--
ppRawTree :: (Show b, Show y, Show z) => RawTree b y z -> [String]
ppRawTree = pp 0 where
    pp dp (TreeL z)      = [indent dp ++ "- " ++ show z]
    pp dp (TreeB b y xs) = (indent dp ++ "> " ++ show b ++ " " ++ show y)
                           : (pp (dp + 1) O.<++> xs)
    indent 0  = ""
    indent dp = replicate (2 * dp) ' '

-- | Convert multiple raw trees to printable lines.
ppRawTrees :: (Show b, Show y, Show z) => [RawTree b y z] -> [String]
ppRawTrees = concatMap ppRawTree

-- | Pretty print raw tree.
--
--   >>> printTree $ TreeB () "Y1" [TreeL "Z1", TreeB () "Y2" [TreeL "Z2", TreeL "Z3"]]
--   Height of tree      = 3
--   Number of branches  = 2
--   Number of leaves    = 3
--   .
--   > () "Y1"
--     - "Z1"
--     > () "Y2"
--       - "Z2"
--       - "Z3"
--
printTree :: (Show b, Show y, Show z) => RawTree b y z -> IO ()
printTree tree =
    do putStrLn $ "Height of tree      = " ++ show (treeHeight tree)
       putStrLn $ "Number of branches  = " ++ num treeListY
       putStrLn $ "Number of leaves    = " ++ num treeListZ
       O.putLn
       putStrLn O.<#!> ppRawTree tree
       O.putLn
    where
      num f = show (length $ f tree)

-- | Pretty print raw trees.
printTrees :: (Show b, Show y, Show z) => [RawTree b y z] -> IO ()
printTrees = mapM_ printTree


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
codeTree :: (Ord b, B.GetCodePos a) => B.GetBracket b a -> B.Bracket b -> b -> [a] -> B.Ab (CodeTree b a)
codeTree bracketType zero one =
    Right . codeTreeWrap one B.<#.> codeTrees bracketType zero

-- | Convert code elements to code trees.
codeTrees :: forall a. forall b. (Ord b, B.GetCodePos a)
    => B.GetBracket b a         -- ^ Bracket definition
    -> B.Bracket b              -- ^ 'B.BracketNone'
    -> [a]                      -- ^ List of code elements
    -> B.Ab [CodeTree b a]      -- ^ Result code trees
codeTrees bracketType zero xs = result where
    result       = do (ts, _) <- loop xs zero
                      Right ts
    add xs2 p a  = do (ts, xs3) <- loop xs2 p
                      Right (a : ts, xs3)

    loop :: [a] -> B.Bracket b -> B.Ab ([CodeTree b a], [a])
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
codeTreeWrap :: b -> [CodeTree b a] -> CodeTree b a
codeTreeWrap _   [x] = x
codeTreeWrap one xs  = TreeB one Nothing xs

-- | Convert tree to list of tokens.
untrees :: [CodeTree b a] -> [a]
untrees = concatMap untree

-- | Convert tree to list of tokens.
untree :: CodeTree b a -> [a]
untree = loop where
    loop (TreeL x) = [x]
    loop (TreeB _ Nothing xs) = loop O.<++> xs
    loop (TreeB _ (Just (open, close)) xs) =
        open : (loop O.<++> xs) ++ [close]

