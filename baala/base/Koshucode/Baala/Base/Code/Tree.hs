{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
    ppRawTreeWith, ppRawTree, ppRawTrees,
    printTree, printTrees,

    -- * Code tree
    CodeTree, CodeTree',
    codeTree, codeTrees,
    codeTreeWrap,
    codeTreeFmap,
    codeTreeFmap',
    untree, untrees,
  ) where

import qualified Data.List                             as Ls
import qualified Koshucode.Baala.Overture              as O
import qualified Koshucode.Baala.Base.Abort            as B
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
ppRawTreeWith :: (b -> String) -> (y -> String) -> (z -> String) -> RawTree b y z -> [String]
ppRawTreeWith showB showY showZ = pp 0 where
    pp dp (TreeL z)      = [indent dp ++ "- " ++ showZ z]
    pp dp (TreeB b y xs) = (indent dp ++ "> " ++ showB b ++ " " ++ showY y)
                           : (pp (dp + 1) O.<++> xs)
    indent 0  = ""
    indent dp = replicate (2 * dp) ' '

-- | Convert single raw tree to printable lines.
--   Branches are marked usign right arrow (@>@),
--   and leaves are marked using hyphen (@-@).
ppRawTree :: (Show b, Show y, Show z) => RawTree b y z -> [String]
ppRawTree = ppRawTreeWith show show show

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

-- | Tree of bracket type (b), token type (k), and textual value (t).
type CodeTree b k t = CodeTree' b (k t)

-- | Tree with open/close brackets.
type CodeTree' b z = RawTree b (Maybe (z, z)) z

instance (B.GetCodePos (k t)) => B.GetCodePos (CodeTree b k t) where
    getCPs t = B.getCP <$> untree t

-- | Mapping function for textual code tree.
codeTreeFmap :: (Functor k) => (t -> u) -> CodeTree b k t -> CodeTree b k u
codeTreeFmap f = codeTreeFmap' (f <$>)

-- | Mapping function for code tree.
codeTreeFmap' :: (x -> y) -> CodeTree' b x -> CodeTree' b y
codeTreeFmap' f = loop where
    loop (TreeL x)                     = TreeL $ f x
    loop (TreeB b Nothing xs)          = TreeB b Nothing $ map loop xs
    loop (TreeB b (Just (x1, x2)) xs)  = TreeB b (Just (f x1, f x2)) $ map loop xs

-- | Convert code elements to a single code tree.
codeTree :: (Ord b, B.GetCodePos (k t)) => B.GetBracket b (k t) -> B.Bracket b -> b -> [k t] -> B.Ab (CodeTree b k t)
codeTree bracketType zero one =
    Right . codeTreeWrap one O.#. codeTrees bracketType zero

-- | Convert code elements to code trees.
codeTrees :: (Ord b, B.GetCodePos (k t))
    => B.GetBracket b (k t)     -- ^ Bracket definition
    -> B.Bracket b              -- ^ 'B.BracketNone'
    -> [k t]                    -- ^ List of code elements
    -> B.Ab [CodeTree b k t]    -- ^ Result code trees
codeTrees bracketType zero xs = result where
    result       = do (ts, _) <- loop xs zero
                      Right ts
    add xs2 p a  = do (ts, xs3) <- loop xs2 p
                      Right (a : ts, xs3)

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
codeTreeWrap :: b -> [CodeTree b k t] -> CodeTree b k t
codeTreeWrap _   [x] = x
codeTreeWrap one xs  = TreeB one Nothing xs

-- | Convert tree to list of tokens.
untrees :: [CodeTree b k t] -> [k t]
untrees = concatMap untree

-- | Convert tree to list of tokens.
untree :: CodeTree b k t -> [k t]
untree = loop where
    loop (TreeL x) = [x]
    loop (TreeB _ Nothing xs) = loop O.<++> xs
    loop (TreeB _ (Just (open, close)) xs) =
        open : (loop O.<++> xs) ++ [close]

