{-# OPTIONS_GHC -Wall #-}

-- | Convert infixed-operator trees
--   into prefixed-operator trees

module Koshucode.Baala.Base.Syntax.Binary
( BinaryHeight
, binaryTree
, heightTable
, heightTableUnbox
) where

import qualified Data.Map   as Map
import qualified Data.Maybe as Maybe

import Koshucode.Baala.Base.Syntax.Tree

{-| Direction and height for binary splitting.
 
    [@Left@ /H/] Splits first the left-most operator,
    and operator height is /H/.
    For example, if the operator @:@ is of @Left 5@,
    an expression @(a : b : c)@ is splitted into @(: a (b : c))@
    and then @(: a (: b c))@.
 
    [@Right@ /H/] Right-most splitting and height /H/.
    For example, if the operator @.@ is of @Right 3@,
    an expression @(a . b . c)@ is splitted into @(. (a . b) c)@
    and then @(. (. a b) c)@.
    An expression @(a . b : c)@ is into @(: (a . b) c)@,
    and then @(: (. a b) c)@.
    Symbols that is not a binary operator like @b@ are of height 0.
    For that reason, heights of expression @(a . b : c)@
    are @(0 3 0 5 0)@
 -}
type BinaryHeight = Either Int Int

height :: BinaryHeight -> Int
height (Left  h) = h
height (Right h) = h

-- (0 L1 0 L4 0) -> (L4 (0 L1 0) 0) -> (L4 (L1 0 0) 0)
-- (0 L1 0 L1 0) -> (L1 0 (0 L1 0)) -> (L1 0 (L1 0 0))
-- (0 R1 0 R1 0) -> (R1 (0 R1 0) 0) -> (R1 (R1 0 0) 0)
-- (0 L1 0 R1 0) -> ambiguous

{-| Split branches in a given tree at infixed binary operators -}
binaryTree :: (Show a)
    => (a -> BinaryHeight)  -- ^ Height function
    -> Tree a               -- ^ Infixed tree
    -> Tree a               -- ^ Prefixed tree
binaryTree ht tree1 = undouble (== 1) $ binaryHeightMap loop ht tree1 where
    loop tree2@(TreeL _)    = tree2
    loop tree2@(TreeB n xs) =
        case binaryPos $ heightList tree2 of
          Right p | p <= 0 -> TreeB n $ map loop xs
          Right p -> let (left, r:ight) = splitAt p xs
                     in  TreeB n $
                         if null ight
                         then [r, sub n left]
                         else [r, sub n left, sub n ight]
          Left (p,q) -> error $ "ambiguous operator group: "
                         ++ show (xs !! p) ++ " and "
                         ++ show (xs !! q)

    sub _ [x] = loop x
    sub n xs  = loop $ TreeB n xs

-- e0 = heightTable [(Left 5, "="), (Right 3, "+-"), (Left 3, "*/")]
-- e1 = map TreeL
-- e2 = binaryTree e0 . binaryTree e0
-- e3 = e2 (TreeB 0 $ e1 "1+2=0")
-- e4 = e2 (TreeB 0 $ e1 "1+2*3+4=0")
-- e5 = e2 (TreeB 0 $ e1 "1+2+3")
-- e6 = e2 (TreeB 0 $ e1 "1+2-3")
-- e7 = e2 (TreeB 0 $ e1 "1+2*3")

heightList :: Tree (BinaryHeight, a) -> [BinaryHeight]
heightList (TreeB _ xs) = map f xs where
    f (TreeL (ht, _)) = ht
    f (TreeB _ _)     = Left 0
heightList _ = undefined

binaryPos :: [BinaryHeight] -> Either (Int, Int) Int
binaryPos xs = f xs 0 (Left 0) (-1) where
    f [] _ _ yi = Right yi
    f (x:xs2) xi y yi
        | hx <  hy  = f xs2 (xi+1) y yi
        | hx >  hy  = f xs2 (xi+1) x xi
        where hx = height x
              hy = height y
    f (  (Left  _) : xs2) xi y@(Left  _) yi = f xs2 (xi+1) y yi
    f (x@(Right _) : xs2) xi   (Right _) _ = f xs2 (xi+1) x xi
    f _ xi _ yi = Left (yi, xi) -- ambiguous positions

-- e0 = binaryPos []
-- e1 = binaryPos [Left 0]
-- e2 = binaryPos [Left 0, Left 1, Left 0]
-- e3 = binaryPos [Left 0, Left 1, Left 0, Left 2, Left 0]
-- e4 = binaryPos [Left 0, Right 2, Left 0, Right 1, Left 0]
-- e5 = binaryPos [Left 0, Left 1, Left 0, Left 1, Left 0]
-- e6 = binaryPos [Left 0, Right 1, Left 0, Right 1, Left 0]
-- e7 = binaryPos [Left 0, Left 1, Left 0, Right 1, Left 0]

{-| Make the height function from a height table of operators. -}
heightTable :: (Ord a)
    => [(BinaryHeight, [a])] -- ^ Height table
    -> (a -> BinaryHeight)   -- ^ Height function
heightTable = heightFunction . heightExpand

heightExpand :: [(BinaryHeight, [a])] -> [(a, BinaryHeight)]
heightExpand tab = [(x, ht) | (ht, xs) <- tab, x <- xs]

heightFunction :: (Ord a) => [(a, BinaryHeight)] -> a -> BinaryHeight
heightFunction xs a = ht where
    ht    = Maybe.fromMaybe (Left 0) $ Map.lookup a hmap
    hmap  = Map.fromList xs

heightTableUnbox :: (Ord a)
    => (b -> a)              -- ^ Unbox function
    -> [(BinaryHeight, [a])] -- ^ Height table
    -> (b -> BinaryHeight)   -- ^ Height function
heightTableUnbox unbox = heightFunctionUnbox unbox . heightExpand

heightFunctionUnbox
    :: (Ord a) => (b -> a) -> [(a, BinaryHeight)] -> b -> BinaryHeight
heightFunctionUnbox unbox xs b = heightFunction xs $ unbox b

binaryHeightMap
    :: (Functor g, Functor f)
    => (f (BinaryHeight, a1) -> g (BinaryHeight, a))
    -> (a1 -> BinaryHeight)
    -> f a1
    -> g a
binaryHeightMap f ht tree2 = binaryUnheight $ f $ binaryHeight ht tree2

binaryHeight :: (Functor f) => (a -> BinaryHeight) -> f a -> f (BinaryHeight, a)
binaryHeight ht = fmap g where
    g x = (ht x, x)

binaryUnheight :: (Functor f) => f (BinaryHeight, a) -> f a
binaryUnheight = fmap snd

