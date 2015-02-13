{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Convert infixed-operator trees into prefixed-operator trees.

module Koshucode.Baala.Base.Syntax.Infix
  ( InfixHeight,
    infixHeight,
    infixToPrefix,
    InfixAb, InfixAbMap, InfixTree, InfixMapper,
  ) where

import qualified Data.Map                         as Map
import qualified Koshucode.Baala.Base.Prelude     as B
import qualified Koshucode.Baala.Base.Syntax.Tree as B


-- ----------------------  Height function

-- | Direction and height for binary splitting.
--
--   [@Left@ /H/] Splits first the left-most operator,
--   and operator height is /H/.
--   For example, if the operator @:@ is of @Left 5@,
--   an expression @(a : b : c)@ is splitted into @(: a (b : c))@
--   and then @(: a (: b c))@.
--
--   [@Right@ /H/] Right-most splitting and height /H/.
--   For example, if the operator @.@ is of @Right 3@,
--   an expression @(a . b . c)@ is splitted into @(. (a . b) c)@
--   and then @(. (. a b) c)@.
--   An expression @(a . b : c)@ is into @(: (a . b) c)@,
--   and then @(: (. a b) c)@.
--   Symbols that is not a binary operator like @b@ are of height 0.
--   For that reason, heights of expression @(a . b : c)@
--   are @(0 3 0 5 0)@
--
type InfixHeight = Either Int Int

heightOf :: InfixHeight -> Int
heightOf (Left  ht)  =  ht
heightOf (Right ht)  =  ht

-- | Make the height function from a height table of operators.
infixHeight :: (Ord b) => (a -> Maybe b) -> [(b, InfixHeight)] -> a -> InfixHeight
infixHeight extract htab a = B.fromMaybe (Left 0) ht where
    hmap = Map.fromList htab
    ht   = case extract a of
             Nothing  -> Nothing
             Just key -> Map.lookup key hmap


-- ----------------------  Conversion

type InfixAb a x      =  Either [(InfixHeight, a)] x
type InfixAbMap a x   =  x -> InfixAb a x
type InfixTree p a    =  B.CodeTree p (InfixHeight, a)  -- tree with height
type InfixMapper p a  =  InfixAbMap a [InfixTree p a] -> InfixAbMap a (InfixTree p a)

-- | Split branches in a given tree at infixed binary operators.
infixToPrefix :: forall p a.
    (B.Map a, B.Map a, B.Map a)
     -> (a -> InfixHeight)
     -> B.Collect (InfixTree p a)
     -> InfixMapper p a
     -> InfixAbMap a (B.CodeTree p a)
infixToPrefix (pre, inf, post) ht g mapper tree =
    do let tree1 = fmap height tree
       tree2 <- mapper binary tree1
       Right $ fmap snd tree2
    where
      height :: a -> (InfixHeight, a)
      height x = (ht x, x)

      binary :: InfixAbMap a [InfixTree p a]
      binary xs = case infixPos $ map treeHeight xs of
          Right xi
              | xi < 0        ->  mapper binary `mapM` xs
              | otherwise     ->  move $ splitAt xi xs
          Left xi             ->  Left $ B.untrees $ map (xs !!) xi

      move :: ([InfixTree p a], [InfixTree p a]) -> InfixAb a [InfixTree p a]
      move ([], op : right)    =  do right' <- binary right
                                     Right [conv pre op, g right']
      move (left, op : [])     =  do left'  <- binary left
                                     Right [conv post op, g left']
      move (left, op : right)  =  do left'  <- binary left
                                     right' <- binary right
                                     Right [conv inf op, g left', g right']
      move (_, _)              =  error "infixToPrefix"

conv :: B.Map a -> B.Map (InfixTree p a)
conv = fmap . B.mapSnd

treeHeight :: InfixTree p a -> InfixHeight
treeHeight (B.TreeL (ht, _))  =  ht
treeHeight (B.TreeB _ _ _)    =  Left 0

infixPos :: [InfixHeight] -> Either [B.Index] B.Index
infixPos = fmap fst . pos (Right (-1, 0)) (Left 0) . zip [0 ..] where
    pos res _ [] = res

    -- different height
    -- /res result  /y highest-element  /xi current-index  /xs elements
    pos res y ((xi, x) : xs)
        | hy > hx = pos res              y xs  -- remains y
        | hy < hx = pos (Right (xi, hx)) x xs  -- replaces y to x
        where  hx = heightOf x
               hy = heightOf y

    -- same height, same direction
    pos _     (Right _) ((xi, x@(Right _)) : xs) = pos (Right (xi, 0)) x xs -- replaces y to x
    pos res y@(Left  _) ((_ ,   (Left  _)) : xs) = pos res y xs             -- remains y

    -- ambiguous: same height, different direction
    pos (Right (yi, _)) y ((xi, _) : xs)  =  pos (Left [xi, yi])  y xs
    pos (Left ps)       y ((xi, _) : xs)  =  pos (Left $ xi : ps) y xs

