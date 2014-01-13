{-# OPTIONS_GHC -Wall #-}

-- | Convert infixed-operator trees
--   into prefixed-operator trees

module Koshucode.Baala.Base.Syntax.Infix
( InfixHeight,
  infixHeight,
  infixToPrefix,
) where

import qualified Data.Map   as Map
import qualified Data.Maybe as Maybe
import qualified Koshucode.Baala.Base.Prelude     as B
import qualified Koshucode.Baala.Base.Syntax.Tree as B


-- ----------------------  Height function

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
type InfixHeight = Either Int Int

heightValue :: InfixHeight -> Int
heightValue (Left  h) = h
heightValue (Right h) = h

{-| Make the height function from a height table of operators. -}
infixHeight :: (Ord b) => (a -> b) -> [(b, InfixHeight)] -> a -> InfixHeight
infixHeight extract htab a = Maybe.fromMaybe (Left 0) ht where
    ht   = Map.lookup (extract a) hmap
    hmap = Map.fromList htab


-- ----------------------  Conversion

{-| Split branches in a given tree at infixed binary operators -}
infixToPrefix :: (Show a) => (a -> InfixHeight) -> B.Map (B.CodeTree a)
infixToPrefix ht = B.undouble (== 1) . withHeight f ht where
    f x@(B.TreeL _) = x
    f   (B.TreeB n pp xs) =
        case infixPos $ map treeHeight xs of
          Right xi | xi <= 0 -> B.TreeB n pp $ map f xs
          Right xi -> let (left, r:ight) = splitAt xi xs
                      in B.TreeB n pp $ subtrees n left r ight
          Left (xi, yi) -> error $ "ambiguous operator group: "
                           ++ show (xs !! xi) ++ " and "
                           ++ show (xs !! yi)

    subtrees n left r ight | null ight = [r, sub n left]
                           | otherwise = [r, sub n left, sub n ight]
    sub _ [x] = f x
    sub n xs  = f $ B.TreeB n Nothing xs

type HeightTree a = B.CodeTree (InfixHeight, a)

treeHeight :: HeightTree a -> InfixHeight
treeHeight (B.TreeL (ht, _)) = ht
treeHeight (B.TreeB _ _ _)   = Left 0

withHeight :: B.Map (HeightTree a) -> (a -> InfixHeight) -> B.Map (B.CodeTree a)
withHeight f ht = fmap snd . f . fmap height where
    height x = (ht x, x)

infixPos :: [InfixHeight] -> Either (Int, Int) Int
infixPos = pos (Right (-1)) (Left 0) . zip [0 ..] where
    pos res _ [] = res

    -- different height
    -- /res result  /y highest-element  /xi current-index  /xs elements
    pos res y ((xi, x) : xs)
        | hy > hx = pos res        y xs  -- remains y
        | hy < hx = pos (Right xi) x xs  -- replaces y to x
        where  hx = heightValue x
               hy = heightValue y

    -- same height, same direction
    pos _     (Right _) ((xi, x@(Right _)) : xs) = pos (Right xi) x xs -- replaces y to x
    pos res y@(Left  _) ((_ ,   (Left  _)) : xs) = pos res y xs        -- remains y

    -- ambiguous: same height, different direction
    pos (Right yi)      y ((xi, _) : xs) = pos (Left (yi, xi)) y xs
    pos (Left (yi, xi)) y (_       : xs) = pos (Left (yi, xi)) y xs

