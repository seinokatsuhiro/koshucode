{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | Convert infixed-operator trees
--   into prefixed-operator trees

module Koshucode.Baala.Base.Syntax.Infix
( InfixHeight,
  infixHeight,
  infixToPrefix,
) where

import qualified Data.Map   as Map
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

heightValue :: InfixHeight -> Int
heightValue (Left  h) = h
heightValue (Right h) = h

-- | Make the height function from a height table of operators.
infixHeight :: (Ord b) => (a -> Maybe b) -> [(b, InfixHeight)] -> a -> InfixHeight
infixHeight extract htab a = B.fromMaybe (Left 0) ht where
    hmap = Map.fromList htab
    ht   = case extract a of
             Nothing  -> Nothing
             Just key -> Map.lookup key hmap


-- ----------------------  Conversion

type InfixTree a = B.CodeTree (InfixHeight, a)

-- | Split branches in a given tree at infixed binary operators.
infixToPrefix :: forall a. B.Map a -> (a -> InfixHeight)
              -> B.CodeTree a -> Either [(InfixHeight, a)] (B.CodeTree a)
infixToPrefix conv ht tree =
    do tree2 <- toPrefix $ fmap height tree
       Right $ fmap snd tree2
    where
      conv' :: B.Map (InfixTree a)
      conv' = fmap $ B.mapSnd conv

      height :: a -> (InfixHeight, a)
      height x = (ht x, x)

      toPrefix :: InfixTree a -> Either [(InfixHeight, a)] (InfixTree a)
      toPrefix x@(B.TreeL _) = Right x
      toPrefix   (B.TreeB n pp xs) =
          case infixPos $ map treeHeight xs of
            Right xi | xi <= 0 -> Right . B.TreeB n pp =<< mapM toPrefix xs
            Right xi -> do let (left, r:ight) = splitAt xi xs
                           s <- subtrees n left r ight
                           Right $ B.TreeB n pp s
            Left xi -> Left $ B.untrees $ map (xs !!) xi

      subtrees :: B.ParenType -> [InfixTree a] -> InfixTree a -> [InfixTree a]
                  -> Either [(InfixHeight, a)] [InfixTree a]
      subtrees n left r ight | null ight = do lt <- sub n left
                                              Right [r, lt]
                             | otherwise = do lt <- sub n left
                                              rt <- sub n ight
                                              Right [conv' r, lt, rt]

      sub :: B.ParenType -> [InfixTree a] -> Either [(InfixHeight, a)] (InfixTree a) 
      sub _ [x] = toPrefix x
      sub n xs  = toPrefix $ B.TreeB n Nothing xs

type HeightTree a = B.CodeTree (InfixHeight, a)

treeHeight :: HeightTree a -> InfixHeight
treeHeight (B.TreeL (ht, _)) = ht
treeHeight (B.TreeB _ _ _)   = Left 0

infixPos :: [InfixHeight] -> Either [Int] Int
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
    pos (Right yi) y ((xi, _) : xs) = pos (Left [xi, yi]) y xs
    pos (Left ps)  y ((xi, _) : xs) = pos (Left $ xi : ps) y xs

