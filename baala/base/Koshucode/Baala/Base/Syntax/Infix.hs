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

type InfixTree a = B.CodeTree (InfixHeight, a)

-- | Split branches in a given tree at infixed binary operators.
infixToPrefix :: forall a. (B.Map a, B.Map a, B.Map a) -> (a -> InfixHeight)
              -> B.CodeTree a -> Either [(InfixHeight, a)] (B.CodeTree a)
infixToPrefix (pre, inf, post) ht tree =
    do tree2 <- toPrefix $ fmap height tree
       Right $ fmap snd tree2
    where
      conv :: B.Map a -> B.Map (InfixTree a)
      conv = fmap . B.mapSnd

      height :: a -> (InfixHeight, a)
      height x = (ht x, x)

      toPrefix :: InfixTree a -> Either [(InfixHeight, a)] (InfixTree a)
      toPrefix x@(B.TreeL _) = Right x
      toPrefix   (B.TreeB n pp xs) =
          case infixPos $ map treeHeight xs of
            Left xi -> Left $ B.untrees $ map (xs !!) xi
            Right (xi, hx)
                | xi == 0 && hx > 0 -> case xs of
                    (op : right) -> do rt <- sub n right
                                       Right $ B.TreeB n pp [conv pre op, rt]
                    []           -> Right $ B.TreeB n pp []
                | xi <= 0        -> Right . B.TreeB n pp =<< mapM toPrefix xs
                | otherwise      -> do let (left, op : right) = splitAt xi xs
                                       s <- subtrees n left op right
                                       Right $ B.TreeB n pp s

      subtrees :: B.ParenType -> [InfixTree a] -> InfixTree a -> [InfixTree a]
                  -> Either [(InfixHeight, a)] [InfixTree a]
      subtrees n left op right
          | null right = do lt <- sub n left
                            Right [conv post op, lt]
          | otherwise  = do lt <- sub n left
                            rt <- sub n right
                            Right [conv inf op, lt, rt]

      sub :: B.ParenType -> [InfixTree a] -> Either [(InfixHeight, a)] (InfixTree a) 
      sub _ [x] = toPrefix x
      sub n xs  = toPrefix $ B.TreeB n Nothing xs

-- Tree with height attribute.
type HeightTree a = B.CodeTree (InfixHeight, a)

treeHeight :: HeightTree a -> InfixHeight
treeHeight (B.TreeL (ht, _))  =  ht
treeHeight (B.TreeB _ _ _)    =  Left 0

infixPos :: [InfixHeight] -> Either [B.Index] (B.Index, Int)
infixPos = pos (Right (-1, 0)) (Left 0) . zip [0 ..] where
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

