{-# OPTIONS_GHC -Wall #-}

-- | Suffix and prefix.

module Koshucode.Baala.Base.Text.Suffix
  ( unprefix,
    unsuffix,
    uniqueNames,
  ) where

import qualified Data.Map.Strict                   as Map
import qualified Koshucode.Baala.Overture          as O
import qualified Koshucode.Baala.Base.Prelude      as B
import qualified Koshucode.Baala.Base.List         as B

-- | Remove characters-delimiter prefix.
--
--   >>> unprefix (`elem` "0123456789") '-' "12-foo"
--   "foo"
--
unprefix :: O.Test Char -> Char -> B.Map String
unprefix test del s = loop s where
    loop [] = s
    loop (c:cs) | test c      = loop cs
                | c == del    = cs
                | otherwise   = s

-- | Remove delimiter-characters suffix.
--
--   >>> unsuffix (`elem` "0123456789") '-' "foo-12"
--   "foo"
--
unsuffix :: O.Test Char -> Char -> B.Map String
unsuffix test del = B.reverseMap $ unprefix test del

-- | Two-argument function which returns a constant value.
--
--   >>> const2 "c" True ()
--   "c"
--
const2 :: a -> b -> c -> a
const2 a _ _ = a

-- | Homotype 'Either'.
type Eith a = Either a a

uneith :: Eith a -> a
uneith (Left x)   = x
uneith (Right x)  = x

-- | Make names unique by adding integer suffixes.
--
--   >>> uniqueNames '-' $ words "a b b c"
--   ["a", "b-1", "b-2", "c"]
--
uniqueNames :: Char -> B.Map [String]
uniqueNames del xs = uneith <$> uniqueNamesEith del (Left <$> xs)

uniqueNamesEith :: Char -> B.Map [Eith String]
uniqueNamesEith del xxs = loop False (Map.map intSuffixes dup) xxs [] where
    loop b m (Left x : xs) ys
        = case Map.lookup x m of
            Just (n : ns) -> let m' = Map.insert x ns m
                             in loop True m' xs (Right (x ++ n) : ys)
            _             -> loop b m xs (Left x : ys)
    loop b m (x : xs) ys  = loop b m xs (x : ys)
    loop True  _ [] ys    = uniqueNamesEith del $ reverse ys
    loop False _ [] ys    = reverse ys  -- completed

    dup = Map.filter (> 0) $ duplicateMap $ map name xxs

    duplicateMap :: (Ord a) => [(a, Int)] -> Map.Map a Int
    duplicateMap = Map.fromListWith $ const2 1

    name (Left  x) = (x, 0)
    name (Right x) = (x, 0)

-- | Generate list of integer suffixes.
--
--   >>> take 5 $ intSuffixes 1
--   ["-1", "-2", "-3", "-4", "-5"]
--
intSuffixes :: Int -> [String]
intSuffixes i = ('-' : show i) : intSuffixes (i + 1)

