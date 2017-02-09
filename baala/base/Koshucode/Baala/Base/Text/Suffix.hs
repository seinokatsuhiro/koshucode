{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

-- | Suffix and prefix.

module Koshucode.Baala.Base.Text.Suffix
  ( unprefix,
    unsuffix,
    uniqueNames,
  ) where

import qualified Data.Map.Strict                   as Ms
import qualified Koshucode.Baala.Overture          as O

-- | Remove characters-delimiter prefix.
--
--   >>> unprefix (`elem` "0123456789") '-' "12-foo"
--   "foo"
--
unprefix :: (O.Textual t) => O.Test Char -> Char -> t -> t
unprefix test del s = loop s where
    loop (O.cut -> O.Jp c cs)
        | test c     = loop cs
        | c == del   = cs
        | otherwise  = s
    loop _           = s

-- | Remove delimiter-characters suffix.
--
--   >>> unsuffix (`elem` "0123456789") '-' "foo-12"
--   "foo"
--
unsuffix :: (O.Textual t) => O.Test Char -> Char -> t -> t
unsuffix test del = O.csReverse . unprefix test del . O.csReverse

-- | Two-argument function which returns a constant value.
--
--   >>> const2 "c" True ()
--   "c"
--
const2 :: a -> b -> c -> a
const2 a _ _ = a

-- | Make names unique by adding integer suffixes.
--
--   >>> uniqueNames '-' $ words "a b b c"
--   ["a", "b-1", "b-2", "c"]
--
uniqueNames :: (O.Textual t) => Char -> O.Map [t]
uniqueNames del xs = O.uneith <$> uniqueNamesEith del (Left <$> xs)

uniqueNamesEith :: (O.Textual t) => Char -> O.Map [O.Eith t]
uniqueNamesEith del xxs = loop False (Ms.map intSuffixes dup) xxs [] where
    loop b m (Left x : xs) ys
        = case Ms.lookup x m of
            Just (n : ns) -> let m' = Ms.insert x ns m
                             in loop True m' xs (Right (x O.++ n) : ys)
            _             -> loop b m xs (Left x : ys)
    loop b m (x : xs) ys  = loop b m xs (x : ys)
    loop True  _ [] ys    = uniqueNamesEith del $ reverse ys
    loop False _ [] ys    = reverse ys  -- completed

    dup = Ms.filter (> 0) $ duplicateMap $ map name xxs

    duplicateMap :: (Ord a) => [(a, Int)] -> Ms.Map a Int
    duplicateMap = Ms.fromListWith $ const2 1

    name (Left  x) = (x, 0)
    name (Right x) = (x, 0)

-- | Generate list of integer suffixes.
--
--   >>> take 5 $ intSuffixes 1
--   ["-1", "-2", "-3", "-4", "-5"]
--
intSuffixes :: (O.Textual t) => Int -> [t]
intSuffixes i = ('-' O.<:> O.showT i) : intSuffixes (i + 1)

