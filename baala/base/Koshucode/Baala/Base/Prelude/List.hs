{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Prelude.List
( front,
  tails,
  isSingleton,
  notNull,
  maybeEmpty,
  right,
  -- $List

  -- * Uniqueness
  duplicates,
  unique, unionUp,
  -- $Uniqueness

  -- * Construct
  li1, li2, li3,

  -- * Map
  map2, mapAt, mapWithLast,
  omit, squeeze, squeezeEmptyLines,

  -- * Divide
  chunks,
  splitBy,
  divide, divideBy,
  -- $Divide
) where

import qualified Data.List as List
import qualified Data.Set  as Set
import qualified Koshucode.Baala.Base.Prelude.Class as B


-- ----------------------  List

-- $List
--
--   /Examples/
--
--   >>> front "abc"
--   "a"
--
--   >>> front ""
--   ""
--
--   >>> tails "abc"
--   ["abc", "bc", "c"]
--

-- | Head or empty.
front :: [a] -> [a]
front [] = []
front (x : _) = [x]

-- | Tails of list.
tails :: [a] -> [[a]]
tails = loop where
    loop [] = []
    loop xxs@(_:xs) = xxs : loop xs

-- | Test list has single element.
isSingleton :: [a] -> Bool
isSingleton [_] = True
isSingleton  _  = False

-- | Test list is not empty.
notNull :: [a] -> Bool
notNull = not . null

maybeEmpty :: Maybe a -> (a -> [b]) -> [b]
maybeEmpty m f = maybe [] f m

right :: b -> B.Map (Either a b)
right _ (Right x) = Right x
right x (Left _)  = Right x


-- ----------------------  Uniqueness

-- $Uniqueness
--
--   /Examples/
--
--   >>> duplicates "banana"
--   "ana"
--
--   >>> unique "banana"
--   "ban"
--
--   >>> unionUp "cde" "abc"
--   "deabc"
--
--   >>> List.union "cde" "abc"
--   "cdeab"
--

-- | Keep duplicate elements.
duplicates :: (Ord a) => [a] -> [a]
duplicates xs = loop xs Set.empty where
    loop [] _ = []
    loop (x:xs2) set
        | Set.member x set = x : loop xs2 set
        | otherwise        = loop xs2 (Set.insert x set)

-- | Remove duplicate elements.
unique :: (Ord a) => [a] -> [a]
unique xs = loop xs Set.empty where
    loop [] _ = []
    loop (x:xs2) set
        | Set.member x set = loop xs2 set
        | otherwise        = x : loop xs2 (Set.insert x set)

-- | Union list to base list.
unionUp :: (Eq a) => [a] -> [a] -> [a]
unionUp xs ys = (xs List.\\ ys) ++ ys


-- ----------------------  Construct

-- | Make singleton list.
li1 :: a -> [a]
li1 x = [x]

li2 :: a -> a -> [a]
li2 x y = [x, y]

li3 :: a -> a -> a -> [a]
li3 x y z = [x, y, z]


-- ----------------------  Map

map2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
map2 = fmap . fmap

-- map2 :: (a -> b) -> [[a]] -> [[b]]
-- map2 = map . map

mapAt :: (B.Map a) -> Int -> B.Map [a]
mapAt f = loop where
    loop 0 (x : xs) = f x : xs
    loop i (x : xs) = x : loop (i - 1) xs
    loop _ []       = []

mapWithLast :: (a -> b) -> (a -> b) -> [a] -> [b]
mapWithLast f g = loop where
    loop [] = []
    loop [x] = [g x]
    loop (x:xs) = f x : loop xs

omit :: (a -> Bool) -> B.Map [a]
omit f = filter $ not . f

squeeze :: (a -> Bool) -> B.Map [a]
squeeze p = loop where
    loop (x1 : x2 : xs)
        | p x1 && p x2 = x2 : squeeze p xs
        | otherwise    = x1 : squeeze p (x2 : xs)
    loop xs = xs

squeezeEmptyLines :: B.Map [String]
squeezeEmptyLines = squeeze $ null . dropWhile (== ' ')


-- ----------------------  Divide

-- $Divide
--
--   /Examples/
--
--   >>> splitBy (== '|') "b c"
--   Left "b c"
--
--   >>> splitBy (== '|') "a | b | c"
--   Right ("a ", '|', " b | c")
--
--   >>> divide '|' "a|bb||ccc|"
--   ["a", "bb", "", "ccc", ""]
--
--   >>> divideBy (== '|') "a|bb||ccc|"
--   ["a", "bb", "", "ccc", ""]

chunks :: Int -> [a] -> [[a]]
chunks n = loop where
    loop xs = case splitAt n xs of
                ([], _)      -> []
                (taked, xs2) -> taked : loop xs2

-- | Split list by predicate.
--   If list contains an element that satisfies the predicate,
--   @(/before-list/, /the-element/, /after-list/)@ is returned.
--   Otherwise, original list is returned.
splitBy :: (a -> Bool) -> [a] -> Either [a] ([a], a, [a])
splitBy p xs =
    case break p xs of
      (a, x : b) -> Right (a, x, b)
      _          -> Left xs

-- | Divide list.
divide :: (Eq a) => a -> [a] -> [[a]]
divide dv = divideBy (== dv)

-- | Divide list.
divideBy :: (a -> Bool) -> [a] -> [[a]]
divideBy p = loop where
    loop xs = case break p xs of
                (x, _ : xs2) -> x : loop xs2
                (x, [])      -> [x]

