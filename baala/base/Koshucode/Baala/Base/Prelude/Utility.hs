{-# OPTIONS_GHC -Wall #-}

-- | General utilities

module Koshucode.Baala.Base.Prelude.Utility
(
  -- * Pair
  mapFst,
  mapSnd,
  cons1,
  mapFstTo,
  mapSndTo,
  maybePairs,
  sequenceSnd,

  -- * List
  front,
  omit, duplicates,
  unique, unionUp,
  singleton, isSingleton,
  splitBy, divide, divideBy,
  maybeEmpty,
  squeeze, squeezeEmptyLines,
  map2,
  mapWithLast,
  notNull,
) where

import Control.Applicative
import qualified Data.List as List
import qualified Data.Set  as Set

import qualified Koshucode.Baala.Base.Prelude.Class as B



-- ----------------------  Pair

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (x, y) = (f x, y)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x, y) = (x, f y)

cons1 :: a -> ([a], b) -> ([a], b)
cons1 x = mapFst (x:)

mapFstTo :: (Functor m) => (a -> c) -> m (a, b) -> m (c, b)
mapFstTo = fmap . mapFst

mapSndTo :: (Functor m) => (b -> c) -> m (a, b) -> m  (a, c)
mapSndTo = fmap . fmap

maybePairs :: [a] -> Maybe [(a, a)]
maybePairs (a:b:xs) = liftA ((a, b):) $ maybePairs xs
maybePairs []       = Just []
maybePairs _        = Nothing

sequenceSnd :: (Monad m) => [(a, m b)] -> m [(a, b)]
sequenceSnd xs =
    do let (fs, ss) = unzip xs
       ss' <- sequence ss
       return $ zip fs ss'



-- ----------------------  List

front :: [a] -> [a]
front [] = []
front (x : _) = [x]

omit :: (a -> Bool) -> [a] -> [a]
omit f = filter $ not . f

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
--
--   >>> unionUp "cde" "abc"
--   "deabc"
--
--   >>> List.union "cde" "abc"
--   "cdeab"
unionUp
    :: (Eq a)
    => [a]  -- ^ Append list
    -> [a]  -- ^ Base list
    -> [a]  -- ^ Result
unionUp xs ys = (xs List.\\ ys) ++ ys

-- | Make singleton list.
singleton :: a -> [a]
singleton x = [x]

isSingleton :: [a] -> Bool
isSingleton [_] = True
isSingleton  _  = False

-- | Split list by predicate.
--   If list contains an element that satisfies the predicate,
--   @(/before-list/, /the-element/, /after-list/)@ is returned.
--   Otherwise, original list is returned.
--
--   >>> splitBy (== '|') "b c"
--   Left "b c"
--
--   >>> splitBy (== '|') "a | b | c"
--   Right ("a ", '|', " b | c")
splitBy
    :: (a -> Bool)
    -> [a]
    -> Either [a] ([a], a, [a])
splitBy p xs =
    case break p xs of
      (a, x : b) -> Right (a, x, b)
      _          -> Left xs

-- | Divide list.
--
--   >>> divide '|' "a|bb||ccc|"
--   ["a", "bb", "", "ccc", ""]
divide :: (Eq a) => a -> [a] -> [[a]]
divide dv = divideBy (== dv)

-- | Divide list.
--
--   >>> divideBy (== '|') "a|bb||ccc|"
--   ["a", "bb", "", "ccc", ""]
divideBy :: (a -> Bool) -> [a] -> [[a]]
divideBy p = loop where
    loop xs = case break p xs of
                (x, _ : xs2) -> x : loop xs2
                (x, [])      -> [x]

maybeEmpty :: Maybe a -> (a -> [b]) -> [b]
maybeEmpty m f = maybe [] f m

squeeze :: (a -> Bool) -> B.Map [a]
squeeze p = loop where
    loop (x1 : x2 : xs)
        | p x1 && p x2 = x2 : squeeze p xs
        | otherwise    = x1 : squeeze p (x2 : xs)
    loop xs = xs

squeezeEmptyLines :: B.Map [String]
squeezeEmptyLines = squeeze $ null . dropWhile (== ' ')

map2 :: (a -> b) -> [[a]] -> [[b]]
map2 = map . map

mapWithLast :: (a -> b) -> (a -> b) -> [a] -> [b]
mapWithLast f g = loop where
    loop [] = []
    loop [x] = [g x]
    loop (x:xs) = f x : loop xs

notNull :: [a] -> Bool
notNull = not . null


