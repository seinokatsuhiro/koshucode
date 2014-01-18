{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC #-}

{-| General utilities -}

module Koshucode.Baala.Base.Prelude.Utility
(
  -- * Pair
  mapFst,
  mapSnd,
  cons1,
  mapmapFst,
  mapmapSnd,
  maybePairs,

  -- * List
  front,
  unique,
  unionUp,
  singleton,
  isSingleton,
  splitBy,
  divide,
  divideBy,
  assocOmit,
  assocOmitAll,
  maybeEmpty,

  -- * String
  padRight,
  padLeft,

  -- * Gather
  Gather,
  gather,
  gatherWith,
  gatherToMap,

  -- * I/O
  putShow,
  putShowLn,
  putLines,
) where

import Control.Applicative
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map  as Map
import qualified Data.Set  as Set

import qualified Koshucode.Baala.Base.Prelude.Class as B



-- ----------------------  Pair

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (x, y) = (f x, y)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x, y) = (x, f y)

cons1 :: a -> ([a], b) -> ([a], b)
cons1 x = mapFst (x:)

mapmapFst :: (a -> c) -> [(a, b)] -> [(c, b)]
mapmapFst = map . mapFst

mapmapSnd :: (b -> c) -> [(a, b)] -> [(a, c)]
mapmapSnd = map . mapSnd

maybePairs :: [a] -> Maybe [(a, a)]
maybePairs (a:b:xs) = liftA ((a, b):) $ maybePairs xs
maybePairs []       = Just []
maybePairs _        = Nothing



-- ----------------------  List

front :: [a] -> [a]
front [] = []
front (x : _) = [x]

{-| Remove duplicate elements. -}
unique :: (Ord a) => [a] -> [a]
unique xs = loop xs Set.empty where
    loop [] _ = []
    loop (x:xs2) set
        | Set.member x set = loop xs2 set
        | otherwise        = x : loop xs2 (Set.insert x set)

{-| Union list to base list.

    >>> unionUp "cde" "abc"
    "deabc"

    >>> List.union "cde" "abc"
    "cdeab"  -}
unionUp
    :: (Eq a)
    => [a]  -- ^ Append list
    -> [a]  -- ^ Base list
    -> [a]  -- ^ Result
unionUp xs ys = (xs List.\\ ys) ++ ys

{-| Make singleton list. -}
singleton :: a -> [a]
singleton x = [x]

isSingleton :: [a] -> Bool
isSingleton [_] = True
isSingleton  _  = False

{-| Split list by predicate.
    If list contains an element that satisfies the predicate,
    @(/before-list/, /the-element/, /after-list/)@ is returned.
    Otherwise, original list is returned.

    >>> splitBy (== '|') "b c"
    Left "b c"

    >>> splitBy (== '|') "a | b | c"
    Right ("a ", '|', " b | c")  -}
splitBy
    :: (a -> Bool)
    -> [a]
    -> Either [a] ([a], a, [a])
splitBy p xs =
    case break p xs of
      (a, x : b) -> Right (a, x, b)
      _          -> Left xs

{-| Divide list.

    >>> divide '|' "a|bb||ccc|"
    ["a", "bb", "", "ccc", ""]  -}
divide :: (Eq a) => a -> [a] -> [[a]]
divide dv = divideBy (== dv)

{-| Divide list.

    >>> divideBy (== '|') "a|bb||ccc|"
    ["a", "bb", "", "ccc", ""]  -}
divideBy :: (a -> Bool) -> [a] -> [[a]]
divideBy p = loop where
    loop xs = case break p xs of
                (x, _ : xs2) -> x : loop xs2
                (x, [])      -> [x]

stringWidth :: String -> Int
stringWidth = sum . map charWidth

charWidth :: Char -> Int
charWidth c
    | Char.ord c >= 256 = 2
    | otherwise         = 1

{-| Omit association that has given key.

    >>> assocOmit "b" [("a",1), ("b",2), ("c",3)]
    [("a",1), ("c",3)]  -}
assocOmit :: (Eq k) => k -> B.Map [(k, a)]
assocOmit k1 = loop where
    loop [] = []
    loop (x@(k2, _) : xs)
        | k1 == k2   =  xs
        | otherwise  =  x : loop xs

{-| Omit associations that have given keys. -}
assocOmitAll :: (Eq k) => [k] -> B.Map [(k, a)]
assocOmitAll ks xs = foldr assocOmit xs ks

maybeEmpty :: Maybe a -> (a -> [b]) -> [b]
maybeEmpty m f = maybe [] f m


-- ----------------------  String

{-| Add spaces to right.

    >>> padRight 10 "abc"
    "abc       "  -}
padRight :: Int -> B.Map String
padRight n s = s ++ replicate rest ' ' where
    rest = max 0 (n - stringWidth s)

{-| Add spaces to left.

    >>> padLeft 10 "abc"
    "       abc"  -}
padLeft :: Int -> B.Map String
padLeft n s = replicate rest ' ' ++ s where
    rest = max 0 (n - stringWidth s)



-- ----------------------  Gather

type Gather a b = a -> (b, a)

{-| Gather what is gotten by splitter. -}
gather :: Gather [a] b -> [a] -> [b]
gather one = loop where
    loop [] = []
    loop xs = let (y, xs2) = one xs
              in y : loop xs2

gatherWith :: (c -> Gather [a] b) -> [c] -> [a] -> [b]
gatherWith f = loop where
    loop [] _ = []
    loop _ [] = []
    loop (c:cs) as = let (b, as') = f c as
                     in b : loop cs as'

{-| Gather (/key/, /value/) to 'Map.Map' /key/ [/value/]. -}
gatherToMap :: (Ord k) => [(k,v)] -> Map.Map k [v]
gatherToMap xs = loop xs Map.empty where
    loop [] m = m
    loop ((k,v) : xs2) m =
        case Map.lookup k m of
          Just vs -> loop xs2 $ Map.insert k (v:vs) m
          Nothing -> loop xs2 $ Map.insert k [v] m



-- ----------------------  Lookup

putShow :: (Show a) => a -> IO ()
putShow = putStr . show

putShowLn :: (Show a) => a -> IO ()
putShowLn = putStrLn . show

putLines :: [String] -> IO ()
putLines = putStr . unlines

