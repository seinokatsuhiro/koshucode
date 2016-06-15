{-# OPTIONS_GHC -Wall #-}

-- | Utilities for association lists.

module Koshucode.Baala.Base.List.Assoc
  ( -- * Association list
    Lookup,
    assocBy,
    assocExist,
    namedMapM,
    lookupBy,
    lookupSatisfy,
    assocFinder,
    -- $Assoc
  
    -- * Tuple-like operator
    assocPick,
    assocCut,
    assocCut1,
    assocRename1,
    assocRehead,
    assocCompose, assocMeet,
  
    -- * Once/more list
    OnceMore (..),
    assocGather,
    assocPush,
    assocOnce,
    assocMore,
    -- $OnceMore
  
    -- * Gather
    Gather, gather, gatherWith,
    gatherToMap, gatherToMapSwap, gatherToAssoc,
  ) where

import qualified Data.Map                       as Map
import qualified Data.Maybe                     as Maybe
import qualified Koshucode.Baala.Base.Prelude   as B



-- ----------------------  Association list

-- $Assoc
--
--  /Examples/
--
--  Construct assoc list by splitting at char @\'a\'@.
--
--    >>> assocBy (`lookup` [('a', "A")]) "banana apple cocoa"
--    ("b", [("A","n"), ("A","n"), ("A"," "), ("A","pple coco"), ("A","")])
--    >>> assocBy (`lookup` [('a', "A")]) ""
--    ("", [])
--
--  Check key exists.
--
--    >>> assocExist "b" [("a",1), ("b",2), ("c",3)]
--    True
--    >>> assocExist "e" [("a",1), ("b",2), ("c",3)]
--    False
--
--  Lookup entry that satisfies @\'n\' \`elem\` \"banana\"@.
--
--    >>> lookupSatisfy 'n' [((`elem` "apple"), 1), ((`elem` "banana"), 2)]
--    Just 2
--
--  Extract value in 'Maybe'.
--
--    >>> fromMaybe 0 (Just 12)
--    12
--    >>> fromMaybe 0 (Nothing)
--    0

type Lookup a = String -> Maybe a

-- | Construct assoc list by splitting base list.
assocBy :: (a -> Maybe k) -> [a] -> ([a], [(k, [a])])
assocBy p = lead [] where
    lead ys []         = (reverse ys, [])
    lead ys (x : xs)   = case p x of
                           Nothing -> lead (x : ys) xs
                           Just k  -> (reverse ys, ass k [] xs)

    ass k ys []        = [(k, reverse ys)]
    ass k ys (x : xs)  = case p x of
                           Nothing -> ass k (x : ys) xs
                           Just k' -> (k, reverse ys) : ass k' [] xs

-- | Check which given key is there in assoc list.
assocExist :: (Eq k) => k -> [(k, a)] -> Bool
assocExist k a = Maybe.isJust $ lookup k a

namedMapM :: (Monad m) => (b -> m c) -> (a, b) -> m (a, c)
namedMapM f (a, b) =
    do c <- f b
       return (a, c)

lookupBy :: (a -> Bool) -> [(a, b)] -> Maybe b
lookupBy p = loop where
    loop [] = Nothing
    loop ((k, v) : xs)
        | p k = Just v
        | otherwise = loop xs

-- | Lookup assoc list that keys are truth-valued functions.
lookupSatisfy :: a -> [(a -> Bool, b)] -> Maybe b
lookupSatisfy x = loop where
    loop [] = Nothing
    loop ((p, v) : ps)
        | p x = Just v
        | otherwise = loop ps

assocFinder :: (Ord k) => [(k, v)] -> k -> Maybe v
assocFinder xs k = Map.lookup k $ Map.fromList xs


-- ----------------------  Tuple-like operator

-- | Pick up associations that have given keys.
--
--   >>> assocPick ["a","c"] [("a",1), ("b",2), ("c",3)]
--   [("a",1), ("c",3)]
assocPick :: (Eq k) => [k] -> B.Map [(k, a)]
assocPick xs = loop where
    loop [] = []
    loop (kv@(key, _) : kvs)
        | key `elem` xs = kv : loop kvs
        | otherwise     =      loop kvs

-- | Cut off associations that have given keys.
--
--   >>> assocCut ["a","c"] [("a",1), ("b",2), ("c",3)]
--   [("b",2)]
--
--   >>> assocCut1 "b" [("a",1), ("b",2), ("c",3)]
--   [("a",1), ("c",3)]
assocCut :: (Eq k) => [k] -> B.Map [(k, a)]
assocCut ks xs = foldr assocCut1 xs ks

assocCut1 :: (Eq k) => k -> B.Map [(k, a)]
assocCut1 k1 = loop where
    loop [] = []
    loop (x@(k2, _) : xs)
        | k1 == k2   =  xs
        | otherwise  =  x : loop xs

-- | Change key of assoc list.
--
--   >>> assocRename1 "x" "a" [("a",1), ("b",2), ("c",3)]
--   [("x",1), ("b",2), ("c",3)]
assocRename1 :: (Eq k) => k -> k -> B.Map [(k, a)]
assocRename1 new old = map r where
    r (k, x) | k == old  = (new, x)
             | otherwise = (k, x)

assocRehead :: (Eq k) => [(k,k)] -> [(k,v)] -> [(k,v)]
assocRehead new = map rehead where
    rehead (k1,v) = case lookup k1 new of
                      Nothing -> (k1,v)
                      Just k2 -> (k2,v)

-- | Compose two assocs.
--
--   >>> assocCompose [("A","H"), ("B","J")] [("H","P"), ("H","Q"), ("I","R"), ("J","S")]
--   [("A","P"), ("A","Q"), ("B","S")]
assocCompose :: (Eq b) => [(a, b)] -> [(b, c)] -> [(a, c)]
assocCompose ab bc =
    [ (a, c) | (a, b)  <- ab
             , (b', c) <- bc
             , b == b' ]

-- | Meet two assocs.
--
--   >>> assocMeet [("A","H"), ("B","J")] [("H","P"), ("H","Q"), ("I","R"), ("J","S")]
--   [("A","H","P"), ("A","H","Q"), ("B","J","S")]
assocMeet :: (Eq b) => [(a, b)] -> [(b, c)] -> [(a, b, c)]
assocMeet ab bc =
    [ (a, b, c) | (a, b)  <- ab
                , (b', c) <- bc
                , b == b' ]


-- ----------------------  Once/more list

-- $OnceMore
--
--  /Examples/
--
--  Convert into once/more list.
--
--    >>> assocGather [(1, "1"), (2, "2"), (1, "01")]
--    [(1, More ["1", "01"]), (2, Once "2")]
--
--  Push key and value into once/more list
--
--    >>> assocPush 2 "02" [(1, Once "1"), (2, Once "2")]
--    [(1, Once "1"), (2, More ["02", "2"])]
--
--  Extract once part.
--
--    >>> assocOnce [(1, Once "1"), (2, More ["02", "2"])]
--    [(1, "1")]
--
--  Extract more part.
--
--    >>> assocMore [(1, Once "1"), (2, More ["02", "2"])]
--    [(2, ["02", "2"])]
--

data OnceMore a
    = Once a
    | More [a]
      deriving (Show, Eq, Ord)

-- isOnce :: OnceMore a -> Bool
-- isOnce (Once _) = True
-- isOnce (More _) = False

-- isMore :: OnceMore a -> Bool
-- isMore = not . isOnce

push :: a -> B.Map (OnceMore a)
push x (Once x2) = More [x, x2]
push x (More xs) = More (x : xs)

-- | Convert general assoc list into once/more assoc list.
assocGather :: (Eq k) => [(k, a)] -> [(k, OnceMore a)]
assocGather [] = []
assocGather ((k, a) : xs) = assocPush k a $ assocGather xs

-- | Push  key and value into once/more assoc list.
assocPush :: (Eq k) => k -> a -> B.Map [(k, OnceMore a)]
assocPush k x = loop where
    loop [] = [(k, Once x)]
    loop (p@(k2, x2) : xs)
        | k2 == k   = (k, push x x2) : xs
        | otherwise = p : loop xs

-- | Extract once part from once/more assoc list.
assocOnce :: [(k, OnceMore a)] -> [(k, a)]
assocOnce = loop where
    loop [] = []
    loop ((k, Once x) : xs) = (k, x) : loop xs
    loop (_           : xs) =          loop xs

-- | Extract more part from once/more assoc list.
assocMore :: [(k, OnceMore a)] -> [(k, [a])]
assocMore = loop where
    loop [] = []
    loop ((k, More x) : xs) = (k, x) : loop xs
    loop (_           : xs) =          loop xs


-- ----------------------  Gather

type Gather a b = a -> (b, a)

-- | Gather what is gotten by splitter.
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

-- | Gather (/k/, /v/) to 'Map.Map' /k/ [/v/].
gatherToMap :: (Ord k) => [(k,v)] -> Map.Map k [v]
gatherToMap xs = loop xs Map.empty where
    loop [] m = m
    loop ((k,v) : xs2) m =
        case Map.lookup k m of
          Just vs -> loop xs2 $ Map.insert k (v:vs) m
          Nothing -> loop xs2 $ Map.insert k [v] m

-- gatherToMap :: (Ord k) => [(k,v)] -> Map.Map k [v]
-- gatherToMap = foldr gath Map.empty where
--     gath (k,v) = Map.alter (add v) k
--
--     add v (Just vs) = Just $ v:vs
--     add v (Nothing) = Just [v]

-- | Gather (/v/, /k/) to 'Map.Map' /k/ [/v/].
gatherToMapSwap :: (Ord k) => [(v,k)] -> Map.Map k [v]
gatherToMapSwap = foldr gath Map.empty where
    gath (v,k) = Map.alter (add v) k

    add v (Just vs) = Just $ v:vs
    add v (Nothing) = Just [v]

gatherToAssoc :: (Ord k) => [(k,v)] -> [(k, [v])]
gatherToAssoc = Map.assocs . gatherToMap