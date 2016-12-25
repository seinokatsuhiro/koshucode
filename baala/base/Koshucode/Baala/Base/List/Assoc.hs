{-# OPTIONS_GHC -Wall #-}

-- | Utilities for association lists.

module Koshucode.Baala.Base.List.Assoc
  ( -- * Association list
    assocBy,
    assocExist,
    lookupBy,
    lookupSatisfy,
    assocFinder,
    -- $Assoc
  
    -- * Tuple-like operator
    assocPick,
    assocCut,
    assocCut1,
    assocRename1,
    assocCompose, assocMeet,
  
    -- * Gather
    Gather, gather, --gatherWith,
    gatherToMap, gatherToMapSwap, gatherToAssoc,
  ) where

import qualified Data.Map                       as Map
import qualified Data.Maybe                     as Maybe
import qualified Koshucode.Baala.Overture       as O



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

-- | Lookup association list using Boolean function.
--
--   >>> lookupBy (> (4 :: Int)) [(1, "one"), (3, "three"), (5, "five")]
--   Just "five"
--
lookupBy :: O.Test a -> [(a, b)] -> Maybe b
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

-- | Create lookup function for fixed association list.
--
--   >>> let f = assocFinder [('a', "apple"), ('b', "banana")]
--   >>> f 'a'
--   Just "apple"
--
assocFinder :: (Ord k) => [(k, v)] -> k -> Maybe v
assocFinder xs k = Map.lookup k $ Map.fromList xs


-- ----------------------  Tuple-like operator

-- | Pick up associations that have given keys.
--
--   >>> assocPick ["a","c"] [("a",1), ("b",2), ("c",3)]
--   [("a",1), ("c",3)]
assocPick :: (Eq k) => [k] -> O.Map [(k, a)]
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
assocCut :: (Eq k) => [k] -> O.Map [(k, a)]
assocCut ks xs = foldr assocCut1 xs ks

-- | Single key version of 'assocCut'.
--
--   >>> assocCut1 "b" [("a",1), ("b",2), ("c",3)]
--   [("a",1), ("c",3)]
--
assocCut1 :: (Eq k) => k -> O.Map [(k, a)]
assocCut1 k1 = loop where
    loop [] = []
    loop (x@(k2, _) : xs)
        | k1 == k2   =  xs
        | otherwise  =  x : loop xs

-- | Change key of assoc list.
--
--   >>> assocRename1 "x" "a" [("a",1), ("b",2), ("c",3)]
--   [("x",1), ("b",2), ("c",3)]
--
assocRename1 :: (Eq k) => k -> k -> O.Map [(k, a)]
assocRename1 new old = map r where
    r (k, x) | k == old  = (new, x)
             | otherwise = (k, x)

-- | Compose two assocs.
--
--   >>> assocCompose [("A","H"), ("B","J")] [("H","P"), ("H","Q"), ("I","R"), ("J","S")]
--   [("A","P"), ("A","Q"), ("B","S")]
--
assocCompose :: (Eq b) => [(a, b)] -> [(b, c)] -> [(a, c)]
assocCompose ab bc =
    [ (a, c) | (a, b)  <- ab
             , (b', c) <- bc
             , b == b' ]

-- | Meet two assocs.
--
--   >>> assocMeet [("A","H"), ("B","J")] [("H","P"), ("H","Q"), ("I","R"), ("J","S")]
--   [("A","H","P"), ("A","H","Q"), ("B","J","S")]
--
assocMeet :: (Eq b) => [(a, b)] -> [(b, c)] -> [(a, b, c)]
assocMeet ab bc =
    [ (a, b, c) | (a, b)  <- ab
                , (b', c) <- bc
                , b == b' ]


-- ----------------------  Gather

-- | Snip /b/ from sequence /a/.
type Gather a b = a -> (b, a)

-- | Gather what is gotten by splitter.
gather :: Gather [a] b -> [a] -> [b]
gather one = loop where
    loop [] = []
    loop xs = let (y, xs2) = one xs
              in y : loop xs2

-- gatherWith :: (c -> Gather [a] b) -> [c] -> [a] -> [b]
-- gatherWith f = loop where
--     loop [] _ = []
--     loop _ [] = []
--     loop (c:cs) as = let (b, as') = f c as
--                      in b : loop cs as'

-- | Gather (/k/, /v/) to 'Map.Map' /k/ [/v/].
--
--   >>> gatherToMap [(1 :: Int, 'a'), (2, 'b'), (1, 'c')]
--   fromList [(1,"ca"),(2,"b")]
--
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

-- | Gather values of duplicable associations.
--
--   >>> gatherToAssoc [(1 :: Int, 'a'), (2, 'b'), (1, 'c')]
--   [(1,"ca"), (2,"b")]
--
gatherToAssoc :: (Ord k) => [(k,v)] -> [(k, [v])]
gatherToAssoc = Map.assocs . gatherToMap
