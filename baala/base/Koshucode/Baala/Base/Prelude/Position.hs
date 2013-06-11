{-# OPTIONS_GHC -Wall #-}

-- | Position in list.

module Koshucode.Baala.Base.Prelude.Position
( ap, like,
  --allIndex,
  sharedIndex,
  indexPick, indexCut
) where
import qualified Data.List as List

-- allIndex :: (Eq a) => [a] -> [a] -> [Int]
-- allIndex ks vs = loop ks where
--     loop [] = []
--     loop (k:ks2) = case List.elemIndex k vs of
--                      Just p  ->  p : loop ks2
--                      Nothing -> -1 : loop ks2

-- words "b d g" `index` words "a b c d e" == [("b",1),("d",3),("g",-1)]
-- index :: (Eq a) => [a] -> [a] -> [(a,Int)]
-- index ks vs = loop ks where
--     loop [] = []
--     loop (k:ks2) = case List.elemIndex k vs of
--                     Just p  -> (k,p)  : loop ks2
--                     Nothing -> (k,-1) : loop ks2

-- | Indices of shared elements.
-- 
--   >>> "bdk" `sharedIndex` "abcdefg"
--   [1,3]

sharedIndex :: (Eq a) => [a] -> [a] -> [Int]
sharedIndex ks vs = loop ks where
    loop [] = []
    loop (k:ks2) = case List.elemIndex k vs of
                    Just p  ->  p : loop ks2
                    Nothing ->      loop ks2

-- | Pick indexed elements.
-- 
--   >>> indexPick [1,3] "abcdefg"
--   "bd"

indexPick :: [Int] -> [a] -> [a]
indexPick ps xs = loop ps xs 0 where
    loop pps@(p:ps2) (x:xs2) i =
        case compare p i of
          EQ -> x : loop ps2 xs2 (i + 1)
          GT ->     loop pps xs2 (i + 1)
          LT | p >= 0    -> indexPick pps xs  -- restart
             | otherwise -> indexPick ps2 xs  -- ignore minus index
    loop pps@(p:ps2) [] i
        | p < i     = indexPick pps xs  -- restart
        | otherwise = indexPick ps2 xs  -- ignore large index
    loop [] _ _ = []

-- Simple implementation
-- indexPick ps xs = map (xs !!) ps

-- | Cut indexed elements
-- 
--   >>> indexCut [1,3] "abcdefg"
--   "acefg"

indexCut :: [Int] -> [a] -> [a]
indexCut ps xs = loop 0 xs where
    loop _ [] = []
    loop p (x:xs2)
        | p `elem` ps  =     loop (p + 1) xs2
        | otherwise    = x : loop (p + 1) xs2

-- e0 p x = (indexPick p x, indexCut p x)
-- e1 = e0 [1,3] "abcdefg"
-- e2 = e0 [5,4,1] "abcdefg"
-- e3 = e0 [1,2,3] "abcdefg"
-- e4 = e0 [0,1] "ab"
-- e5 = e0 [1,0] "ab"
-- e6 = e0 [0,5] "ab"
-- e7 = e0 [0] "ab"
-- e8 = e0 [1] "ab"
-- e9 = e0 [2] "ab"

-- grip :: [Int] -> [a] -> ([a], [a])
-- grip ps xs = (take ps xs, xs)

-- split :: [Int] -> [a] -> ([a], [a])
-- split ps xs = loop 0 xs where
--     loop _ [] = ([], [])
--     loop p (x:xs2)
--         | p `elem` ps = mapFst (x:) $ loop (p + 1) xs2
--         | otherwise   = mapSnd (x:) $ loop (p + 1) xs2

{-# ANN like "HLint: ignore Use String" #-}
like :: [Int] -> [Char] -> Bool
like xs cs = loop xs cs where
    loop (x:xs2) (c:cs2)
        | c /= '-' && x >= 0   = loop xs2 cs2
        | c == '-' && x <  0   = loop xs2 cs2
        | c == '?'             = loop xs2 cs2
        | otherwise            = False
    loop [] [] = True
    loop _  _  = False

ap :: ((Int -> v) -> [v] -> a) -> [v] -> a
ap f arg = f (arg !!) arg

