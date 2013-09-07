{-# OPTIONS_GHC -Wall #-}

{- | Arrangement of lists. -}

module Koshucode.Baala.Base.Prelude.Arrange
( Arrange,
  sharedIndex,
  arrangePick,
  arrangeCut,
  arrangeFore,
) where

import qualified Data.List as List
import qualified Koshucode.Baala.Base.Prelude.Class as B

type Arrange a = [Int] -> B.Map [a]

{-| Indices of shared elements.

    >>> "bdk" `sharedIndex` "abcdefg"
    [1,3]  -}
sharedIndex :: (Eq a) => [a] -> [a] -> [Int]
sharedIndex ks vs = loop ks where
    loop [] = []
    loop (k:ks2) = case List.elemIndex k vs of
                    Just p  ->  p : loop ks2
                    Nothing ->      loop ks2

{-| Pick indexed elements.

    >>> arrangePick [1,3] "abcdefg"
    "bd"  -}
arrangePick :: Arrange a
arrangePick ps xs = loop ps xs 0 where
    loop pps@(p:ps2) (x:xs2) i =
        case compare p i of
          EQ -> x : loop ps2 xs2 (i + 1)
          GT ->     loop pps xs2 (i + 1)
          LT | p >= 0    -> arrangePick pps xs  -- restart
             | otherwise -> arrangePick ps2 xs  -- ignore minus index
    loop pps@(p:ps2) [] i
        | p < i     = arrangePick pps xs  -- restart
        | otherwise = arrangePick ps2 xs  -- ignore large index
    loop [] _ _ = []

-- Simple implementation
-- arrangePick :: [Int] -> Map [a]
-- arrangePick ps xs = map (xs !!) ps

{-| Cut indexed elements

    >>> arrangeCut [1,3] "abcdefg"
    "acefg"  -}
arrangeCut :: [Int] -> B.Map [a]
arrangeCut ps xs = loop 0 xs where
    loop _ [] = []
    loop p (x:xs2)
        | p `elem` ps  =     loop (p + 1) xs2
        | otherwise    = x : loop (p + 1) xs2

{-| Move indexed elements to the front.

    >>> arrangeFore [1,3] "abcdefg"
    "bdacefg"  -}
arrangeFore :: [Int] -> B.Map [a]
arrangeFore ps xs = arrangePick ps xs ++ arrangeCut ps xs

-- grip :: [Int] -> [a] -> ([a], [a])
-- grip ps xs = (take ps xs, xs)

-- split :: [Int] -> [a] -> ([a], [a])
-- split ps xs = loop 0 xs where
--     loop _ [] = ([], [])
--     loop p (x:xs2)
--         | p `elem` ps = mapFst (x:) $ loop (p + 1) xs2
--         | otherwise   = mapSnd (x:) $ loop (p + 1) xs2

-- like :: [Int] -> [Char] -> Bool
-- like xs cs = loop xs cs where
--     loop (x:xs2) (c:cs2)
--         | c /= '-' && x >= 0   = loop xs2 cs2
--         | c == '-' && x <  0   = loop xs2 cs2
--         | c == '?'             = loop xs2 cs2
--         | otherwise            = False
--     loop [] [] = True
--     loop _  _  = False

-- ap :: ((Int -> v) -> [v] -> a) -> [v] -> a
-- ap f arg = f (arg !!) arg

