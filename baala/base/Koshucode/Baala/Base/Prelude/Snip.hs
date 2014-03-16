{-# OPTIONS_GHC -Wall #-}

-- | Snipping elements.

module Koshucode.Baala.Base.Prelude.Snip
( -- * Type
  Snip,

  -- * Function
  snipIndex,
  snipFrom,
  snipOff,
  snipFore,

  -- * Example
  -- $Example
) where

import qualified Data.List as List
import qualified Koshucode.Baala.Base.Prelude.Class as B

type Snip a = [Int] -> B.Map [a]

-- | Indices of shared elements.
snipIndex :: (Eq a) => [a] -> [a] -> [Int]
snipIndex ks vs = loop ks where
    loop [] = []
    loop (k:ks2) = case List.elemIndex k vs of
                    Just p  ->  p : loop ks2
                    Nothing ->      loop ks2

-- | Pick up indexed elements.
snipFrom :: Snip a
snipFrom ps xs = loop ps xs 0 where
    loop pps@(p:ps2) (x:xs2) i =
        case compare p i of
          EQ             -> x : loop ps2 xs2 (i + 1)
          GT             ->     loop pps xs2 (i + 1)
          LT | p >= 0    -> snipFrom pps xs  -- restart
             | otherwise -> snipFrom ps2 xs  -- ignore minus index
    loop pps@(p:ps2) [] i
        | p < i     = snipFrom pps xs  -- restart
        | otherwise = snipFrom ps2 xs  -- ignore large index
    loop [] _ _ = []

-- Simple implementation
-- snipFrom :: [Int] -> Map [a]
-- snipFrom ps xs = map (xs !!) ps

-- | Cut off indexed elements.
snipOff :: Snip a
snipOff ps xs = loop 0 xs where
    loop _ [] = []
    loop p (x:xs2)
        | p `elem` ps  =     loop (p + 1) xs2
        | otherwise    = x : loop (p + 1) xs2

-- | Move indexed elements to the front.
snipFore :: Snip a
snipFore ps xs = snipFrom ps xs ++ snipOff ps xs


-- --------------------------------------------
-- $Example
--
--  Obtain indicies for snipping @\"bdk\"@ from @\"abcdefg\"@.
--  The element @\"k\"@ is ignored because it is not in  @\"abcdefg\"@.
--
--  >>> snipIndex "bdk" "abcdefg"
--  [1,3]
--
--  Pick up same-position elements.
--
--  >>> [1,3] `snipFrom` "abcdefg"
--  "bd"
--  >>> [1,3] `snipFrom` "ABCDEFG"
--  "BD"
--
--  Cut off same-position elements.
--
--  >>> (snipIndex "ce" "abcdefg") `snipOff` "ABCDEFG"
--  "ABDFG"
--
--  Move snipping elements to the front.
--
--  >>> (snipIndex "cd" "abcdefg") `snipFore` "ABCDEFG"
--  "CDABEFG"

