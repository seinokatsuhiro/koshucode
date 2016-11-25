{-# OPTIONS_GHC -Wall #-}

-- | Replace sublists.

module Koshucode.Baala.Cop.Replace
  ( Replace,
    replaceAll,
    replaceFirst,
    replaceLast,
    replaceBegin,
    replaceEnd,
  ) where

import qualified Data.List  as List

-- | Type of replace function, /from/ → /to/ → /xs/ → /ys/.
type Replace a = [a] -> [a] -> [a] -> [a]

-- | Replace all occurrences.
--
-- >>> replaceAll "br" "BR" "abracadabra"
-- "aBRacadaBRa"
--
-- >>> replaceAll "ba" "BA" "abracadabra"
-- "abracadabra"
--
-- >>> replaceAll "" "BR" "abracadabra"
-- "abracadabra"

replaceAll :: (Eq a) => Replace a
replaceAll from to = loop where
    loop [] = []
    loop xxs@(x:xs)
        | xxs =* from   = to ++ loop (dropLength from xxs)
        | otherwise     = x : loop xs

-- | Replace first occurrence.
--
-- >>> replaceFirst "br" "BR" "abracadabra"
-- "aBRacadabra"
--
-- >>> replaceFirst "ba" "BA" "abracadabra"
-- "abracadabra"
--
-- >>> replaceFirst "" "BR" "abracadabra"
-- "abracadabra"

replaceFirst :: (Eq a) => Replace a
replaceFirst from to = loop where
    loop [] = []
    loop xxs@(x:xs)
        | xxs =* from   = to ++ dropLength from xxs
        | otherwise     = x : loop xs

-- | Replace last occurrence.
--
-- >>> replaceLast "br" "BR" "abracadabra"
-- "abracadaBRa"
--
-- >>> replaceLast "ba" "BA" "abracadabra"
-- "abracadabra"
--
-- >>> replaceLast "" "BR" "abracadabra"
-- "abracadabra"

replaceLast :: (Eq a) => [a] -> [a] -> [a] -> [a]
replaceLast = replaceReverse replaceFirst

-- | Replace occurrence at the beginning.
--
-- >>> replaceBegin "ab" "AB" "abracadabra"
-- "ABracadabra"
--
-- >>> replaceBegin "br" "BR" "abracadabra"
-- "abracadabra"
--
-- >>> replaceBegin "" "BR" "abracadabra"
-- "abracadabra"

replaceBegin :: (Eq a) => Replace a
replaceBegin from to xs
    | xs =* from   = to ++ dropLength from xs
    | otherwise    = xs

-- | Replace occurrence at the end.
--
-- >>> replaceEnd "ra" "RA" "abracadabra"
-- "abracadabRA"
--
-- >>> replaceEnd "br" "BR" "abracadabra"
-- "abracadabra"
--
-- >>> replaceEnd "" "BR" "abracadabra"
-- "abracadabra"

replaceEnd :: (Eq a) => Replace a
replaceEnd = replaceReverse replaceBegin


-- ----------------------  Utility

(=*) :: (Eq a) => [a] -> [a] -> Bool
_  =* []    = False
xs =* from  = from `List.isPrefixOf` xs

dropLength :: [a] -> [a] -> [a]
dropLength = drop . length

replaceReverse :: (Eq a) => Replace a -> Replace a
replaceReverse rep from to xs = reverse $ rep from' to' xs' where
    from' = reverse from
    to'   = reverse to
    xs'   = reverse xs

