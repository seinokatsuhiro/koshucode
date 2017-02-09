{-# LANGUAGE ViewPatterns #-}
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

import qualified Koshucode.Baala.Overture as O

-- | Type of replace function, /from/ → /to/ → /xs/ → /ys/.
type Replace t = t -> t -> t -> t

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

replaceAll :: (O.Textual t) => Replace t
replaceAll from to = loop where
    loop xxs@(O.tCut -> O.Jp x xs)
        | xxs =* from   = to O.++ loop (dropLength from xxs)
        | otherwise     = x O.<:> loop xs
    loop xs = xs

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

replaceFirst :: (O.Textual t) => Replace t
replaceFirst from to = loop where
    loop xxs@(O.tCut -> O.Jp x xs)
        | xxs =* from   = to O.++ dropLength from xxs
        | otherwise     = x O.<:> loop xs
    loop xs = xs

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

replaceLast :: (O.Textual t) => Replace t
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

replaceBegin :: (O.Textual t) => Replace t
replaceBegin from to xs
    | xs =* from   = to O.++ dropLength from xs
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

replaceEnd :: (O.Textual t) => Replace t
replaceEnd = replaceReverse replaceBegin


-- ----------------------  Utility

(=*) :: (O.Textual t) => t -> t -> Bool
xs =* from
   | O.tIsEmpty from = False
   | otherwise       = O.csIsPrefix from xs

dropLength :: (O.Textual t) => t -> t -> t
dropLength = O.tDrop . O.tLength

replaceReverse :: (O.Textual t) => Replace t -> Replace t
replaceReverse rep from to xs = O.csReverse $ rep from' to' xs' where
    from' = O.csReverse from
    to'   = O.csReverse to
    xs'   = O.csReverse xs

