{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Dots pattern.

module Koshucode.Baala.Base.List.Dots
  ( dotsStrings,
    undotsStrings,
    takeDots,
  ) where

import qualified Data.Char                           as Ch
import qualified Koshucode.Baala.Overture            as O
import qualified Koshucode.Baala.Base.List.Split     as B

pattern Dots a s = '.' : '.' : '.' : a : s

undup :: (Eq a) => a -> [a] -> [a]
undup k = loop where
    loop [] = []
    loop [x1] = [x1]
    loop (x1 : x2 : xs)
               | x1 == k && x2 == k  = loop (x2 : xs)
               | otherwise           = x1 : loop (x2 : xs)

-- | Divide dots-pattern string.
--
--   >>> dotsStrings "abc ... xyz"
--   ["abc","...","xyz"]
--
--   >>> dotsStrings "a ... h ... ... v ..."
--   ["a","...","h","...","v","..."]

dotsStrings :: String -> [String]
dotsStrings = begin where
    begin (Dots a s)
        | Ch.isSpace a     = dots (mid "" $ O.trimLeft s)
    begin s                = mid "" s

    mid w (b : Dots a s)
        | Ch.isSpace b &&
          Ch.isSpace a     = revTrim w : dots (begin $ O.trimLeft s)
    mid w (c:s)            = mid (c:w) s
    mid w ""               = end w

    end (Dots a w)
        | Ch.isSpace a     = revTrim w : dots []
    end w                  = [reverse w]

    dots pat@("..." : _)   = pat
    dots pat               = "..." : pat

    revTrim                = reverse . O.trimLeft

-- | Concatenate dots-pattern strings,
--   inserting one space between dots and word.
--
--   >>> undotsStrings ["abc", "...", "xyz"]
--   "abc ... xyz"
--
--   >>> undotsStrings ["a", "...", "h", "...", "...", "v", "..."]
--   "a ... h ... v ..."

undotsStrings :: [String] -> String
undotsStrings = loop "" . undup "..." where
    loop sp ["..."]       = sp ++ "..."
    loop sp ("..." : xs)  = sp ++ "..." ++ loop " " xs
    loop sp (x : xs)      = sp ++ x     ++ loop " " xs
    loop _ []             = ""

-- | Take dots-parts from string.
--
--   >>> takeDots ["a", "...", "h", "...", "v", "..."] "abcdefghijklmnopqrstuvwxyz"
--   Just ["bcdefg","ijklmnopqrstu","wxyz"]
--
--   >>> takeDots ["a", "...", "h", "...", "v", "..."] "abcdefg"
--   Nothing

takeDots :: [String] -> String -> Maybe [String]
takeDots = takeBy (== "...")

takeBy :: (Eq a) => ([a] -> Bool) -> [[a]] -> [a] -> Maybe [[a]]
takeBy dots = loop False where
    loop _ [] []       = Just []  -- pattern and data are matched
    loop _ []  _       = Nothing  -- data remains
    loop _ [p] s
        | dots p       = Just [s] -- dot pattern matches tail part
    loop ad (p : ps) s
        | dots p       = loop True ps s                  -- entering after-dots (ad) state
        | ad           = do (d, _, a) <- B.splitSub p s  -- dots-part and after-part
                            a' <- loop False ps a
                            Just $ d : a'
        | otherwise    = do a <- B.dropSub p s           -- drop pattern
                            loop False ps a

-- dotsStrings "...def"
-- dotsStrings "... def"
-- dotsStrings " abc "
-- dotsStrings "abc...def"
-- dotsStrings "abc ..."
-- dotsStrings "abc ... def ... hij"
-- dotsStrings "abc  ...  def"
-- dotsStrings "abc ... ... def"
-- dotsStrings "abc ...... def"

-- undotsStrings ["...", "abc"]
-- undotsStrings ["...", "abc", "...", "...", "fg"]

-- takeDots "..." ["a", "..."] "abcdefg"
-- takeDots "..." ["a", "...", "g"] "abcdefg"
-- takeDots "..." ["a", "...", "e", "...", "g"] "abcdefg"
