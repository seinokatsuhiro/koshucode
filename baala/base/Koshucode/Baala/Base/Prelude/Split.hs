{-# OPTIONS_GHC -Wall #-}

-- | Split lists.

module Koshucode.Baala.Base.Prelude.Split
  ( -- * Type
    Split,
    Split2, SplitList2,
    Split3', SplitList3',
    Split3, SplitList3,

    -- * Function
    chunks,
    splitBy,
    divide, divideBy,
    wordsBy,
  ) where

-- --------------------------------------------  Type

-- | Split list into before-part, split-element and after-part (@Right@),
--   or original list if not splittable (@Left@).
type Split a = [a] -> Either [a] ([a], a, [a])

-- | Split elements into two subelements.
type Split2 t a = t a -> Maybe (t a, t a)

-- | Split list into two sublists.
type SplitList2 a = Split2 [] a

-- | Split elements into before-elem-after parts.
type Split3' t a = t a -> Maybe (t a, a, t a)

-- | Split list into before-elem-after parts.
type SplitList3' a = Split3' [] a

-- | Split elements into three subelements.
type Split3 t a = t a -> Maybe (t a, t a, t a)

-- | Split list into three sublists.
type SplitList3 a = Split3 [] a


-- --------------------------------------------  Function

chunks :: Int -> [a] -> [[a]]
chunks n = loop where
    loop xs = case splitAt n xs of
                ([], _)      -> []
                (taked, xs2) -> taked : loop xs2

-- | Split list by predicate.
--
--   >>> splitBy (== '|') "b c"
--   Left "b c"
--
--   >>> splitBy (== '|') "a | b | c"
--   Right ("a ", '|', " b | c")

splitBy :: (a -> Bool) -> Split a
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

-- | Divide string into words.
--
--   >>> wordsBy (== '|') "a|bb||ccc|"
--   ["a","bb","ccc"]

wordsBy :: (a -> Bool) -> [a] -> [[a]]
wordsBy p = loop where
    loop s = case dropWhile p s of
               [] -> []
               s1 -> let (w, s2) = break p s1
                     in w : loop s2
