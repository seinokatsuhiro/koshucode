{-# OPTIONS_GHC -Wall #-}

-- | Split lists.

module Koshucode.Baala.Base.Prelude.Split
  ( -- * Type
    Split1, SplitList1,
    Split2, SplitList2,
    Split3e, SplitList3e,
    Split3, SplitList3,

    -- * Split
    dropSubBy, dropSub,
    splitSubBy, splitSub,
    splitBy,

    -- * Divide
    chunks,
    divide, divideBy, wordsBy,
  ) where


-- --------------------------------------------  Type

-- | Split elements into subelements.
type Split1 t a = t a -> Maybe (t a)

-- | Split list into sublist.
type SplitList1 a = Split1 [] a

-- | Split elements into two subelements.
type Split2 t a = t a -> Maybe (t a, t a)

-- | Split list into two sublists.
type SplitList2 a = Split2 [] a

-- | Split elements into before-elem-after parts.
type Split3e t a = t a -> Maybe (t a, a, t a)

-- | Split list into before-elem-after parts.
type SplitList3e a = Split3e [] a

-- | Split list into before-part, split-element and after-part (@Right@),
--   or original list if not splittable (@Left@).
type Split3eEither a = [a] -> Either [a] ([a], a, [a])

-- | Split elements into three subelements.
type Split3 t a = t a -> Maybe (t a, t a, t a)

-- | Split list into three sublists.
type SplitList3 a = Split3 [] a


-- --------------------------------------------  Split

-- | Drop sublist.
dropSubBy :: SplitList2 a -> SplitList1 a
dropSubBy p s = case p s of
                  Just (_, b) -> Just b
                  Nothing     -> Nothing

-- | Drop prefix part from a list.
--   This function is similar to 'Data.List.stripPrefix'.
--
--   >>> dropSub "abc" "abcdefghi"
--   Just "defghi"
--
--   >>> dropSub "" "abcdefghi"
--   Just "abcdefghi"
--
--   >>> dropSub "xyz" "abcdefghi"
--   Nothing

dropSub :: (Eq a) => [a] -> SplitList1 a
dropSub sub = dropSubBy p where
    n = length sub
    p xs | take n xs == sub  = Just (sub, drop n xs)
         | otherwise         = Nothing

-- | Split list by given predicate.
splitSubBy :: SplitList2 a -> SplitList3 a
splitSubBy p = loop [] where
    loop _ [] = Nothing
    loop before xxs@(x : xs) =
        case p xxs of
          Nothing            -> loop (x : before) xs
          Just (mid, after)  -> Just (reverse before, mid, after)

-- | Split list by given sublist.
--
--   >>> splitSub "def" "abcdefghi"
--   Just ("abc","def","ghi")
--
--   >>> splitSub "" "abcdefghi"
--  Just ("","","abcdefghi")

splitSub :: (Eq a) => [a] -> SplitList3 a
splitSub sub = splitSubBy p where
    n = length sub
    p xs | take n xs == sub  = Just (sub, drop n xs)
         | otherwise         = Nothing

-- | Split list by predicate.
--
--   >>> splitBy (== '|') "b c"
--   Left "b c"
--
--   >>> splitBy (== '|') "a | b | c"
--   Right ("a ", '|', " b | c")

splitBy :: (a -> Bool) -> SplitList3e a
splitBy p xs =
    case break p xs of
      (a, x : b) -> Just (a, x, b)
      _          -> Nothing


-- --------------------------------------------  Divide

-- | Divide list into sublists of given length.
--
--   >>> chunks 2 "abcdefg"
--   ["ab","cd","ef","g"]

chunks :: Int -> [a] -> [[a]]
chunks n = loop where
    loop xs = case splitAt n xs of
                ([], _)      -> []
                (taked, xs2) -> taked : loop xs2

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
