{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

-- | Miscellaneous functions.

module Koshucode.Baala.Overture.Misc
 ( IOPath, GetIOPath (..),
   keepOn, omitOn,
   zero,
   ints,
   printList,
   truncateString,
 ) where

import qualified Koshucode.Baala.Overture.Type as O


-- ---------------------------------  I/O path

-- | URI, file path, etc.
type IOPath = String

-- | Type which has I/O path.
class GetIOPath a where
    getIOPath :: a -> IOPath

instance GetIOPath String where
    getIOPath = id


-- ---------------------------------

-- | Keep elements.
keepOn :: (a -> Maybe b) -> O.Test b -> O.Map [a]
keepOn get f = loop where
    loop (x@(get -> Just x') : xs)
        | f x'       = x : loop xs
        | otherwise  = loop xs
    loop (_ : xs)    = loop xs
    loop []          = []

-- | Omit elements.
omitOn :: (a -> Maybe b) -> O.Test b -> O.Map [a]
omitOn get f = keepOn get (not . f)

-- | 0 of type 'Int'.
zero :: Int
zero = 0

-- | Inteeger list start with given integer.
--
--   >>> take 4 $ ints 1
--   [1,2,3,4]
--
ints :: Int -> [Int]
ints n = [n ..]

-- | Print list.
--
--   >>> printList ["foo", "bar", "baz"]
--   "foo"
--   "bar"
--   "baz"
--
printList :: (Show a) => [a] -> IO ()
printList = mapM_ print

-- | Truncate end of long string.
--
--   >>> truncateString 10 "abcdefg"
--   "abcdefg"
--
--   >>> truncateString 10 "abcdefg hijklmn"
--   "abcdefg..."
--
truncateString :: Int -> O.StringMap
truncateString = truncateStringWith "..."

truncateStringWith :: String -> Int -> O.StringMap
truncateStringWith ellipsis size s
    | length s > size  = take (size - 3) s ++ ellipsis
    | otherwise        = s

