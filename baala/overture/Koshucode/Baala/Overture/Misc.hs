{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Miscellaneous functions.

module Koshucode.Baala.Overture.Misc
 ( IOPath,
   GetIOPath (..),
   ints,
   printList,
 ) where

-- | URI, file path, etc.
type IOPath = String

-- | Type which has I/O path.
class GetIOPath a where
    getIOPath :: a -> IOPath

instance GetIOPath String where
    getIOPath = id

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
