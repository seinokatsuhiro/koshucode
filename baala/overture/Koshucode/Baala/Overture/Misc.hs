{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | Miscellaneous functions.

module Koshucode.Baala.Overture.Misc
 ( IOPath, GetIOPath (..),
   OrElse (..),
   uncons,
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

-- ---------------------------------  Or else

-- Same as ||
infixr 2 <||>
infixr 2 `orElse`

-- | Or-else pattern.
--   This is similar to 'Control.Applicative.Alternative'.
class OrElse a where
    orElse :: a -> a -> a

    (<||>) :: (OrElse a) => a -> a -> a
    (<||>) = orElse

-- | False or something else.
--
--   >>> False <||> True <||> True
--   True
--
instance OrElse Bool where
    orElse False a  = a
    orElse a _      = a

-- | Empty list or something else.
--
--   >>> "" <||> "foo" <||> "bar"
--   "foo"
--
instance OrElse [a] where
    orElse [] a  = a
    orElse a _   = a

-- | Nothing or something else.
--
--   >>> Nothing <||> Just "foo" <||> Just "bar"
--   Just "foo"
--
instance OrElse (Maybe a) where
    orElse Nothing a  = a
    orElse a _        = a

-- | Left or something else.
--
--   >>> Left "foo" <||> Right "bar" <||> Right "baz"
--   Right "bar"
--
instance OrElse (Either a b) where
    orElse (Left _) a  = a
    orElse a _         = a

-- ---------------------------------

-- | Head and tail of list.
--   This is a list version of text 'Data.Text.uncons'.
--
--   >>> uncons "abcdefg"
--   Just ('a', "bcdefg")
--
{-# INLINE uncons #-}
uncons :: [a] -> Maybe (a, [a])
uncons (x : xs)  = Just (x, xs)
uncons []        = Nothing

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

