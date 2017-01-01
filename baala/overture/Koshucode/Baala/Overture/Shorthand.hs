{-# OPTIONS_GHC -Wall #-}

-- | Shorthand types and functions.

module Koshucode.Baala.Overture.Shorthand
 ( -- * Derived types
   Pair, Twin,

   -- * Infix operators
   (&),
   (++), (<++>),
   (<?>),
   (<$$>),
   (<#>), (<#!>),

   -- * Functions
   ordEq, compareOn,
   int, integer,
   nothing,
 ) where

import Prelude hiding ((++))
import qualified Data.Maybe                    as May
import qualified Koshucode.Baala.Overture.Type as O


-- ============================================  Types

-- | Pair of /a/ and /b/.
type Pair a b = (a, b)

-- | Pair of /a/ and /a/.
type Twin a = (a, a)


-- ============================================  Infix operators

infixr 0 &

-- | Pairing operator.
--
--   >>> "a" & "b"
--   ("a", "b")
--
--   >>> "a" & "b" & "c"
--   ("a", ("b", "c"))
--
{-# INLINE (&) #-}
(&) :: a -> b -> (a, b)
a & b = (a, b)

infixr 6 ++

-- | Associative operator of monoid,
--   same as 'mappend' or infix '<>'.
--
--   >>> Just "abc" ++ Nothing ++ Just "def"
--   Just "abcdef"
--
{-# INLINE (++) #-}
(++) :: (Monoid a) => a -> a -> a
(++) = mappend

-- | Same as 'concatMap'.
--
--   >>> words <++> ["foo bar", "baz quux"]
--   ["foo","bar","baz","quux"]
--
{-# INLINE (<++>) #-}
(<++>) :: (a -> [b]) -> [a] -> [b]
(<++>) = concatMap

-- | Same as 'May.mapMaybe'.
(<?>) :: (a -> Maybe b) -> [a] -> [b]
(<?>) = May.mapMaybe

infixl 4 <$$>

-- | Double fmap.
--
--   >>> length <$$> [("a", "apple"), ("b", "banana"), ("c", "cocoa")]
--   [("a",5), ("b",6), ("c",5)]
--
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
f <$$> x = fmap f <$> x

-- | Monadic mapping, same as 'mapM'.
{-# INLINE (<#>) #-}
(<#>) :: (Monad m, Traversable t) => (a -> m b) -> t a -> m (t b)
(<#>) = mapM

-- | Execution by monadic mapping, same as 'mapM_'.
{-# INLINE (<#!>) #-}
(<#!>) :: (Monad m, Traversable t) => (a -> m b) -> t a -> m ()
(<#!>) = mapM_


-- ============================================  Function

-- | Test equality using 'Ord' method.
--   This function can be used for implementing 'Eq' instance.
--
--   > instance Eq X where
--   >   (==) = ordEq
--
ordEq :: (Ord a) => a -> a -> Bool
ordEq x y = (x `compare` y) == EQ

-- | @compareOn@ /f/ /x/ /y/ comapres /f x/ and /f y/
--   instead of /x/ and /y/.
--
--   >>> compareOn length "foo" "bar"
--   EQ
--
compareOn :: (Ord b) => (a -> b) -> a -> a -> Ordering
compareOn f x y = f x `compare` f y

-- | 'Int' shorthand.
--
--   >>> int 12
--   12
--
--   This is same as:
--
--   >>> 12 :: Int
--   12
--
{-# INLINE int #-}
int :: O.Map Int
int = id

-- | 'Integer' shorthand.
--
--   >>> integer 12
--   12
--
--   This is same as:
--
--   >>> 12 :: Integer
--   12
--
{-# INLINE integer #-}
integer :: O.Map Integer
integer = id

-- | Function which always returns 'Nothing'.
--
--   >>> nothing True :: Maybe String
--   Nothing
--
nothing :: a -> Maybe b
nothing _ = Nothing
