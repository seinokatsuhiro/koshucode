{-# OPTIONS_GHC -Wall #-}

-- | Shorthand types and functions.

module Koshucode.Baala.Overture.Shorthand
 ( -- * Derived types
   Pair, Twin,

   -- * Functions
   ordEq, compareOn,
   int, integer,
   nothing,
 ) where

import qualified Koshucode.Baala.Overture.Type as O


-- ============================================  Types

-- | Pair of /a/ and /b/.
type Pair a b = (a, b)

-- | Pair of /a/ and /a/.
type Twin a = (a, a)


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
