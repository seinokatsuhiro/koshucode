{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall #-}

-- | Shorthand types and functions.

module Koshucode.Baala.Overture.Shorthand
 ( -- * Derived types
   Pair, Twin,
   Bs, Bz,
   pattern Jp,
   pattern Jp2,
   pattern Jp3,

   -- * Functions
   ordEq, compareOn,
   int, integer,
   nothing,
 ) where

import qualified Data.ByteString                as Bs
import qualified Data.ByteString.Lazy           as Bz
import qualified Koshucode.Baala.Overture.Type  as O


-- ============================================  Types

-- | Nominal pair type.
type Pair a b = (a, b)

-- | Twin pair.
type Twin a = (a, a)

-- | Strict bytestring.
type Bs = Bs.ByteString

-- | Lazy bytestring.
type Bz = Bz.ByteString

-- | Just pair: __Jp /a b/__ is equal to __Just (/a/, /b/)__.
pattern Jp a b = Just (a, b)

-- | Double just pairs: __Jp2 /a b c/__ is equal to __Just (/a/, Just (/b/, /c/))__.
pattern Jp2 a b c = Just (a, Just (b, c))

-- | Triple just pairs: __Jp3 /a b c d/__ is equal to __Just (/a/, Just (/b/, Just (/c/, /d/)))__.
pattern Jp3 a b c d = Just (a, Just (b, Just (c, d)))


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
