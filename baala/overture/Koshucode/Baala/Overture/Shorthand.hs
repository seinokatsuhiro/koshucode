{-# OPTIONS_GHC -Wall #-}

-- | Shorthand functions.

module Koshucode.Baala.Overture.Shorthand
 ( -- * Infix operators
   (&),
   (++),
   (<$$>),
   (<#>), (<#!>),

   -- * Functions
   int, integer,
   nothing,
 ) where

import Prelude hiding ((++))
import qualified Koshucode.Baala.Overture.Type as O

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
