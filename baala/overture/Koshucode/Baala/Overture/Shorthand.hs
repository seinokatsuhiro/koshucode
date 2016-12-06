{-# OPTIONS_GHC -Wall #-}

-- | Shorthand functions.

module Koshucode.Baala.Overture.Shorthand
 ( (&),
   (++),
   (<$$>),
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
{-# INLINE (++) #-}
(++) :: (Monoid a) => a -> a -> a
(++) = mappend

infixl 4 <$$>

-- | Double fmap.
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
f <$$> x = fmap f <$> x

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
