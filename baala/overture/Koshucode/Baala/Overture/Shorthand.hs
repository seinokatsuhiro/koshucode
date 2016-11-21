{-# OPTIONS_GHC -Wall #-}

-- | Shorthand functions.

module Koshucode.Baala.Overture.Shorthand
 ( (&),
   int, integer,
   nothing,
 ) where

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
(&) :: a -> b -> (a, b)
a & b = (a, b)

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
int :: O.Map Int
{-# INLINE int #-}
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
integer :: O.Map Integer
{-# INLINE integer #-}
integer = id

-- | Function which always returns 'Nothing'.
--
--   >>> nothing True :: Maybe String
--   Nothing
--
nothing :: a -> Maybe b
nothing _ = Nothing
