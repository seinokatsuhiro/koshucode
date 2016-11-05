{-# OPTIONS_GHC -Wall #-}

-- | Derivied types.

module Koshucode.Baala.Overture.Type
 ( Eith,
   uneith,
   Map, ManyMap,
   Test, Test2,
   Bin,
   int, integer,
 ) where

-- | Homotype 'Either'.
type Eith a = Either a a

-- | Extract 'Eith' content.
--
--   >>> uneith <$> [Left "L", Right "R"]
--   ["L", "R"]
--
uneith :: Eith a -> a
uneith (Left x)   = x
uneith (Right x)  = x

-- | Map from something to same type.
type Map a = a -> a

-- | Map from something to list of something.
type ManyMap a = a -> [a]

-- | Boolean-valued function, also called predicate.
type Test a = a -> Bool

-- | Boolean-valued function with 2 arguments.
type Test2 a b = a -> b -> Bool

-- | Type for binary operators.
type Bin a = a -> a -> a

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
int :: Map Int
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
integer :: Map Integer
{-# INLINE integer #-}
integer = id
