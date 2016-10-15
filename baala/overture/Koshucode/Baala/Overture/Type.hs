{-# OPTIONS_GHC -Wall #-}

-- | Derivied types.

module Koshucode.Baala.Overture.Type
 ( Eith,
   uneith,
   Map, ManyMap,
   Test, Test2,
 ) where

-- | Homotype 'Either'.
type Eith a = Either a a

-- | Extract 'Eith' content.
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

