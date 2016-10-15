{-# OPTIONS_GHC -Wall #-}

-- | Derivied types.

module Koshucode.Baala.Overture.Type
 ( Test, Test2,
 ) where

-- | Boolean-valued function, also called predicate.
type Test a = a -> Bool

-- | Boolean-valued function with 2 arguments.
type Test2 a b = a -> b -> Bool

