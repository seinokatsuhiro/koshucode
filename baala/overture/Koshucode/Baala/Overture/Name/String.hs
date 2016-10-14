{-# OPTIONS_GHC -Wall #-}

-- | String name.

module Koshucode.Baala.Overture.Name.String
 ( Name,
   GetName (..),
 ) where

-- | String name.
type Name = String

-- | Class for types which has name.
class GetName a where
    getName :: a -> Name

