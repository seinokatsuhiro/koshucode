{-# OPTIONS_GHC -Wall #-}

-- | Integer or string name.

module Koshucode.Baala.Overture.Name.IntString
 ( Name (..),
   GetName (..),
 ) where

-- | String or integer name.
data Name
    = NameI Int
    | NameS String 
      deriving (Show, Eq, Ord)

-- | Class for types which has name.
class GetName a where
    getName :: a -> Name

