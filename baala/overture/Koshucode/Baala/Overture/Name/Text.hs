{-# OPTIONS_GHC -Wall #-}

-- | Text name.

module Koshucode.Baala.Overture.Name.Text
 ( Name,
   GetName (..),
 ) where

import Data.Text

-- | Text name.
type Name = Text

-- | Class for types which has name.
class GetName a where
    getName :: a -> Name

