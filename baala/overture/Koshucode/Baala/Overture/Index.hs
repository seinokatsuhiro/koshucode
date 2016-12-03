{-# OPTIONS_GHC -Wall #-}

-- | Indexed value.

module Koshucode.Baala.Overture.Index
 ( Ix, GetIx (..),
   Index, GetIndex (..),
 ) where

-- | Numerical index.
type Ix = Int

-- | 'Int'-indexed type.
class GetIx a where
    getIx :: a -> Ix

-- | Numerical index.
type Index = Int

-- | 'Integer'-indexed type.
class GetIndex a where
    getIndex :: a -> Index

