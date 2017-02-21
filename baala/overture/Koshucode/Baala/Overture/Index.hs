{-# OPTIONS_GHC -Wall #-}

-- | Indexed value.

module Koshucode.Baala.Overture.Index
 ( Ix, Iz, GetIx (..),
   Index, GetIndex (..),
   lookupIx, lookupIz,
 ) where

-- | Numerical index.
type Ix = Int

-- | Zero-origin index.
type Iz = Int

-- | 'Int'-indexed type.
class GetIx a where
    getIx :: a -> Ix

-- | Numerical index.
type Index = Int

-- | 'Integer'-indexed type.
class GetIndex a where
    getIndex :: a -> Index

{-| Lookup list by one-origin index.

    === __Examples__

    >>> lookupIx 3 "abcdefg"
    Just 'c'

    >>> lookupIx 10 "abcdefg"
    Nothing

    >>> lookupIx 0 "abcdefg"
    Nothing
    -}
lookupIx :: Ix -> [a] -> Maybe a
lookupIx = loop where
    loop 1 (x : _)   = Just x
    loop i (_ : xs)  = loop (i - 1) xs
    loop _ _         = Nothing

{-| Lookup list by zero-origin index.

    === __Examples__

    >>> lookupIz 3 "abcdefg"
    Just 'd'

    >>> lookupIz 10 "abcdefg"
    Nothing
    -}
lookupIz :: Iz -> [a] -> Maybe a
lookupIz = loop where
    loop 0 (x : _)   = Just x
    loop i (_ : xs)  = loop (i - 1) xs
    loop _ _         = Nothing
