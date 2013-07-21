{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Content.Class
(
  -- * Generic content
  PrimContent (),
  CContent (..),

  -- * Haskell data
  CBool (..),
  CInt (..),
  CString (..),
  CList (..),

  -- * Koshu data
  CNil (..),
  CSet (..),
  CTermset (..),
  CRel (..),
) where

import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Base.Data



-- ----------------------  Primitive content

class PrimContent c

class (Ord c, Pretty c, 
       CBool c, CString c, CInt c, CList c,
       CNil c , CSet c, CTermset c, CRel c) =>
    CContent c where
    appendContent :: c -> c -> c
    joinContent :: [c] -> c
    joinContent = foldr appendContent nil



-- ----------------------  Haskell built-in data

class (PrimContent c) => CBool c where
    putBool    ::   Bool -> c
    getBool    ::      c -> Bool
    isBool     ::      c -> Bool

class (PrimContent c) => CInt c where
    putInt     ::    Int -> c
    getInt     ::      c -> Int
    isInt      ::      c -> Bool

class (PrimContent c) => CString c where
    putString  :: String -> c
    getString  ::      c -> String
    isString   ::      c -> Bool

class (PrimContent c) => CList c where
    putList    ::    [c] -> c
    getList    ::      c -> [c]
    isList     ::      c -> Bool



-- ----------------------  Data in koshucode

{-| Types that can be nil -}
class (PrimContent c) => CNil c where
    nil         ::          c
    isNil       ::          c -> Bool

class (PrimContent c) => CSet c where
    putSet      ::        [c] -> c
    getSet      ::          c -> [c]
    isSet       ::          c -> Bool

class (PrimContent c) => CTermset c where
    putTermset  :: [Named c] -> c
    getTermset  ::         c -> [Named c]
    isTermset   ::         c -> Bool

class (PrimContent c) => CRel c where
    putRel      ::     Rel c -> c
    getRel      ::         c -> Rel c
    isRel       ::         c -> Bool

