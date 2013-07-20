{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Base.Content.Class
(
  -- * Primitive content
  PrimContent (),

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

  -- * Convinient class
  CContent (..),
) where

import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Base.Data



-- ----------------------  Primitive content

class PrimContent a



-- ----------------------  Haskell built-in data

class (PrimContent v) => CBool v where
    putBool    ::   Bool -> v
    isBool     ::      v -> Bool

class (PrimContent v) => CInt v where
    putInt     ::    Int -> v
    isInt      ::      v -> Bool

class (PrimContent v) => CString v where
    putString  :: String -> v
    isString   ::      v -> Bool
    getString  ::      v -> String

class (PrimContent v) => CList v where
    putList    ::    [v] -> v
    isList     ::      v -> Bool
    getList    ::      v -> [v]



-- ----------------------  Data in koshucode

{-| Types that can be nil -}
class (PrimContent v) => CNil v where
    nil         ::          v
    isNil       ::          v -> Bool

class (PrimContent v) => CSet v where
    putSet      ::        [v] -> v
    isSet       ::          v -> Bool

class (PrimContent v) => CTermset v where
    putTermset  :: [Named v] -> v
    isTermset   ::         v -> Bool

class (PrimContent v) => CRel v where
    putRel      ::     Rel v -> v
    getRel      ::         v -> Rel v
    isRel       ::         v -> Bool



-- ----------------------  Convinient class

class (Ord v, Pretty v, CNil v,
       CBool v, CString v, CInt v,
       CList v, CSet v, CTermset v,
       CRel v) => CContent v where
    appendContent :: v -> v -> v
    joinContent :: [v] -> v
    joinContent = foldr appendContent nil

