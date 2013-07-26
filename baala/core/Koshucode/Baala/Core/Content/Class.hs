{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Content.Class
(
  -- * Generic content
  PrimContent (),
  CContent (..),

  nonNullFilter,
  nonNilFilter,

  -- * Haskell data
  CBool    (..),
  CInt     (..),
  CText    (..),
  CList    (..),

  -- * Koshu data
  CNil     (..),
  CSet     (..),
  CTermset (..),
  CRel     (..),
) where

import Koshucode.Baala.Base.Prelude
import Koshucode.Baala.Base.Data



-- ----------------------  Generic content

class PrimContent c

class (Ord c, Pretty c, 
       CBool c, CText c, CInt c, CList c,
       CNil c , CSet c, CTermset c, CRel c) =>
    CContent c where

    appendContent :: c -> c -> c

    joinContent :: [c] -> c
    joinContent = foldr appendContent nil

    typename :: c -> String
    typename c
        | isBool    c  =  "boolean"
        | isText    c  =  "text"
        | isInt     c  =  "int"
        | isList    c  =  "list"
        | isNil     c  =  "nil"
        | isSet     c  =  "set"
        | isTermset c  =  "termset"
        | isRel     c  =  "relation"
        | otherwise    =  "unknown"

{-| Delete empty list ('null') from content list. -}
nonNullFilter :: Map [[a]]
nonNullFilter = filter (not . null)

{-| Delete 'nil' from content list. -}
nonNilFilter :: (CNil c) => Map [c]
nonNilFilter = filter (not . isNil)



-- ----------------------  Haskell built-in data

class (PrimContent c) => CBool c where
    {-| Put Boolean value into content @c@. -}
    putBool    ::   Bool -> c
    {-| Get Boolean value from content @c@. -}
    getBool    ::      c -> Bool
    {-| Test content @c@ has Boolean value. -}
    isBool     ::      c -> Bool

class (PrimContent c) => CInt c where
    putInt     ::    Int -> c
    getInt     ::      c -> Int
    isInt      ::      c -> Bool

class (PrimContent c) => CText c where
    putText    :: String -> c
    getText    ::      c -> String
    isText     ::      c -> Bool

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

