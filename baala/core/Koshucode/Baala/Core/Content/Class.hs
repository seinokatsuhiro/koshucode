{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Content.Class
(
  -- * Generic content
  PrimContent (..),
  CContent    (..),

  nonNullFilter,
  nonNilFilter,

  -- * Haskell data
  CBool       (..),
  CText       (..),
  CList       (..),

  -- * Koshu data
  CNil        (..),
  CDec        (..),
  putDecFromInt,
  CSet        (..),
  CTermset    (..),
  CRel        (..),
) where

import qualified Koshucode.Baala.Base as B
import Koshucode.Baala.Core.Content.Decimal



-- ----------------------  Generic content

class (Show c) => PrimContent c where
    typename :: c -> String

class (Ord c, B.Pretty c, PrimContent c,
       CBool c, CText c, CDec c, CList c,
       CNil c , CSet c, CTermset c, CRel c) =>
    CContent c where

    appendContent :: c -> c -> c

    joinContent :: [c] -> c
    joinContent = foldr appendContent nil

{-| Delete empty list ('null') from content list. -}
nonNullFilter :: B.Map [[a]]
nonNullFilter = filter (not . null)

{-| Delete 'nil' from content list. -}
nonNilFilter :: (CNil c) => B.Map [c]
nonNilFilter = filter (not . isNil)

need :: PrimContent c => (c -> Bool) -> (c -> b) -> c -> B.Ab b
need is get x
    | is x = Right $ get x
    | otherwise = Left $ B.AbortUnmatchType (typename x)



-- ----------------------  Haskell built-in data

class (PrimContent c) => CBool c where
    {-| Test content @c@ has Boolean value. -}
    isBool     ::       c -> Bool
    {-| Put Boolean value into content @c@. -}
    putBool    ::    Bool -> c
    {-| Get Boolean value from content @c@. -}
    getBool    ::       c -> Bool
    {-| Get Boolean value from content @c@ if @c@ is @CBool@. -}
    needBool   ::       c -> B.Ab Bool
    needBool = need isBool getBool

class (PrimContent c) => CText c where
    isText     ::       c -> Bool
    getText    ::       c -> String
    putText    ::  String -> c

    needText   ::       c -> B.Ab String
    needText = need isText getText

class (PrimContent c) => CList c where
    isList     ::       c -> Bool
    getList    ::       c -> [c]
    putList    ::     [c] -> c

    needList   ::       c -> B.Ab [c]
    needList = need isList getList



-- ----------------------  Data in koshucode

{-| Types that can be nil -}
class (PrimContent c) => CNil c where
    isNil       ::          c -> Bool
    nil         ::          c

class (PrimContent c) => CDec c where
    isDec      ::           c -> Bool
    getDec     ::           c -> Decimal
    putDec     ::     Decimal -> c

    needDec     ::          c -> B.Ab Decimal
    needDec = need isDec getDec

putDecFromInt :: (CDec c) => Int -> c
putDecFromInt = putDec . intDecimal

class (PrimContent c) => CSet c where
    isSet       ::          c -> Bool
    getSet      ::          c -> [c]
    putSet      ::        [c] -> c

    needSet     ::          c -> B.Ab [c]
    needSet = need isSet getSet

class (PrimContent c) => CTermset c where
    isTermset   ::           c -> Bool
    getTermset  ::           c -> [B.Named c]
    putTermset  :: [B.Named c] -> c

    needTermset ::           c -> B.Ab [B.Named c]
    needTermset = need isTermset getTermset

class (PrimContent c) => CRel c where
    isRel       ::           c -> Bool
    getRel      ::           c -> B.Rel c
    putRel      ::     B.Rel c -> c

    needRel     ::           c -> B.Ab (B.Rel c)
    needRel = need isRel getRel

