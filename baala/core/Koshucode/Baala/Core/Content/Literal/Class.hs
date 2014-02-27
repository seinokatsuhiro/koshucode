{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Content.Literal.Class
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

  putTextList,
  putTextSet,
) where

import qualified Control.Monad as Monad
import qualified Koshucode.Baala.Base as B



-- ----------------------  Generic content

class (Show c) => PrimContent c where
    typename :: c -> String

class (Ord c, B.Pretty c, PrimContent c,
       CBool c, CText c, CDec c, CList c,
       CNil c , CSet c, CTermset c, CRel c) =>
    CContent c where

    appendContent :: c -> c -> B.Ab c

    joinContent :: [c] -> B.Ab c
    joinContent = Monad.foldM appendContent nil

{-| Delete empty list ('null') from content list. -}
nonNullFilter :: B.Map [[a]]
nonNullFilter = filter (not . null)

{-| Delete 'nil' from content list. -}
nonNilFilter :: (CNil c) => B.Map [c]
nonNilFilter = filter (not . isNil)

need :: PrimContent c => (c -> Bool) -> (c -> b) -> B.Ab c -> B.Ab b
need _ _ (Left reason) =  Left reason
need is get (Right x)
    | is x = Right $ get x
    | otherwise = Left $ B.AbortCalc [] $ B.ACUnmatchType (typename x)



-- ----------------------  Haskell built-in data

class (PrimContent c) => CBool c where
    {-| Test content @c@ has Boolean value. -}
    isBool     ::       c -> Bool
    {-| Put Boolean value into content @c@. -}
    putBool    ::    Bool -> c
    putBoolA   ::    Bool -> B.Ab c
    putBoolA   =    Right . putBool
    {-| Get Boolean value from content @c@. -}
    getBool    ::       c -> Bool
    {-| Get Boolean value from content @c@ if @c@ is @CBool@. -}
    needBool   ::  B.Ab c -> B.Ab Bool
    needBool = need isBool getBool

class (PrimContent c) => CText c where
    isText     ::       c -> Bool
    getText    ::       c -> String
    putText    ::  String -> c
    putTextA   ::  String -> B.Ab c
    putTextA   =    Right . putText

    needText   ::  B.Ab c -> B.Ab String
    needText = need isText getText

class (PrimContent c) => CList c where
    isList     ::       c -> Bool
    getList    ::       c -> [c]
    putList    ::     [c] -> c
    putListA   ::     [c] -> B.Ab c
    putListA   =    Right . putList

    needList   ::  B.Ab c -> B.Ab [c]
    needList = need isList getList



-- ----------------------  Data in koshucode

{-| Types that can be nil -}
class (PrimContent c) => CNil c where
    isNil       ::          c -> Bool
    nil         ::          c

class (PrimContent c) => CDec c where
    isDec      ::           c -> Bool
    getDec     ::           c -> B.Decimal
    putDec     ::   B.Decimal -> c
    putDecA    ::   B.Decimal -> B.Ab c
    putDecA    =    Right . putDec

    needDec     ::     B.Ab c -> B.Ab B.Decimal
    needDec = need isDec getDec

putDecFromInt :: (CDec c) => Int -> c
putDecFromInt = putDec . B.intDecimal

class (PrimContent c) => CSet c where
    isSet       ::          c -> Bool
    getSet      ::          c -> [c]
    putSet      ::        [c] -> c
    putSetA     ::        [c] -> B.Ab c
    putSetA      =      Right . putSet

    needSet     ::     B.Ab c -> B.Ab [c]
    needSet = need isSet getSet

class (PrimContent c) => CTermset c where
    isTermset   ::           c -> Bool
    getTermset  ::           c -> [B.Named c]
    putTermset  :: [B.Named c] -> c
    putTermsetA :: [B.Named c] -> B.Ab c
    putTermsetA  = Right . putTermset

    needTermset ::      B.Ab c -> B.Ab [B.Named c]
    needTermset = need isTermset getTermset

class (PrimContent c) => CRel c where
    isRel       ::           c -> Bool
    getRel      ::           c -> B.Rel c
    putRel      ::     B.Rel c -> c
    putRelA     ::     B.Rel c -> B.Ab c
    putRelA      =     Right. putRel

    needRel     ::      B.Ab c -> B.Ab (B.Rel c)
    needRel = need isRel getRel

putTextSet :: (CText c, CSet c) => [String] -> c
putTextSet = putSet . map putText

putTextList :: (CText c, CList c) => [String] -> c
putTextList = putList . map putText

