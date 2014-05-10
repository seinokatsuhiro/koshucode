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
  pTextList,
  pTextSet,
  CList       (..),

  -- * Koshu data
  CNil        (..),
  CDec        (..),
  pDecFromInt,
  CSet        (..),
  CTermset    (..),
  CRel        (..),
  isMember,
) where

import qualified Koshucode.Baala.Base         as B
import qualified Koshucode.Baala.Core.Message as Message



-- ----------------------  Generic content

class (Show c) => PrimContent c where
    typename :: c -> String

class (Ord c, B.Pretty c, PrimContent c,
       CBool c, CText c, CDec c, CList c,
       CNil c , CSet c, CTermset c, CRel c) =>
    CContent c where

    appendContent :: c -> c -> B.Ab c

    joinContent :: [c] -> B.Ab c
    joinContent = B.foldM appendContent nil

-- | Delete empty list ('null') from content list.
nonNullFilter :: B.Map [[a]]
nonNullFilter = filter (not . null)

-- | Delete 'nil' from content list.
nonNilFilter :: (CNil c) => B.Map [c]
nonNilFilter = filter (not . isNil)

getAbAb :: PrimContent c => (c -> Bool) -> (c -> b) -> B.Ab c -> B.Ab b
getAbAb _ _ (Left reason) =  Left reason
getAbAb is get (Right x)
    | is x = Right $ get x
    | otherwise = Message.unmatchType (typename x)



-- ----------------------  Haskell built-in data

class (PrimContent c) => CBool c where
    isBool      ::       c -> Bool
    pBool       ::    Bool -> c
    gBool       ::       c -> Bool

    getBool     ::  B.Ab c -> B.Ab Bool
    getBool     =   getAbAb isBool gBool

    putBool     ::    Bool -> B.Ab c
    putBool     =    Right . pBool

class (PrimContent c) => CText c where
    isText      ::       c -> Bool
    gText       ::       c -> String
    pText       ::  String -> c

    getText     ::  B.Ab c -> B.Ab String
    getText     =   getAbAb isText gText

    putText     ::  String -> B.Ab c
    putText     =    Right . pText

pTextSet :: (CText c, CSet c) => [String] -> c
pTextSet = pSet . map pText

pTextList :: (CText c, CList c) => [String] -> c
pTextList = pList . map pText

class (PrimContent c) => CList c where
    isList      ::       c -> Bool
    gList       ::       c -> [c]
    pList       ::     [c] -> c

    getList     ::  B.Ab c -> B.Ab [c]
    getList     =   getAbAb isList gList

    putList     ::     [c] -> B.Ab c
    putList     =    Right . pList



-- ----------------------  Data in koshucode

-- | Types that can be nil
class (PrimContent c) => CNil c where
    isNil       ::          c -> Bool
    nil         ::          c

class (PrimContent c) => CDec c where
    isDec       ::           c -> Bool
    gDec        ::           c -> B.Decimal
    pDec        ::   B.Decimal -> c

    getDec      ::     B.Ab c -> B.Ab B.Decimal
    getDec      =      getAbAb isDec gDec

    putDec      ::   B.Decimal -> B.Ab c
    putDec      =    Right . pDec

pDecFromInt :: (CDec c) => Int -> c
pDecFromInt = pDec . B.intDecimal

class (PrimContent c) => CSet c where
    isSet       ::          c -> Bool
    gSet        ::          c -> [c]
    pSet        ::        [c] -> c

    getSet      ::     B.Ab c -> B.Ab [c]
    getSet      =      getAbAb isSet gSet

    putSet      ::        [c] -> B.Ab c
    putSet      =       Right . pSet

class (PrimContent c) => CTermset c where
    isTermset   ::           c -> Bool
    gTermset    ::           c -> [B.Named c]
    pTermset    :: [B.Named c] -> c

    getTermset  ::      B.Ab c -> B.Ab [B.Named c]
    getTermset  =       getAbAb isTermset gTermset

    putTermset  :: [B.Named c] -> B.Ab c
    putTermset  =  Right . pTermset

class (PrimContent c) => CRel c where
    isRel       ::           c -> Bool
    gRel        ::           c -> B.Rel c
    pRel        ::     B.Rel c -> c

    getRel      ::      B.Ab c -> B.Ab (B.Rel c)
    getRel      =       getAbAb isRel gRel

    putRel      ::     B.Rel c -> B.Ab c
    putRel      =      Right. pRel

isMember :: (Eq c, CSet c, CList c) => c -> c -> Bool
isMember x xs | isSet xs  = x `elem` gSet xs
isMember x xs | isList xs = x `elem` gList xs
isMember _ _ = False

