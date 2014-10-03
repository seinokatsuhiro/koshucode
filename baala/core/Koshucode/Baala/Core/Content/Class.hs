{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Content.Class
( -- * Generic content
  CContent    (..),
  CTypeOf     (..),

  -- * Haskell data
  CBool       (..), true, false,
  CText       (..), pTextList, pTextSet,
  CList       (..),

  -- * Koshu data
  CEmpty      (..),
  CDec        (..), pDecFromInt,
  CTerm       (..),
  CSet        (..),
  CAssn       (..),
  CRel        (..), isMember,
  CInterp     (..),
  CType       (..),

  contAp, contMap,
  contApTextToText,
  contMapTextToList,
) where

import qualified Koshucode.Baala.Base         as B
import qualified Koshucode.Baala.Core.Message as Msg



-- ----------------------  Generic content

class (Ord c, B.Write c, CTypeOf c,
       CEmpty c, CBool c, CText c, CTerm c, CDec c, CType c, CInterp c,
       CList c, CSet c, CAssn c, CRel c) =>
    CContent c where

    appendContent :: c -> c -> B.Ab c

    joinContent :: [c] -> B.Ab c
    joinContent = B.foldM appendContent empty

class (Show c, B.Write c) => CTypeOf c where
    typeOf :: c -> B.Type

getAbAb :: CTypeOf c => (c -> Bool) -> (c -> b) -> B.Ab c -> B.Ab b
getAbAb _ _ (Left reason) =  Left reason
getAbAb is get (Right x)
    | is x = Right $ get x
    | otherwise = Msg.unmatchType $ show $ B.doc $ typeOf x



-- ----------------------  Haskell built-in data

class (CTypeOf c) => CBool c where
    isBool      ::       c -> Bool
    pBool       ::    Bool -> c
    gBool       ::       c -> Bool

    getBool     ::  B.Ab c -> B.Ab Bool
    getBool     =   getAbAb isBool gBool

    putBool     ::    Bool -> B.Ab c
    putBool     =    Right . pBool

true, false :: (CBool c) => c
true  = pBool True
false = pBool False

class (CTypeOf c) => CText c where
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

class (CTypeOf c) => CList c where
    isList      ::       c -> Bool
    gList       ::       c -> [c]
    pList       ::     [c] -> c

    getList     ::  B.Ab c -> B.Ab [c]
    getList     =   getAbAb isList gList

    putList     ::     [c] -> B.Ab c
    putList     =    Right . pList



-- ----------------------  Data in koshucode

-- | Types that can be empty
class (CTypeOf c) => CEmpty c where
    isEmpty     ::          c -> Bool
    empty       ::          c

class (CTypeOf c) => CDec c where
    isDec       ::           c -> Bool
    gDec        ::           c -> B.Decimal
    pDec        ::   B.Decimal -> c

    getDec      ::     B.Ab c -> B.Ab B.Decimal
    getDec      =      getAbAb isDec gDec

    putDec      ::   B.Decimal -> B.Ab c
    putDec      =    Right . pDec

pDecFromInt :: (CDec c) => Int -> c
pDecFromInt = pDec . B.intDecimal

class (CTypeOf c) => CTerm c where
    isTerm       ::           c -> Bool
    gTerm        ::           c -> String
    pTerm        ::      String -> c

    getTerm      ::      B.Ab c -> B.Ab String
    getTerm      =       getAbAb isTerm gTerm

    putTerm      ::      String -> B.Ab c
    putTerm      =       Right . pTerm

class (CTypeOf c) => CSet c where
    isSet       ::          c -> Bool
    gSet        ::          c -> [c]
    pSet        ::        [c] -> c

    getSet      ::     B.Ab c -> B.Ab [c]
    getSet      =      getAbAb isSet gSet

    putSet      ::        [c] -> B.Ab c
    putSet      =       Right . pSet

class (CTypeOf c) => CAssn c where
    isAssn      ::           c -> Bool
    gAssn       ::           c -> [B.Named c]
    pAssn       :: [B.Named c] -> c

    getAssn     ::      B.Ab c -> B.Ab [B.Named c]
    getAssn     =       getAbAb isAssn gAssn

    putAssn     :: [B.Named c] -> B.Ab c
    putAssn     =  Right . pAssn

class (CTypeOf c) => CRel c where
    isRel       ::           c -> Bool
    gRel        ::           c -> B.Rel c
    pRel        ::     B.Rel c -> c

    getRel      ::      B.Ab c -> B.Ab (B.Rel c)
    getRel      =       getAbAb isRel gRel

    putRel      ::     B.Rel c -> B.Ab c
    putRel      =      Right . pRel

class (CTypeOf c) => CInterp c where
    isInterp    ::           c -> Bool
    gInterp     ::           c -> B.Interp
    pInterp     ::    B.Interp -> c

    getInterp   ::      B.Ab c -> B.Ab B.Interp
    getInterp   =       getAbAb isInterp gInterp

    putInterp   ::    B.Interp -> B.Ab c
    putInterp   =     Right . pInterp

class (CTypeOf c) => CType c where
    isType      ::           c -> Bool
    gType       ::           c -> B.Type
    pType       ::      B.Type -> c

    getType     ::      B.Ab c -> B.Ab B.Type
    getType     =       getAbAb isType gType

    putType     ::      B.Type -> B.Ab c
    putType     =       Right . pType

isMember :: (Eq c, CSet c, CList c) => c -> c -> Bool
isMember x xs | isSet xs  = x `elem` gSet xs
isMember x xs | isList xs = x `elem` gList xs
isMember _ _ = False

contAp :: (c -> a) -> (b -> d) -> (a -> b) -> c -> d
contAp get put f = put . f . get

contMap :: (c -> [a]) -> ([b] -> d) -> (a -> b) -> c -> d
contMap get put f = contAp get put $ map f

contApTextToText :: (CText c) => B.Map String -> B.AbMap c
contApTextToText = contAp gText putText

contMapTextToList :: (CList c, CText c) => (Char -> c) -> B.AbMap c
contMapTextToList = contMap gText putList
