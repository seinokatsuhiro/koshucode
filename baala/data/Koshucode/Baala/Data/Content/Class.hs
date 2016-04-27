{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Data.Content.Class
  ( -- * Generic content
    CContent (..),
    CTypeOf (..),
  
    -- * Simple contents
    -- ** Empty and full
    CEmpty (..), maybeEmpty, omitEmpty,
    CFull (..),
    -- ** Boolean
    CBool (..), true, false, putTrue, putFalse,
    -- ** Number
    CDec (..), pInt, pInteger, pDecFromInt, pDecFromInteger,
    -- ** Clock and time
    CClock (..),
    CTime (..),
    -- ** Textual
    CCode (..),
    CTerm (..), pTermSet,
    CText (..), pMaybeText, pTextList, pTextSet,

    -- * Complex contents
    -- ** Collection
    CList (..),
    CSet (..), gSetSort,
    -- ** Relational
    CTie (..),
    CRel (..), isMember, dee, dum,
    CInterp (..),
    -- ** Type
    CType (..),

    -- * Utility
    contAp, contMap,
    contApTextToText,
    contMapTextToList,

    -- * Get & Put
    CGetPut,
    gpText, gpList, gpSet, gpSetSort,
  ) where

import qualified Koshucode.Baala.Base                 as B
import qualified Koshucode.Baala.Data.Symbol          as D
import qualified Koshucode.Baala.Data.Type            as D
import qualified Koshucode.Baala.Data.Content.Message as Msg



-- ----------------------  Generic content

class (Ord c, B.Write c, CTypeOf c,
       CEmpty c, CFull c,
       CBool c, CCode c, CText c, CClock c, CTime c,
       CTerm c, CDec c, CType c, CInterp c,
       CList c, CSet c, CTie c, CRel c) =>
    CContent c where

    appendContent :: c -> c -> B.Ab c

    joinContent :: [c] -> B.Ab c
    joinContent = B.foldM appendContent empty

class (Show c, B.Write c) => CTypeOf c where
    typeOf :: c -> D.Type

getAbAb :: CTypeOf c => (c -> Bool) -> (c -> b) -> B.Ab c -> B.Ab b
getAbAb _ _ (Left reason) =  Left reason
getAbAb is get (Right x)
    | is x = Right $ get x
    | otherwise = Msg.unmatchType $ show $ B.doc $ typeOf x


-- ----------------------  Simple contents

-- | Types that can be empty.
class (CTypeOf c) => CEmpty c where
    isEmpty     ::          c -> Bool
    empty       ::          c

maybeEmpty :: (CEmpty c) => (a -> c) -> Maybe a -> c
maybeEmpty f (Just a)   = f a
maybeEmpty _ (Nothing)  = empty

omitEmpty :: (CEmpty c) => B.Map [(a, c)]
omitEmpty = B.omit (isEmpty . snd)

-- | The maximum content.
class (CTypeOf c) => CFull c where
    isFull      ::          c -> Bool
    full        ::          c

-- | True or false, affirmed or denied.
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

putTrue :: (CBool c) => B.Ab c
putTrue  = putBool True

putFalse :: (CBool c) => B.Ab c
putFalse = putBool False

-- | Decimal number.
class (CTypeOf c) => CDec c where
    isDec       ::           c -> Bool
    gDec        ::           c -> D.Decimal
    pDec        ::   D.Decimal -> c

    getDec      ::     B.Ab c -> B.Ab D.Decimal
    getDec      =      getAbAb isDec gDec

    putDec      ::   D.Decimal -> B.Ab c
    putDec      =    Right . pDec

pInt :: (CDec c) => Int -> c
pInt = pDec . D.integralDecimal . toInteger

pInteger :: (CDec c) => Integer -> c
pInteger = pDec . D.integralDecimal

pDecFromInt :: (CDec c) => Int -> c
pDecFromInt = pInt

pDecFromInteger :: (CDec c) => Integer -> c
pDecFromInteger = pInteger

-- | Distance between two points in timeline.
class (CTypeOf c) => CClock c where
    isClock      ::           c -> Bool
    gClock       ::           c -> D.Clock
    pClock       ::     D.Clock -> c

    getClock     ::      B.Ab c -> B.Ab D.Clock
    getClock     =       getAbAb isClock gClock

    putClock     ::     D.Clock -> B.Ab c
    putClock     =      Right . pClock

-- | Point in timeline.
class (CTypeOf c) => CTime c where
    isTime       ::           c -> Bool
    gTime        ::           c -> D.Time
    pTime        ::      D.Time -> c

    getTime      ::     B.Ab c -> B.Ab D.Time
    getTime      =      getAbAb isTime gTime

    putTime      ::   D.Time -> B.Ab c
    putTime      =    Right . pTime

-- | Code.
class (CTypeOf c) => CCode c where
    isCode       ::           c -> Bool
    gCode        ::           c -> String
    pCode        ::      String -> c

    getCode      ::      B.Ab c -> B.Ab String
    getCode      =       getAbAb isCode gCode

    putCode      ::      String -> B.Ab c
    putCode      =       Right . pCode

-- | Term name.
class (CTypeOf c) => CTerm c where
    isTerm       ::           c -> Bool
    gTerm        ::           c -> String
    pTerm        ::      String -> c

    getTerm      ::      B.Ab c -> B.Ab String
    getTerm      =       getAbAb isTerm gTerm

    putTerm      ::      String -> B.Ab c
    putTerm      =       Right . pTerm

pTermSet :: (CTerm c, CSet c) => [String] -> c
pTermSet = pSet . map pTerm

-- | Double-quoted text content.
class (CTypeOf c) => CText c where
    isText      ::       c -> Bool
    gText       ::       c -> String
    pText       ::  String -> c

    getText     ::  B.Ab c -> B.Ab String
    getText     =   getAbAb isText gText

    putText     ::  String -> B.Ab c
    putText     =    Right . pText

pMaybeText :: (CText c, CEmpty c) => String -> c
pMaybeText s | B.trimLeft s == "" = empty
             | otherwise          = pText s

pTextSet :: (CText c, CSet c) => [String] -> c
pTextSet = pSet . map pText

pTextList :: (CText c, CList c) => [String] -> c
pTextList = pList . map pText


-- ----------------------  Complex contents

-- | List of contents.
class (CTypeOf c) => CList c where
    isList      ::       c -> Bool
    gList       ::       c -> [c]
    pList       ::     [c] -> c

    getList     ::  B.Ab c -> B.Ab [c]
    getList     =   getAbAb isList gList

    putList     ::     [c] -> B.Ab c
    putList     =    Right . pList

-- | Set of contents.
class (CTypeOf c) => CSet c where
    isSet       ::          c -> Bool
    gSet        ::          c -> [c]
    pSet        ::        [c] -> c

    getSet      ::     B.Ab c -> B.Ab [c]
    getSet      =      getAbAb isSet gSet

    putSet      ::        [c] -> B.Ab c
    putSet      =       Right . pSet

gSetSort :: (Ord c, CSet c) => c -> [c]
gSetSort = B.sort . gSet

-- | Tie of terms.
class (CTypeOf c) => CTie c where
    isTie       ::           c -> Bool
    gTie        ::           c -> [D.Term c]
    pTie        ::  [D.Term c] -> c

    getTie      ::      B.Ab c -> B.Ab [D.Term c]
    getTie      =       getAbAb isTie gTie

    putTie      ::  [D.Term c] -> B.Ab c
    putTie      =  Right . pTie

-- | Relation of terms.
class (CTypeOf c) => CRel c where
    isRel       ::           c -> Bool
    gRel        ::           c -> D.Rel c
    pRel        ::     D.Rel c -> c

    getRel      ::      B.Ab c -> B.Ab (D.Rel c)
    getRel      =       getAbAb isRel gRel

    putRel      ::     D.Rel c -> B.Ab c
    putRel      =      Right . pRel

-- | Nullary full relation.
dee :: (CRel c) => c
dee = pRel D.reldee

-- | Nullary empty relation.
dum :: (CRel c) => c
dum = pRel D.reldum

-- | Data intepretation.
class (CTypeOf c) => CInterp c where
    isInterp    ::           c -> Bool
    gInterp     ::           c -> D.Interp
    pInterp     ::    D.Interp -> c

    getInterp   ::      B.Ab c -> B.Ab D.Interp
    getInterp   =       getAbAb isInterp gInterp

    putInterp   ::    D.Interp -> B.Ab c
    putInterp   =     Right . pInterp

-- | Type of content.
class (CTypeOf c) => CType c where
    isType      ::           c -> Bool
    gType       ::           c -> D.Type
    pType       ::      D.Type -> c

    getType     ::      B.Ab c -> B.Ab D.Type
    getType     =       getAbAb isType gType

    putType     ::      D.Type -> B.Ab c
    putType     =       Right . pType


-- ----------------------  Utility

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


-- ----------------------  Get & Put

type CGetPut a c = (c -> a, a -> c)

gpText :: (CText c) => CGetPut [Char] c
gpText = (gText, pText)

gpList :: (CList c) => CGetPut [c] c
gpList = (gList, pList)

gpSet :: (CSet c) => CGetPut [c] c
gpSet = (gSet, pSet)

gpSetSort :: (Ord c, CSet c) => CGetPut [c] c
gpSetSort = (gSetSort, pSet)
