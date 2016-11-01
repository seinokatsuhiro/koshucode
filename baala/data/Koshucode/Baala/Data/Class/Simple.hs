{-# OPTIONS_GHC -Wall #-}

-- | Simple content type.

module Koshucode.Baala.Data.Class.Simple
  ( -- * Simple contents
    -- ** Boolean
    CBool (..), true, false, putTrue, putFalse,
    -- ** Decimal
    CDec (..), pInt, pInteger, pIntegral,
    -- ** Clock and time
    CClock (..),
    CTime (..),
    -- ** Textual
    CCode (..),
    CTerm (..),
    CText (..), pMaybeText,
  ) where

import qualified Koshucode.Baala.Overture                as O
import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Data.Type               as D
import qualified Koshucode.Baala.Data.Class.Singleton    as D


-- --------------------------------------------  Simple contents

-- ----------------------  Bool

-- | True or false, affirmed or denied.
class (D.CTypeOf c) => CBool c where
    isBool      ::       c -> Bool
    pBool       ::    Bool -> c
    gBool       ::       c -> Bool

    getBool     ::  B.Ab c -> B.Ab Bool
    getBool     =   D.getContent isBool gBool

    putBool     ::    Bool -> B.Ab c
    putBool     =    Right . pBool

-- | Boolean constant true.
true :: (CBool c) => c
true  = pBool True

-- | Boolean constant false.
false :: (CBool c) => c
false = pBool False

-- | Boolean constant true.
putTrue :: (CBool c) => B.Ab c
putTrue  = putBool True

-- | Boolean constant false.
putFalse :: (CBool c) => B.Ab c
putFalse = putBool False

-- ----------------------  Dec

-- | Decimal number.
class (D.CTypeOf c) => CDec c where
    isDec       ::           c -> Bool
    gDec        ::           c -> D.Decimal
    pDec        ::   D.Decimal -> c

    getDec      ::     B.Ab c -> B.Ab D.Decimal
    getDec      =      D.getContent isDec gDec

    putDec      ::   D.Decimal -> B.Ab c
    putDec      =    Right . pDec

-- | Create decimal content.
pInt :: (CDec c) => Int -> c
pInt = pIntegral

-- | Create decimal content.
pInteger :: (CDec c) => Integer -> c
pInteger = pDec . fromInteger

-- | Create decimal content.
pIntegral :: (CDec c, Integral n) => n -> c
pIntegral = pInteger . toInteger

-- ----------------------  Clock

-- | Distance between two points in timeline.
class (D.CTypeOf c) => CClock c where
    isClock      ::           c -> Bool
    gClock       ::           c -> D.Clock
    pClock       ::     D.Clock -> c

    getClock     ::      B.Ab c -> B.Ab D.Clock
    getClock     =       D.getContent isClock gClock

    putClock     ::     D.Clock -> B.Ab c
    putClock     =      Right . pClock

-- ----------------------  Time

-- | Point in timeline.
class (D.CTypeOf c) => CTime c where
    isTime       ::           c -> Bool
    gTime        ::           c -> D.Time
    pTime        ::      D.Time -> c

    getTime      ::     B.Ab c -> B.Ab D.Time
    getTime      =      D.getContent isTime gTime

    putTime      ::   D.Time -> B.Ab c
    putTime      =    Right . pTime

-- ----------------------  Code

-- | Code.
class (D.CTypeOf c) => CCode c where
    isCode       ::           c -> Bool
    gCode        ::           c -> String
    pCode        ::      String -> c

    getCode      ::      B.Ab c -> B.Ab String
    getCode      =       D.getContent isCode gCode

    putCode      ::      String -> B.Ab c
    putCode      =       Right . pCode

-- ----------------------  Term

-- | Term name.
class (D.CTypeOf c) => CTerm c where
    isTerm       ::           c -> Bool
    gTerm        ::           c -> String
    pTerm        ::      String -> c

    getTerm      ::      B.Ab c -> B.Ab String
    getTerm      =       D.getContent isTerm gTerm

    putTerm      ::      String -> B.Ab c
    putTerm      =       Right . pTerm


-- ----------------------  Text

-- | Double-quoted text content.
class (D.CTypeOf c) => CText c where
    isText      :: c -> Bool
    gText       :: c -> String
    pText       :: String -> c

    getText     :: B.Ab c -> B.Ab String
    getText      = D.getContent isText gText

    putText     :: String -> B.Ab c
    putText      = Right . pText

-- | Create Text or empty content.
--
--   >>> pMaybeText "a" :: BaalaC
--   VText "a"
--
--   >>> pMaybeText "" :: BaalaC
--   VEmpty
--
pMaybeText :: (CText c, D.CEmpty c) => String -> c
pMaybeText s | O.trimLeft s == "" = D.empty
             | otherwise          = pText s

