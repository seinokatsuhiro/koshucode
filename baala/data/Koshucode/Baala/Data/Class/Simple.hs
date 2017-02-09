{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

-- | Simple content type.

module Koshucode.Baala.Data.Class.Simple
  ( -- * Simple contents
    -- ** Boolean
    CBool (..), true, false, putTrue, putFalse,
    -- ** Decimal
    CDec (..), pInt, pInteger, pIntegral, gRational, gIntegral,
    getRational, getIntegral,
    -- ** Clock and time
    CClock (..),
    CTime (..),
    -- ** Textual
    CCode (..),
    CTerm (..),
    CText (..), getChar, pChar, pMaybeText, pTx, pTz,
  ) where

import Prelude hiding (getChar)
import qualified Data.Text                               as Tx
import qualified Data.Text.Lazy                          as Tz
import qualified Koshucode.Baala.Overture                as O
import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Syntax                  as S
import qualified Koshucode.Baala.Type                    as T
import qualified Koshucode.Baala.Data.Class.Edge         as D
import qualified Koshucode.Baala.Data.Class.Message      as Msg


-- ============================================  Simple contents

-- ---------------------------------  Bool

-- | True or false, affirmed or denied.
class (D.Basis c) => CBool c where
    isBool      ::       c -> Bool
    pBool       ::    Bool -> c
    gBool       ::       c -> Bool

    getBool     ::  D.GetContent Bool c
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

-- ---------------------------------  Dec

-- | Decimal number.
class (D.Basis c) => CDec c where
    isDec       ::           c -> Bool
    gDec        ::           c -> T.Decimal
    pDec        ::   T.Decimal -> c

    getDec      ::     D.GetContent T.Decimal c
    getDec      =      D.getContent isDec gDec

    putDec      ::   T.Decimal -> B.Ab c
    putDec      =    Right . pDec

-- | Create decimal content.
--
--   >>> pInt 15 :: Content
--   ContentDec Decimal (0) 15
--
pInt :: (CDec c) => Int -> c
pInt = pIntegral

-- | Create decimal content.
pInteger :: (CDec c) => Integer -> c
pInteger = pDec . fromInteger

-- | Create decimal content.
pIntegral :: (CDec c, Integral n) => n -> c
pIntegral = pInteger . toInteger

-- | Get rational number from decimal content.
gRational :: (CDec c) => c -> Rational
gRational = toRational . gDec

-- | Get rational number from decimal content.
getRational :: (CDec c) => D.GetContent Rational c
getRational (Right c) | isDec c  = Right $ gRational c
getRational (Right c)            = Msg.notDec c
getRational (Left a)             = Left a

-- | Get truncated integer from decimal content.
gIntegral :: (CDec c, Integral n) => c -> n
gIntegral = truncate . gRational

-- | Get truncated integer from decimal content.
getIntegral :: (CDec c, Integral n) => D.GetContent n c
getIntegral (Right c) | isDec c  = Right $ gIntegral c
getIntegral (Right c)            = Msg.notDec c
getIntegral (Left a)             = Left a

-- ---------------------------------  Clock

-- | Distance between two points in timeline.
class (D.Basis c) => CClock c where
    isClock      ::           c -> Bool
    gClock       ::           c -> T.Clock
    pClock       ::     T.Clock -> c

    getClock     ::      D.GetContent T.Clock c
    getClock     =       D.getContent isClock gClock

    putClock     ::     T.Clock -> B.Ab c
    putClock     =      Right . pClock

-- ---------------------------------  Time

-- | Point in timeline.
class (D.Basis c) => CTime c where
    isTime       ::           c -> Bool
    gTime        ::           c -> T.Time
    pTime        ::      T.Time -> c

    getTime      ::     D.GetContent T.Time c
    getTime      =      D.getContent isTime gTime

    putTime      ::   T.Time -> B.Ab c
    putTime      =    Right . pTime

-- ---------------------------------  Code

-- | Code.
class (D.Basis c) => CCode c where
    isCode       ::           c -> Bool
    gCode        ::           c -> S.Chars
    pCode        ::      (O.Textual t) => t -> c

    getCode      ::      D.GetContent S.Chars c
    getCode      =       D.getContent isCode gCode

    putCode      ::      (O.Textual t) => t -> B.Ab c
    putCode      =       Right . pCode

-- ---------------------------------  Term

-- | Term name.
class (D.Basis c) => CTerm c where
    isTerm       ::           c -> Bool
    gTerm        ::           c -> S.TermName
    pTerm        ::  S.TermName -> c

    getTerm      ::      D.GetContent S.TermName c
    getTerm      =       D.getContent isTerm gTerm

    putTerm      ::  S.TermName -> B.Ab c
    putTerm      =       Right . pTerm


-- ---------------------------------  Text

-- | Double-quoted text content.
class (D.Basis c) => CText c where
    isText      :: c -> Bool
    gText       :: c -> S.Chars
    pText       :: (O.Textual t) => t -> c

    getText     :: D.GetContent S.Chars c
    getText      = D.getContent isText gText

    putText     :: (O.Textual t) => t -> B.Ab c
    putText      = Right . pText

-- | Text content of single character.
pChar :: (CText c) => Char -> c
pChar = pText . (O.charT :: Char -> S.Chars)

-- | Get single character of text content.
getChar :: (CText c) => B.Ab c -> B.Ab Char
getChar (getText -> Right (O.cut2 -> O.Jp c Nothing)) = Right c
getChar (Right c) = Msg.typeUnmatched c
getChar (Left a)  = Left a

-- | Create text or empty content.
--
--   >>> pMaybeText "a" :: Content
--   ContentText "a"
--
--   >>> pMaybeText "" :: Content
--   ContentEmpty
--
pMaybeText :: (O.Textual t, CText c, D.CEmpty c) => t -> c
pMaybeText s | O.tIsEmpty $ O.trimBegin s  = D.empty
             | otherwise                   = pText s

-- | Create text content from strict text.
{-# DEPRECATED pTx "Consider 'pText'." #-}
pTx :: (CText c) => Tx.Text -> c
pTx = pText

-- | Create text content from lazy text.
{-# DEPRECATED pTz "Consider 'pText'." #-}
pTz :: (CText c) => Tz.Text -> c
pTz = pText

