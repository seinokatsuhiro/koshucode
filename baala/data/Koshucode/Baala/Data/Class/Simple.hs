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
    CText (..), pMaybeText, pTx, pTz,
  ) where

import qualified Data.Text                               as Tx
import qualified Data.Text.Lazy                          as Tz
import qualified Koshucode.Baala.Overture                as O
import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Syntax                  as S
import qualified Koshucode.Baala.Type                    as D
import qualified Koshucode.Baala.Data.Class.Edge         as D
import qualified Koshucode.Baala.Data.Class.Message      as Msg


-- ============================================  Simple contents

-- ---------------------------------  Bool

-- | True or false, affirmed or denied.
class (D.CTypeOf c) => CBool c where
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
class (D.CTypeOf c) => CDec c where
    isDec       ::           c -> Bool
    gDec        ::           c -> D.Decimal
    pDec        ::   D.Decimal -> c

    getDec      ::     D.GetContent D.Decimal c
    getDec      =      D.getContent isDec gDec

    putDec      ::   D.Decimal -> B.Ab c
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
getRational :: (B.MixEncode c, CDec c) => D.GetContent Rational c
getRational (Right c) | isDec c  = Right $ gRational c
getRational (Right c)            = Msg.notDec c
getRational (Left a)             = Left a

-- | Get truncated integer from decimal content.
gIntegral :: (CDec c, Integral n) => c -> n
gIntegral = truncate . gRational

-- | Get truncated integer from decimal content.
getIntegral :: (B.MixEncode c, CDec c, Integral n) => D.GetContent n c
getIntegral (Right c) | isDec c  = Right $ gIntegral c
getIntegral (Right c)            = Msg.notDec c
getIntegral (Left a)             = Left a

-- ---------------------------------  Clock

-- | Distance between two points in timeline.
class (D.CTypeOf c) => CClock c where
    isClock      ::           c -> Bool
    gClock       ::           c -> D.Clock
    pClock       ::     D.Clock -> c

    getClock     ::      D.GetContent D.Clock c
    getClock     =       D.getContent isClock gClock

    putClock     ::     D.Clock -> B.Ab c
    putClock     =      Right . pClock

-- ---------------------------------  Time

-- | Point in timeline.
class (D.CTypeOf c) => CTime c where
    isTime       ::           c -> Bool
    gTime        ::           c -> D.Time
    pTime        ::      D.Time -> c

    getTime      ::     D.GetContent D.Time c
    getTime      =      D.getContent isTime gTime

    putTime      ::   D.Time -> B.Ab c
    putTime      =    Right . pTime

-- ---------------------------------  Code

-- | Code.
class (D.CTypeOf c) => CCode c where
    isCode       ::           c -> Bool
    gCode        ::           c -> String
    pCode        ::      String -> c

    getCode      ::      D.GetContent String c
    getCode      =       D.getContent isCode gCode

    putCode      ::      String -> B.Ab c
    putCode      =       Right . pCode

-- ---------------------------------  Term

-- | Term name.
class (D.CTypeOf c) => CTerm c where
    isTerm       ::           c -> Bool
    gTerm        ::           c -> S.TermName
    pTerm        ::  S.TermName -> c

    getTerm      ::      D.GetContent S.TermName c
    getTerm      =       D.getContent isTerm gTerm

    putTerm      ::  S.TermName -> B.Ab c
    putTerm      =       Right . pTerm


-- ---------------------------------  Text

-- | Double-quoted text content.
class (D.CTypeOf c) => CText c where
    isText      :: c -> Bool
    gText       :: c -> String
    pText       :: String -> c

    getText     :: D.GetContent String c
    getText      = D.getContent isText gText

    putText     :: String -> B.Ab c
    putText      = Right . pText

-- | Create text or empty content.
--
--   >>> pMaybeText "a" :: Content
--   ContentText "a"
--
--   >>> pMaybeText "" :: Content
--   ContentEmpty
--
pMaybeText :: (CText c, D.CEmpty c) => String -> c
pMaybeText s | O.trimBegin s == "" = D.empty
             | otherwise           = pText s

-- | Create text content from strict text.
pTx :: (CText c) => Tx.Text -> c
pTx = pText . Tx.unpack

-- | Create text content from lazy text.
pTz :: (CText c) => Tz.Text -> c
pTz = pText . Tz.unpack

