{-# OPTIONS_GHC -Wall #-}

-- | Generic content class.

module Koshucode.Baala.Data.Class.Content
  ( -- * Generic content
    CContent (..),
    toDec, toDecReplace,
    cTrimBoth, cTrimBegin, cTrimEnd,
    valueContent,
  ) where

import qualified Koshucode.Baala.Overture                 as O
import qualified Koshucode.Baala.Base                     as B
import qualified Koshucode.Baala.Syntax                   as S
import qualified Koshucode.Baala.Type                     as T
import qualified Koshucode.Baala.Data.Class.Complex       as D
import qualified Koshucode.Baala.Data.Class.Edge          as D
import qualified Koshucode.Baala.Data.Class.Simple        as D

-- | Generic content class.
class (Show c, B.Default c,
       D.CEmpty c, D.CEnd c,
       D.CBool c, D.CCode c, D.CText c, D.CClock c, D.CTime c,
       D.CTerm c, D.CDec c, D.CType c, D.CInterp c,
       D.CList c, D.CSet c, D.CTie c, D.CRel c) =>
    CContent c where

    -- | Order of content type.
    typeOrder :: c -> Int
    typeOrder c
        -- empty
        | D.isEmpty    c = 1

        -- simple numeric
        | D.isBool     c = 11
        | D.isDec      c = 12
        | D.isClock    c = 13
        | D.isTime     c = 14

        -- simple textual
        | D.isCode     c = 15
        | D.isTerm     c = 16
        | D.isText     c = 17

        -- complex
        | D.isList     c = 21
        | D.isSet      c = 22
        | D.isTie      c = 23
        | D.isRel      c = 24
        | D.isInterp   c = 25
        | D.isType     c = 27

        -- end
        | D.isEnd      c = 31
        | otherwise      = error "unknown content"

-- | Convert some content to decimal number.
toDec :: (CContent c) => O.Map c
toDec = toDecWith id

-- | Convert content to decimal number,
--   or replace inconvertible content to constant decimal.
--
--   >>> toDecReplace (the $ pInt 0) (the $ pText "12.0")
--   ContentDec Decimal (1) 12
--
--   >>> toDecReplace (the $ pInt 0) (the $ pText "aa")
--   ContentDec Decimal (0) 0
--
toDecReplace :: (CContent c) => c -> O.Map c
toDecReplace rep = toDecWith $ const rep

toDecWith :: (CContent c) => O.Map c -> O.Map c
toDecWith f c
    | D.isText c  = case T.decodeDecimal $ D.gText c of
                      Right n  -> D.pDec n
                      Left _   -> f c
    | D.isBool c  = case D.gBool c of
                      True   -> D.pInt 1
                      False  -> D.pInt 0
    | otherwise   = f c

-- | Trim spaces of text content.
--
--   >>> cTrimBoth (the $ pText " foo ")
--   ContentText "foo"
--
cTrimBoth :: (CContent c) => O.Map c
cTrimBoth = cTextMap O.trimBoth

-- | Trim beginning spaces of text content.
cTrimBegin :: (CContent c) => O.Map c
cTrimBegin = cTextMap O.trimBegin

-- | Trim ending spaces of text content.
cTrimEnd :: (CContent c) => O.Map c
cTrimEnd = cTextMap O.trimEnd

cTextMap :: (CContent c) => S.CharsMap -> O.Map c
cTextMap f c
    | D.isText c  = D.pText $ f $ D.gText c
    | otherwise   = c

-- | Convert 'O.Value' to content.
valueContent :: (CContent c) => O.Value -> c
valueContent (O.VEmpty)      = D.empty
valueContent (O.VBool b)     = D.pBool b
valueContent (O.VInt i)      = D.pInt i
valueContent (O.VInteger i)  = D.pInteger i
valueContent (O.VStr s)      = D.pText s
valueContent (O.VTx s)       = D.pText s
valueContent (O.VTz s)       = D.pText s
valueContent (O.VList cs)    = D.pList (valueContent <$> cs)

