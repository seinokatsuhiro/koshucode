{-# OPTIONS_GHC -Wall #-}

-- | Generic content class.

module Koshucode.Baala.Data.Class.Content
  ( -- * Generic content
    CContent (..),
    toDec,
  ) where

import qualified Koshucode.Baala.Overture                 as O
import qualified Koshucode.Baala.Base                     as B
import qualified Koshucode.Baala.Type                     as T
import qualified Koshucode.Baala.Data.Class.Complex       as D
import qualified Koshucode.Baala.Data.Class.Edge          as D
import qualified Koshucode.Baala.Data.Class.Simple        as D

-- | Generic content class.
class (Ord c, Show c, B.Default c, B.MixEncode c,
       D.CTypeOf c, D.CEmpty c, D.CEnd c,
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
toDec c
    | D.isText c  = case T.decodeDecimal $ D.gText c of
                      Right n  -> D.pDec n
                      Left _   -> c
    | D.isBool c  = case D.gBool c of
                      True   -> D.pInt 1
                      False  -> D.pInt 0
    | otherwise   = c

