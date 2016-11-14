{-# OPTIONS_GHC -Wall #-}

-- | Generic content class.

module Koshucode.Baala.Data.Class.Content
  ( -- * Generic content
    CContent (..),
  ) where

import qualified Koshucode.Baala.Base                     as B
import qualified Koshucode.Baala.Data.Class.Singleton     as D
import qualified Koshucode.Baala.Data.Class.Simple        as D
import qualified Koshucode.Baala.Data.Class.Complex       as D

{-# DEPRECATED appendContent "Do not use it." #-}
{-# DEPRECATED joinContent "Do not use it." #-}

-- | Generic content class.
class (Ord c, Show c, B.MixShortEncode c, D.CTypeOf c,
       D.CEmpty c, D.CEnd c,
       D.CBool c, D.CCode c, D.CText c, D.CClock c, D.CTime c,
       D.CTerm c, D.CDec c, D.CType c, D.CInterp c,
       D.CList c, D.CSet c, D.CTie c, D.CRel c) =>
    CContent c where

    appendContent :: c -> c -> B.Ab c

    joinContent :: [c] -> B.Ab c
    joinContent = B.foldM appendContent D.empty

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

