{-# OPTIONS_GHC -Wall #-}

-- | Concrete content type.

module Koshucode.Baala.Core.Resource.Concrete
  ( -- * About
    About (..), AboutJudges,

    -- * Concrete type
    AboutC, AboutJudgesC, GlobalC,
    ResourceC, ResultC, ResultWriterC,
  ) where

import qualified Koshucode.Baala.Base                    as B
import qualified Koshucode.Baala.Syntax                  as S
import qualified Koshucode.Baala.Data                    as D
import qualified Koshucode.Baala.Core.Relmap             as C
import qualified Koshucode.Baala.Core.Resource.Resource  as C


-- --------------------------------------------  About

-- | @about@ clause.
data About c = About [S.Term c] deriving (Show)

instance (B.MixShortEncode c) => B.MixShortEncode (About c) where
    mixTransEncode sh (About ts) =
        B.mixString "about" `B.mixSep2` D.termsToMix2 sh ts

-- | Judges with about setting.
type AboutJudges c = (Maybe (About c), [D.Judge c])


-- --------------------------------------------  Concrete type

-- | @About@ for concrete baala content.
type AboutC = About D.Content

-- | @AboutJudges@ for concrete baala content.
type AboutJudgesC = AboutJudges D.Content

-- | @Global@ for concrete baala content.
type GlobalC = C.Global D.Content

-- | @Resource@ for concrete baala content.
type ResourceC = C.Resource D.Content

-- | @Result@ for concrete baala content.
type ResultC = C.Result D.Content

-- | @ResultWriter@ for concrete baala content.
type ResultWriterC = C.ResultWriter D.Content

