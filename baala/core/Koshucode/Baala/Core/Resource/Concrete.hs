{-# OPTIONS_GHC -Wall #-}

-- | Concrete content type.

module Koshucode.Baala.Core.Resource.Concrete
  ( -- * About
    About (..), AboutJudges,

    -- * Concrete type
    AboutC, AboutJudgesC, GlobalC, JudgeC,
    RelC, ResourceC, ResultC, ResultWriterC,
  ) where

import qualified Koshucode.Baala.Syntax                  as S
import qualified Koshucode.Baala.Data                    as D
import qualified Koshucode.Baala.Core.Relmap             as C
import qualified Koshucode.Baala.Core.Resource.Resource  as C


-- --------------------------------------------  About

-- | @about@ clause.
data About c = About [S.Term c] deriving (Show)

type AboutJudges c = (Maybe (About c), [D.Judge c])


-- --------------------------------------------  Concrete type

-- | @About@ for concrete baala content.
type AboutC = About D.BaalaC

-- | @AboutJudges@ for concrete baala content.
type AboutJudgesC = AboutJudges D.BaalaC

-- | @Global@ for concrete baala content.
type GlobalC = C.Global D.BaalaC

-- | @Judge@ for concrete baala content.
type JudgeC = D.Judge D.BaalaC

-- | @Rel@ for concrete baala content.
type RelC = D.Rel D.BaalaC

-- | @Resource@ for concrete baala content.
type ResourceC = C.Resource D.BaalaC

-- | @Result@ for concrete baala content.
type ResultC = C.Result D.BaalaC

-- | @ResultWriter@ for concrete baala content.
type ResultWriterC = C.ResultWriter D.BaalaC

