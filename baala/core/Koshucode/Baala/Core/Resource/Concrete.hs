{-# OPTIONS_GHC -Wall #-}

-- | Concrete content type.

module Koshucode.Baala.Core.Resource.Concrete
  ( AboutC, AboutJudgesC, GlobalC, JudgeC,
    RelC, ResourceC, ResultC, ResultWriterC,
  ) where

import qualified Koshucode.Baala.Data                    as D
import qualified Koshucode.Baala.Core.Relmap             as C
import qualified Koshucode.Baala.Core.Resource.About     as C
import qualified Koshucode.Baala.Core.Resource.Resource  as C

-- | @About@ for concrete baala content.
type AboutC = C.About D.BaalaC

-- | @AboutJudges@ for concrete baala content.
type AboutJudgesC = C.AboutJudges D.BaalaC

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

