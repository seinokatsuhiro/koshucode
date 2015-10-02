{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Relkit.Message
  ( -- * Abortable
    abRun,
    -- * Message
    unkNestRel,
  ) where

import qualified Koshucode.Baala.Base as B
import qualified Koshucode.Baala.Data as D

abRun :: (B.CodePtr cp) => [cp] -> B.Map (B.Ab b)
abRun = B.abortable "run"

-- | Unknown nested relation
unkNestRel :: D.Token -> String -> [String] -> B.Ab a
unkNestRel p n rs = Left $ B.abortLines "Unknown nested relation" $ ref : rs
    where ref = "/" ++ n ++ " in " ++ D.tokenContent p

