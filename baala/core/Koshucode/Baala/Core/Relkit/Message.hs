{-# OPTIONS_GHC -Wall #-}

-- | Message list.

module Koshucode.Baala.Core.Relkit.Message
  ( -- * Abortable
    abRun,
    -- * Message
    unkLocalRel,
  ) where

import qualified Koshucode.Baala.Base      as B
import qualified Koshucode.Baala.Syntax    as S

-- | Abortable scope for running relkit.
abRun :: (B.GetCodePos cp) => B.Abortable cp b
abRun = B.abortable "run"

-- | Unknown local relation
unkLocalRel :: S.Token -> String -> [String] -> B.Ab a
unkLocalRel p n rs = B.leftLines "Unknown local relation" $ ref : rs
    where ref = n ++ " in " ++ S.tokenContent p

