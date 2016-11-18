{-# OPTIONS_GHC -Wall #-}

-- | Message list.

module Koshucode.Baala.Core.Relkit.Message
  ( -- * Abortable
    abRun,
    -- * Message
    unkLocalRel,
  ) where

import qualified Koshucode.Baala.Overture  as O
import qualified Koshucode.Baala.Base      as B
import qualified Koshucode.Baala.Syntax    as S

-- | Abortable scope for running relkit.
abRun :: (B.CodePtr cp) => [cp] -> O.Map (B.Ab b)
abRun = B.abortable "run"

-- | Unknown local relation
unkLocalRel :: S.Token -> String -> [String] -> B.Ab a
unkLocalRel p n rs = Left $ B.abortLines "Unknown local relation" $ ref : rs
    where ref = n ++ " in " ++ S.tokenContent p

