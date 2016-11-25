{-# OPTIONS_GHC -Wall #-}

-- | Abortable process.

module Koshucode.Baala.Base.Abort.Abortable
  ( abortable,
  ) where

import qualified Koshucode.Baala.Overture            as O
import qualified Koshucode.Baala.Base.IO             as B
import qualified Koshucode.Baala.Base.Abort.Reason   as B

-- | Push source information when process is aborted.
abortable :: (B.CodePtr cp) => B.AbortTag -> cp -> B.MapAb b
abortable tag cp = either (Left . push tag (B.codePtList cp)) Right

push :: String -> [B.CodePt] -> O.Map B.AbortReason
push _ [] a = a
push tag (p:_) a@B.AbortReason { B.abortPoint = ps } =
    a { B.abortPoint = (tag, p) : ps }
