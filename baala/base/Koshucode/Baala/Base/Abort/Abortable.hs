{-# OPTIONS_GHC -Wall #-}

-- | Abortable process.

module Koshucode.Baala.Base.Abort.Abortable
  ( abortable,
  ) where

import qualified Koshucode.Baala.Overture            as O
import qualified Koshucode.Baala.Base.IO             as B
import qualified Koshucode.Baala.Base.Abort.Reason   as B

-- | Push source information when process is aborted.
abortable :: (B.CodePtr cp) => B.AbortTag -> [cp] -> B.MapAb b
abortable tag ps = abortablePoints tag $ concatMap B.codePtList ps

abortablePoints :: String -> [B.CodePt] -> B.MapAb b
abortablePoints tag ps = either (Left . push tag ps) Right

push :: String -> [B.CodePt] -> O.Map B.AbortReason
push tag ps abort@B.AbortReason { B.abortPoint = src } =
     case ps of
       []      -> abort
       [p]     -> abort { B.abortPoint = (tag, p) : src }
       (p : _) -> push tag [p] abort

