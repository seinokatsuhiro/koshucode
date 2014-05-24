{-# OPTIONS_GHC -Wall #-}

-- | Abortable process

module Koshucode.Baala.Base.Abort.Abortable
( abortable,
) where

import qualified Koshucode.Baala.Base.Prelude        as B
import qualified Koshucode.Baala.Base.Text           as B
import qualified Koshucode.Baala.Base.Abort.Reason   as B


-- | Push source information when process is aborted.
abortable :: (B.CodePointer p) => String -> [p] -> B.Map (B.Ab b)
abortable tag ps = abortablePoints tag $ concatMap B.codePoint ps

abortablePoints :: String -> [B.CodePoint] -> B.Map (B.Ab b)
abortablePoints tag ps = either (Left . push tag ps) Right

push :: String -> [B.CodePoint] -> B.Map B.AbortReason
push tag ps abort@B.AbortReason { B.abortPoint = src } =
     case ps of
       []      -> abort
       [p]     -> abort { B.abortPoint = (tag, p) : src }
       (p : _) -> push tag [p] abort

