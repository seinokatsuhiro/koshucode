{-# OPTIONS_GHC -Wall #-}

-- | Abortable process

module Koshucode.Baala.Base.Abort.Abortable
  ( abortable,
    abortableSourced,
  ) where

import qualified Koshucode.Baala.Base.Prelude        as B
import qualified Koshucode.Baala.Base.Text           as B
import qualified Koshucode.Baala.Base.Abort.Reason   as B


-- | Push source information when process is aborted.
abortable :: (B.CodePtr p) => String -> [p] -> B.Map (B.Ab b)
abortable tag ps = abortablePoints tag $ concatMap B.codePts ps

abortablePoints :: String -> [B.CodePt] -> B.Map (B.Ab b)
abortablePoints tag ps = either (Left . push tag ps) Right

push :: String -> [B.CodePt] -> B.Map B.AbortReason
push tag ps abort@B.AbortReason { B.abortPoint = src } =
     case ps of
       []      -> abort
       [p]     -> abort { B.abortPoint = (tag, p) : src }
       (p : _) -> push tag [p] abort

abortableSourced :: String -> (a -> B.Ab b) -> B.Sourced a -> B.Ab (B.Sourced b)
abortableSourced tag f (B.Sourced pt x) =
    abortable tag pt $ do
      y <- f x
      Right $ B.Sourced pt y

