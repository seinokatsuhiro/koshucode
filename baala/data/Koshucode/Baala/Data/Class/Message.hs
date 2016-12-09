{-# OPTIONS_GHC -Wall #-}

-- | Message list.

module Koshucode.Baala.Data.Class.Message
  ( -- * Message
    unmatchType,
    badArg,
  ) where

import qualified Koshucode.Baala.Base            as B
import qualified Koshucode.Baala.Data.Type.Message   as Msg

-- | Type unmatch
unmatchType :: String -> B.Ab a
unmatchType = Left . B.abortLine "Type unmatch"

-- | Bad argument
badArg :: (B.MixEncode c) => [B.Ab c] -> B.Ab a
badArg cs' = case sequence cs' of
               Left a   -> Left a
               Right cs -> badArgInternal cs

badArgInternal :: (B.MixEncode c) => [c] -> B.Ab a
badArgInternal cs = Left $ Msg.abortEncodables "Bad argument" cs
          
