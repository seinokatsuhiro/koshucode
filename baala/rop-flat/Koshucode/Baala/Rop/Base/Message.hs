{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Rop.Base.Message
  ( module Koshucode.Baala.Core.Message,
    appendAttr,
    noAttr,
    notImpl,
  ) where

import qualified Koshucode.Baala.Base as B
import Koshucode.Baala.Core.Message

-- | append requires two relmaps
appendAttr :: B.Ab a
appendAttr = Left $ B.abortBecause "'append' requires two relmaps"

-- | Attribute not found
noAttr :: String -> B.Ab a
noAttr n = Left $ B.abortLine "Attribute not found" n

-- | Not implemented
notImpl :: B.Ab a
notImpl = Left $ B.abortBecause "Not implemented"

