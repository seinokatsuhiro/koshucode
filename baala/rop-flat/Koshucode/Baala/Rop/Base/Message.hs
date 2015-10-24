{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Rop.Base.Message
  ( module Koshucode.Baala.Core.Message,
    noAttr,
    notImpl,
  ) where

import qualified Koshucode.Baala.Base as B
import Koshucode.Baala.Core.Message


-- | Attribute not found
noAttr :: String -> B.Ab a
noAttr n = Left $ B.abortLine "Attribute not found" n

-- | Not implemented
notImpl :: B.Ab a
notImpl = Left $ B.abortBecause "Not implemented"

