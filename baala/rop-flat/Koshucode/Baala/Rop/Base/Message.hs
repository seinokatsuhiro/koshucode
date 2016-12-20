{-# OPTIONS_GHC -Wall #-}

-- | Message list.

module Koshucode.Baala.Rop.Base.Message
  ( module Koshucode.Baala.Core.Message,
    reqRelmap,
    noAttr,
    notImpl,
  ) where

import qualified Koshucode.Baala.Base as B
import Koshucode.Baala.Core.Message

-- | Require /N/ relmaps
reqRelmap :: Int -> B.Ab a
reqRelmap 0 = B.leftBecause "Require no relmaps"
reqRelmap 1 = B.leftBecause "Require one relmap"
reqRelmap n = B.leftBecause $ "Require " ++ show n ++ " relmap"

-- | Attribute not found
noAttr :: String -> B.Ab a
noAttr n = Left $ B.abortLine "Attribute not found" n

-- | Not implemented
notImpl :: B.Ab a
notImpl = B.leftBecause "Not implemented"

