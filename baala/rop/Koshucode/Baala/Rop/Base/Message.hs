{-# OPTIONS_GHC -Wall #-}

-- | Message list.

module Koshucode.Baala.Rop.Base.Message
  ( module Koshucode.Baala.Core.Message,
    reqRelmap,
    noAttr,
    notImpl,
  ) where

import qualified Koshucode.Baala.DataPlus as K
import Koshucode.Baala.Core.Message

-- | Require /N/ relmaps
reqRelmap :: Int -> K.Ab a
reqRelmap 0 = K.leftBecause "Require no relmaps"
reqRelmap 1 = K.leftBecause "Require one relmap"
reqRelmap n = K.leftBecause $ "Require " ++ show n ++ " relmap"

-- | Attribute not found
noAttr :: String -> K.Ab a
noAttr n = K.leftLine "Attribute not found" n

-- | Not implemented
notImpl :: K.Ab a
notImpl = K.leftBecause "Not implemented"

