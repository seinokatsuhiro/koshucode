{-# OPTIONS_GHC -Wall #-}

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
reqRelmap 0 = Left $ B.abortBecause "Require no relmaps"
reqRelmap 1 = Left $ B.abortBecause "Require one relmap"
reqRelmap n = Left $ B.abortBecause $ "Require " ++ show n ++ " relmap"

-- | Attribute not found
noAttr :: String -> B.Ab a
noAttr n = Left $ B.abortLine "Attribute not found" n

-- | Not implemented
notImpl :: B.Ab a
notImpl = Left $ B.abortBecause "Not implemented"

