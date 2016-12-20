{-# OPTIONS_GHC -Wall #-}

-- | Message list.

module Koshucode.Baala.Base.Abort.Message
  ( adlib,
    bug,
    unsupported,
  ) where

import qualified Koshucode.Baala.Base.Abort    as B

-- | BUG: /reason/
bug :: String -> B.Ab a
bug reason = B.leftBecause $ "BUG: " ++ reason

-- | /reason/
adlib :: String -> B.Ab a
adlib reason = B.leftBecause reason

-- | Unsupported feature.
unsupported :: String -> B.Ab a
unsupported = Left . B.abortLine "Unsupported feature"

