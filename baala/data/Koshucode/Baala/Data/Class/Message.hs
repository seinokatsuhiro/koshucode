{-# OPTIONS_GHC -Wall #-}

-- | Message list.

module Koshucode.Baala.Data.Class.Message
  ( -- * Message
    unmatchType,
  ) where

import qualified Koshucode.Baala.Base            as B

-- | Type unmatch
unmatchType :: String -> B.Ab a
unmatchType = Left . B.abortLine "Type unmatch"

