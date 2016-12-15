{-# OPTIONS_GHC -Wall #-}

-- | Message list.

module Koshucode.Baala.Data.Class.Message
  ( -- * Message
    unmatchType,
    badArg,
    unexpArg,
    notDec,
  ) where

import qualified Koshucode.Baala.Base                as B
import qualified Koshucode.Baala.Type.Message        as Msg

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

-- | Unexpected argument
unexpArg :: (B.MixEncode c) => [B.Ab c] -> [String] -> B.Ab a
unexpArg cs' expected =
    case sequence cs' of
      Left a   -> Left a
      Right cs -> Left $ B.abortLines "Unexpected argument"
                         (["Expect"] ++ indent expected ++
                          ["Actual"] ++ indent (Msg.encodableLines cs))
    where indent = map ("  " ++)

-- | Not a decimal.
notDec :: (B.MixEncode c) => c -> B.Ab a
notDec c = Left $ B.abortLine "Not a decimal" (B.encode c)

