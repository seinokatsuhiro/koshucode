{-# OPTIONS_GHC -Wall #-}

-- | Message list.

module Koshucode.Baala.Data.Class.Message
  ( -- * Message
    unmatchType,
    typeUnmatched,
    badArg,
    unexpArg,
    notDec,
  ) where

import qualified Koshucode.Baala.Base                as B
import qualified Koshucode.Baala.Type.Message        as Msg

-- | [Type unmatch]
unmatchType :: String -> B.Ab a
unmatchType = B.leftLine "Type unmatch"

-- | [Type unmatch] Content = /C/
typeUnmatched :: (B.MixEncode c) => c -> B.Ab a
typeUnmatched = leftContent "Content type is unmatched"

-- | [Bad argument] \#1 = /C/, \#2 = /C/, ...
badArg :: (B.MixEncode c) => [B.Ab c] -> B.Ab a
badArg cs' = case sequence cs' of
               Left a   -> Left a
               Right cs -> badArgInternal cs

badArgInternal :: (B.MixEncode c) => [c] -> B.Ab a
badArgInternal cs = Left $ Msg.abortEncodables "Bad argument" cs

-- | [Unexpected argument] Expect /E/, Actual /A/
unexpArg :: (B.MixEncode c) => [B.Ab c] -> [String] -> B.Ab a
unexpArg cs' expected =
    case sequence cs' of
      Left a   -> Left a
      Right cs -> B.leftLines "Unexpected argument"
                         (["Expect"] ++ indent expected ++
                          ["Actual"] ++ indent (Msg.encodableLines cs))
    where indent = map ("  " ++)

-- | [Not a decimal] Content = /C/
notDec :: (B.MixEncode c) => c -> B.Ab a
notDec = leftContent "Not a decimal"

leftContent :: (B.MixEncode c) => String -> c -> B.Ab a
leftContent reason c = B.leftLine reason $ "Content = " ++ B.encode c
