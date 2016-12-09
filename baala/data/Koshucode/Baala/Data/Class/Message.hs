{-# OPTIONS_GHC -Wall #-}

-- | Message list.

module Koshucode.Baala.Data.Class.Message
  ( -- * Message
    unmatchType,
    badArg,
  ) where

import qualified Koshucode.Baala.Overture        as O
import qualified Koshucode.Baala.Base            as B

-- | Type unmatch
unmatchType :: String -> B.Ab a
unmatchType = Left . B.abortLine "Type unmatch"

-- | Bad argument
badArg :: (B.MixTransEncode c) => [B.Ab c] -> B.Ab a
badArg cs' = case sequence cs' of
               Left a   -> Left a
               Right cs -> badArgInternal cs
          
badArgInternal :: (B.MixTransEncode c) => [c] -> B.Ab a
badArgInternal cs = Left $ B.abortLines "Bad argument" (zipWith f (O.ints 1) cs) where
    f i c = "#" ++ show i ++ " = " ++ B.plainEncode c
          
