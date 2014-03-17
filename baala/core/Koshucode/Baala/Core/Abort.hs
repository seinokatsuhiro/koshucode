{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Core.Abort
( unexpOperand,
) where

import qualified Koshucode.Baala.Base as B


-- ----------------------  Function

ab :: AbortCore -> B.Ab a
ab = Left . B.abortBy

-- | Unexpected term names
unexpOperand :: String -> B.Ab a
unexpOperand = ab . OpUnexpOperand

-- ----------------------  Datatype

data AbortCore
    = OpUnexpOperand String
      deriving (Show, Eq, Ord)

instance B.AbortBy AbortCore where
    abortBy a = B.AbortReason
                { B.abortSymbol = B.abortSymbolGet a
                , B.abortReason = r a
                , B.abortDetail = d a
                , B.abortSource = []
                } where

        r (OpUnexpOperand _)   = "Unexpected term names"

        d (OpUnexpOperand s)   = [s]

