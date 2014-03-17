{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Abort
( noTerm,
  reqBool,
  reqNewTerm,
  unexpOperand,
  unexpTermName,
) where

import qualified Koshucode.Baala.Base as B


-- ----------------------  Function

ab :: AbortOp -> B.Ab a
ab = Left . B.abortBy

-- | Input relation does not given terms
noTerm :: [String] -> B.Ab a
noTerm = ab . OpNoTerm

-- | Require Boolean
reqBool :: B.Ab a
reqBool = ab OpReqBool

-- | Require new term names
reqNewTerm :: [String] -> B.Ab a
reqNewTerm = ab . OpReqNewTerm

-- | Unexpected term names
unexpOperand :: String -> B.Ab a
unexpOperand = ab . OpUnexpOperand

-- | Unexpected operand
unexpTermName :: B.Ab a
unexpTermName = ab OpUnexpTermName


-- ----------------------  Datatype

data AbortOp
    = OpNoTerm [String]
    | OpReqBool
    | OpReqNewTerm [String]
    | OpUnexpOperand String
    | OpUnexpTermName
      deriving (Show, Eq, Ord)

instance B.AbortBy AbortOp where
    abortBy a = B.AbortReason
                { B.abortSymbol = B.abortSymbolGet a
                , B.abortReason = r a
                , B.abortDetail = d a
                , B.abortSource = []
                } where

        r (OpNoTerm _)         = "Input relation does not given terms"
        r (OpReqBool)          = "Require Boolean"
        r (OpReqNewTerm _)     = "Require new term names"
        r (OpUnexpOperand _)   = "Unexpected term names"
        r (OpUnexpTermName)    = "Unexpected operand"

        d (OpNoTerm ns)        = [unwords ns]
        d (OpReqNewTerm ns)    = [unwords ns]
        d (OpUnexpOperand s)   = [s]
        d _                    = []

