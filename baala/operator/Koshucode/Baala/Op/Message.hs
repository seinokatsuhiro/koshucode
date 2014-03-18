{-# OPTIONS_GHC -Wall #-}

module Koshucode.Baala.Op.Message
( -- * Core package
  module Koshucode.Baala.Core.Message,

  -- * Op package
  noOperand,
  reqBool,
  reqNewTerm,
  unexpTermName,
) where

import qualified Koshucode.Baala.Base as B
import Koshucode.Baala.Core.Message


-- ----------------------  Function

ab :: AbortOp -> B.Ab a
ab = Left . B.abortBy

-- | Operand not found
noOperand :: B.Ab a
noOperand = Left $ B.abortBecause "Operand not found"

-- | Require Boolean
reqBool :: B.Ab a
reqBool = ab OpReqBool

-- | Require new term names
reqNewTerm :: [String] -> B.Ab a
reqNewTerm = ab . OpReqNewTerm

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
                { B.abortReason = r a
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

