{-# OPTIONS_GHC -Wall #-}

-- | Class for operand patterns

module Koshucode.Baala.Minimal.Relmap.Pattern
( OpPattern (..)
, operators
) where

import Koshucode.Baala.Minimal.OpKit as Kit

-- | Class for operand pattern.
class OpPattern p where
    operandParser :: p -> OpParser
    operandParser p = operandParser' p . operandGroup

    operandParser' :: p -> OpParser'
    operandParser' _ = id

    operandUsage :: p -> [String]
    operandUsage _ = []

-- | Make relmap implementations.
operators
    :: (OpPattern p)
    => [(String, p, OpCons v)] -- ^ Operator implementations
    -> [OpImplement v] -- ^ Result
operators = map f where
    f (op, p, full) =
        let parser  = operandParser p
            addOp u = op ++ " " ++ u
        in OpImplement op parser full (map addOp $ operandUsage p)

